mod symbol_table;

#[cfg(test)]
mod tests;

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, ClosureParams, EnumVariantType, Expression, Float,
        FunctionItem, FunctionOrMethodParam, FunctionParam, FunctionPtr, Hash, Identifier,
        ImportDecl, InferredType, InherentImplItem, Int, Item, Keyword, Literal, LiteralPatt,
        ModuleItem, PathExpr, PathRoot, Pattern, SelfType, Statement, Str, StructDef, TraitDefItem,
        TraitImplItem, TupleStructDef, TupleStructDefField, Type, TypePath, UInt, UnaryOp,
        UnitType, Visibility,
    },
    error::{CompilerError, SemanticErrorKind},
    logger::{LogLevel, Logger},
    parser::{ty::build_item_path, Program},
    span::{Span, Spanned},
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use core::fmt;
use std::collections::HashMap;

use symbol_table::{Scope, ScopeKind, Symbol, SymbolTable};

#[allow(dead_code)]
struct SemanticAnalyser {
    scope_stack: Vec<Scope>,
    module_registry: HashMap<TypePath, SymbolTable>,
    errors: Vec<CompilerError<SemanticErrorKind>>,
    logger: Logger,
}

#[allow(dead_code)]
impl SemanticAnalyser {
    pub(crate) fn new(log_level: LogLevel, external_code: Option<SymbolTable>) -> Self {
        let mut logger = Logger::new(log_level);
        let mut external_symbols: SymbolTable = HashMap::new();
        let mut module_registry: HashMap<TypePath, SymbolTable> = HashMap::new();

        // add external code (e.g., library functions) to the global scope if there is any
        if let Some(ext) = external_code {
            logger.debug(&format!("importing external code …"));

            for (path, ref sym) in ext {
                if let Symbol::Module { path, symbols, .. } = sym.clone() {
                    module_registry.insert(path, symbols);
                }

                external_symbols.insert(path, sym.clone());
            }
        }

        SemanticAnalyser {
            scope_stack: vec![
                Scope {
                    scope_kind: ScopeKind::Public,
                    symbols: external_symbols,
                },
                Scope {
                    scope_kind: ScopeKind::Lib,
                    symbols: HashMap::new(),
                },
            ],
            module_registry,
            errors: Vec::new(),
            logger,
        }
    }

    fn enter_scope(&mut self, scope_kind: ScopeKind) {
        self.logger
            .info(&format!("entering new scope: `{:?}` …", scope_kind));

        self.scope_stack.push(Scope {
            scope_kind,
            symbols: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) -> Option<Scope> {
        if let Some(exited_scope) = self.scope_stack.pop() {
            self.logger
                .info(&format!("exited scope: `{:?}`", exited_scope.scope_kind));

            Some(exited_scope)
        } else {
            None
        }
    }

    fn insert(&mut self, path: TypePath, symbol: Symbol) -> Result<(), SemanticErrorKind> {
        if let Some(curr_scope) = self.scope_stack.last_mut() {
            self.logger.info(&format!(
                "inserting symbol `{symbol}` into scope `{:?}` at path `{path}` …",
                curr_scope.scope_kind
            ));

            curr_scope.symbols.insert(path, symbol);

            Ok(())
        } else {
            Err(SemanticErrorKind::UndefinedScope)
        }
    }

    fn lookup(&mut self, path: &TypePath) -> Option<&Symbol> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.symbols.get(path) {
                self.logger.debug(&format!(
                    "found symbol `{symbol}` in scope `{:?}` at path `{path}`",
                    scope.scope_kind
                ));

                return Some(symbol);
            }
        }
        self.logger
            .warn(&format!("path `{path}` not found in any scope"));

        None
    }

    fn analyse_program(
        &mut self,
        program: &Program,
        path: TypePath,
    ) -> Result<(), Vec<CompilerError<SemanticErrorKind>>> {
        self.logger.info(&format!("starting semantic analysis …"));

        let program_path = if let Some(Scope {
            scope_kind: ScopeKind::Lib,
            ..
        }) = self.scope_stack.last()
        {
            let mut prefix = vec![Identifier::from("lib")];

            if let Some(mut segments) = path.associated_type_path_prefix_opt {
                if segments.get(0) == Some(&Identifier::from("lib")) {
                    segments.remove(0);
                }

                prefix.append(&mut segments);
            }

            if path.type_name != Identifier::from("") {
                prefix.push(path.type_name);
            }

            TypePath::from(prefix)
        } else {
            path
        };

        self.enter_scope(ScopeKind::RootModule(program_path.to_string()));

        let mut module_items: Vec<Item> = Vec::new();

        program.statements.clone().into_iter().for_each(|stmt| {
            match stmt {
                Statement::Item(i) => module_items.push(i),
                _ => (),
            };
        });

        let start_span = if let Some(stmt) = program.statements.first() {
            stmt.span()
        } else {
            Span::default()
        };

        let end_span = if let Some(stmt) = program.statements.last() {
            stmt.span()
        } else {
            Span::default()
        };

        let module_span = Span::new(&start_span.input(), start_span.start(), end_span.end());

        let root_module = ModuleItem {
            outer_attributes_opt: None,
            visibility: Visibility::Pub,
            kw_module: Keyword::Anonymous,
            module_name: program_path.type_name.clone(),
            inner_attributes_opt: None,
            items_opt: {
                if module_items.is_empty() {
                    None
                } else {
                    Some(module_items)
                }
            },
            span: module_span,
        };

        let mut module_contents: SymbolTable = HashMap::new();

        module_contents.insert(
            program_path.clone(),
            Symbol::Module {
                path: TypePath::from(root_module.module_name.clone()),
                module: root_module,
                symbols: HashMap::new(),
            },
        );

        self.module_registry
            .insert(program_path.clone(), module_contents);

        for stmt in &program.statements {
            self.analyse_stmt(stmt, &program_path)
                .map_err(|_| self.errors.clone())?;
        }

        self.logger
            .info("semantic analysis complete, no errors detected");

        Ok(())
    }

    fn analyse_stmt(
        &mut self,
        statement: &Statement,
        root: &TypePath,
    ) -> Result<(), SemanticErrorKind> {
        match statement {
            Statement::Let(ls) => {
                self.logger
                    .info(&format!("analysing let statement: `{}` …", statement));

                // variables declared must have a type and are assigned the unit type if not;
                // this prevents uninitialized variable errors
                let value_type = if let Some(val) = &ls.value_opt {
                    self.analyse_expr(val, root)?
                } else {
                    Type::UnitType(UnitType)
                };

                // get the type annotation if there is one, otherwise assume the value's type
                let declared_type = match &ls.type_ann_opt {
                    Some(ty) => ty,
                    _ => &value_type,
                };

                // check that the value matches the type annotation
                if &value_type != declared_type {
                    return Err(SemanticErrorKind::TypeMismatchDeclaredType {
                        actual_type: format!("`{}`", value_type),
                        declared_type: format!("`{}`", declared_type),
                    });
                }

                let assignee_path = TypePath::from(ls.assignee.name.clone());

                let symbol = match &value_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U256(_)
                    | Type::U512(_)
                    | Type::F32(_)
                    | Type::F64(_)
                    | Type::Byte(_)
                    | Type::B2(_)
                    | Type::B4(_)
                    | Type::B8(_)
                    | Type::B16(_)
                    | Type::B32(_)
                    | Type::H160(_)
                    | Type::H256(_)
                    | Type::H512(_)
                    | Type::Str(_)
                    | Type::Char(_)
                    | Type::Bool(_)
                    | Type::UnitType(_)
                    | Type::GroupedType(_)
                    | Type::Array { .. }
                    | Type::Tuple(_)
                    | Type::FunctionPtr(_)
                    | Type::Reference { .. }
                    | Type::SelfType(_)
                    | Type::InferredType(_)
                    | Type::Vec { .. }
                    | Type::Mapping { .. }
                    | Type::Option { .. }
                    | Type::Result { .. } => Symbol::Variable {
                        name: ls.assignee.name.clone(),
                        var_type: value_type.clone(),
                    },
                    Type::UserDefined(tp) => {
                        let type_path = self.check_path(tp, root, String::from("type"))?;

                        match self.lookup(&type_path) {
                            Some(sym) => sym.clone(),
                            _ => Symbol::Variable {
                                name: ls.assignee.name.clone(),
                                var_type: value_type,
                            },
                        }
                    }
                };

                // add the variable to the symbol table
                self.insert(assignee_path, symbol)?;
            }

            Statement::Item(item) => match item {
                Item::ImportDecl(id) => {
                    self.logger
                        .info(&format!("analysing import declaration: `{}` …", statement));

                    self.analyse_import(&id, root)?
                }

                Item::AliasDecl(ad) => {
                    self.logger
                        .info(&format!("analysing alias declaration: `{}` …", statement));

                    let alias_path = build_item_path(root, TypePath::from(ad.alias_name.clone()));

                    self.insert(
                        alias_path.clone(),
                        Symbol::Alias {
                            path: alias_path,
                            visibility: ad.visibility.clone(),
                            alias_name: ad.alias_name.clone(),
                            original_type_opt: ad.original_type_opt.clone(),
                        },
                    )?;
                }

                Item::ConstantDecl(cd) => {
                    self.logger.info(&format!(
                        "analysing constant declaration: `{}` …",
                        statement
                    ));

                    let value_type = match &cd.value_opt {
                        Some(val) => {
                            let value = wrap_into_expression(val.clone());
                            Some(self.analyse_expr(&value, root)?)
                        }
                        _ => None,
                    };

                    if value_type.clone().is_some_and(|t| t != *cd.constant_type) {
                        return Err(SemanticErrorKind::TypeMismatchDeclaredType {
                            declared_type: format!("`{}`", cd.constant_type),
                            actual_type: format!("`{}`", value_type.unwrap()),
                        });
                    }

                    let constant_path =
                        build_item_path(root, TypePath::from(cd.constant_name.clone()));

                    self.insert(
                        constant_path.clone(),
                        Symbol::Constant {
                            path: constant_path,
                            visibility: cd.visibility.clone(),
                            constant_name: cd.constant_name.clone(),
                            constant_type: value_type.unwrap_or(Type::InferredType(InferredType {
                                name: Identifier::from("_"),
                            })),
                        },
                    )?;
                }

                Item::StaticVarDecl(s) => {
                    self.logger.info(&format!(
                        "analysing static variable declaration: `{}` …",
                        statement
                    ));

                    let assignee_type = match &s.assignee_opt {
                        Some(a_expr) => {
                            let assignee = wrap_into_expression(*a_expr.clone());
                            self.analyse_expr(&assignee, root)?
                        }
                        _ => Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        }),
                    };

                    if assignee_type != s.var_type.clone() {
                        return Err(SemanticErrorKind::TypeMismatchDeclaredType {
                            declared_type: format!("`{}`", s.var_type),
                            actual_type: format!("`{}`", assignee_type),
                        });
                    }

                    let static_var_path = build_item_path(root, TypePath::from(s.var_name.clone()));

                    self.insert(
                        static_var_path,
                        Symbol::Variable {
                            name: s.var_name.clone(),
                            var_type: s.var_type.clone(),
                        },
                    )?;
                }

                Item::ModuleItem(m) => {
                    // TODO: sort out visibility
                    let module_path = build_item_path(root, TypePath::from(m.module_name.clone()));

                    let scope_kind = ScopeKind::Module(module_path.to_string());

                    let mut module_scope = Scope {
                        scope_kind: scope_kind.clone(),
                        symbols: HashMap::new(),
                    };

                    self.enter_scope(scope_kind);

                    if let Some(items) = &m.items_opt {
                        for item in items.iter() {
                            match self.analyse_stmt(&Statement::Item(item.clone()), &module_path) {
                                Ok(_) => (),
                                Err(e) => self.log_error(e, &item.span()),
                            }
                        }
                    }

                    if let Some(curr_scope) = self.scope_stack.pop() {
                        self.logger
                            .info(&format!("exiting scope: `{:?}`", curr_scope.scope_kind));

                        module_scope = curr_scope;
                    }

                    self.insert(
                        module_path.clone(),
                        Symbol::Module {
                            path: module_path.clone(),
                            module: m.clone(),
                            symbols: module_scope.symbols.clone(),
                        },
                    )?;

                    self.logger.info(&format!(
                        "inserting symbols into module at path: `{}`",
                        module_path,
                    ));

                    self.module_registry
                        .insert(module_path, module_scope.symbols);
                }

                Item::TraitDef(t) => {
                    let trait_name_path = TypePath::from(t.trait_name.clone());
                    let trait_def_path = build_item_path(root, trait_name_path.clone());

                    self.insert(
                        trait_def_path.clone(),
                        Symbol::Trait {
                            path: trait_name_path,
                            trait_def: t.clone(),
                        },
                    )?;

                    if let Some(items) = &t.trait_items_opt {
                        for i in items.iter() {
                            match i {
                                TraitDefItem::AliasDecl(ad) => {
                                    self.analyse_stmt(
                                        &Statement::Item(Item::AliasDecl(ad.clone())),
                                        &trait_def_path,
                                    )?;
                                }
                                TraitDefItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                    &trait_def_path,
                                )?,
                                TraitDefItem::FunctionItem(fi) => {
                                    let function_name_path =
                                        TypePath::from(fi.function_name.clone());

                                    let function_def_path = build_item_path(
                                        &trait_def_path,
                                        function_name_path.clone(),
                                    );

                                    self.insert(
                                        function_def_path,
                                        Symbol::Function {
                                            path: function_name_path,
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(fi, &trait_def_path, true, false)?;
                                }
                            }
                        }
                    }
                }

                Item::EnumDef(e) => {
                    self.logger
                        .info(&format!("analysing enum definition: `{}` …", statement));

                    let enum_name_path = TypePath::from(e.enum_name.clone());
                    let enum_def_path = build_item_path(root, enum_name_path.clone());

                    self.insert(
                        enum_def_path.clone(),
                        Symbol::Enum {
                            path: enum_name_path,
                            enum_def: e.clone(),
                        },
                    )?;

                    for variant in e.variants.clone() {
                        let variant_path = build_item_path(
                            &enum_def_path,
                            TypePath::from(variant.variant_name.clone()),
                        );

                        if let Some(variant_type) = variant.variant_type_opt {
                            match variant_type {
                                EnumVariantType::Struct(s) => {
                                    self.insert(
                                        variant_path.clone(),
                                        Symbol::Struct {
                                            path: variant_path,
                                            struct_def: StructDef {
                                                attributes_opt: None,
                                                visibility: Visibility::Private,
                                                kw_struct: Keyword::Anonymous,
                                                struct_name: variant.variant_name,
                                                fields_opt: Some(s.struct_fields),
                                                span: Span::default(),
                                            },
                                        },
                                    )?;
                                }
                                EnumVariantType::TupleStruct(t) => {
                                    self.insert(
                                        variant_path.clone(),
                                        Symbol::TupleStruct {
                                            path: variant_path.clone(),
                                            tuple_struct_def: TupleStructDef {
                                                attributes_opt: None,
                                                visibility: Visibility::Private,
                                                kw_struct: Keyword::Anonymous,
                                                struct_name: Identifier::from(
                                                    &variant_path.to_string(),
                                                ),
                                                fields_opt: {
                                                    let mut elements: Vec<TupleStructDefField> =
                                                        Vec::new();

                                                    for elem_type in t.element_types {
                                                        let elem = TupleStructDefField {
                                                            attributes_opt: None,
                                                            visibility: Visibility::Private,
                                                            field_type: Box::new(elem_type),
                                                        };

                                                        elements.push(elem);
                                                    }

                                                    Some(elements)
                                                },
                                                span: Span::default(),
                                            },
                                        },
                                    )?;
                                }
                            }
                        } else {
                            self.insert(
                                variant_path.clone(),
                                Symbol::Variable {
                                    name: Identifier::from(&variant_path.to_string()),
                                    var_type: Type::UserDefined(variant_path),
                                },
                            )?;
                        }
                    }
                }

                Item::StructDef(s) => {
                    self.logger
                        .info(&format!("analysing struct definition: `{}` …", statement));

                    let struct_name_path = TypePath::from(s.struct_name.clone());
                    let struct_def_path = build_item_path(root, struct_name_path.clone());

                    self.insert(
                        struct_def_path,
                        Symbol::Struct {
                            path: struct_name_path,
                            struct_def: s.clone(),
                        },
                    )?;
                }

                Item::TupleStructDef(ts) => {
                    self.logger.info(&format!(
                        "analysing tuple struct definition: `{}` …",
                        statement
                    ));

                    let struct_name_path = TypePath::from(ts.struct_name.clone());
                    let tuple_struct_path = build_item_path(root, struct_name_path.clone());

                    self.insert(
                        tuple_struct_path,
                        Symbol::TupleStruct {
                            path: struct_name_path,
                            tuple_struct_def: ts.clone(),
                        },
                    )?;
                }

                Item::InherentImplDef(i) => {
                    let type_path = build_item_path(root, i.nominal_type.clone());

                    if let Some(items) = &i.associated_items_opt {
                        for i in items.iter() {
                            match i {
                                InherentImplItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                    &type_path,
                                )?,
                                InherentImplItem::FunctionItem(fi) => {
                                    let function_name_path =
                                        TypePath::from(fi.function_name.clone());

                                    let function_def_path =
                                        build_item_path(&type_path, function_name_path.clone());

                                    self.insert(
                                        function_def_path.clone(),
                                        Symbol::Function {
                                            path: function_name_path,
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(fi, &type_path, true, false)?;
                                }
                            }
                        }
                    }
                }

                Item::TraitImplDef(t) => {
                    let trait_impl_path = match &t.implementing_type {
                        Type::UserDefined(tp) => build_item_path(
                            &build_item_path(root, tp.clone()),
                            TypePath::from(t.implemented_trait_path.type_name.clone()),
                        ),
                        ty => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "user-defined type".to_string(),
                                found: format!("`{}`", ty),
                            })
                        }
                    };

                    if let Some(items) = &t.associated_items_opt {
                        for i in items.iter() {
                            match i {
                                TraitImplItem::AliasDecl(ad) => self.analyse_stmt(
                                    &Statement::Item(Item::AliasDecl(ad.clone())),
                                    &trait_impl_path,
                                )?,
                                TraitImplItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                    &trait_impl_path,
                                )?,
                                TraitImplItem::FunctionItem(fi) => {
                                    let function_impl_path = build_item_path(
                                        &trait_impl_path,
                                        TypePath::from(fi.function_name.clone()),
                                    );

                                    self.insert(
                                        function_impl_path.clone(),
                                        Symbol::Function {
                                            path: function_impl_path,
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(fi, &trait_impl_path, true, true)?;
                                }
                            }
                        }
                    }
                }

                Item::FunctionItem(f) => {
                    self.logger
                        .info(&format!("analysing function item: `{}` …", statement));

                    let function_name_path = TypePath::from(f.function_name.clone());
                    let function_item_path = build_item_path(root, function_name_path.clone());

                    self.insert(
                        function_item_path,
                        Symbol::Function {
                            path: function_name_path,
                            function: f.clone(),
                        },
                    )?;

                    self.analyse_function_def(f, root, false, false)?;
                }
            },

            Statement::Expression(expr) => {
                self.logger.info(&format!(
                    "analysing expression statement: `{}` …",
                    statement
                ));

                self.analyse_expr(expr, root)?;
            }
        }

        Ok(())
    }

    fn analyse_function_def(
        &mut self,
        f: &FunctionItem,
        path: &TypePath,
        is_associated_func: bool,
        is_trait_impl: bool,
    ) -> Result<(), SemanticErrorKind> {
        // What is the root path? [i.e., path to the function, excluding the function name]
        // Options:
        // * module (e.g., `lib::some_module` or just `lib`)
        // * object implementation (e.g., `lib::some_module::SomeObject`)
        // * trait implementation (e.g., `lib::some_module::SomeObject::SomeTrait`)

        println!("enter `analyse_function_def()`");

        let function_root = if is_trait_impl {
            if let Some(prefix) = &path.associated_type_path_prefix_opt {
                TypePath::from(prefix.clone())
            } else {
                path.clone()
            }
        } else {
            path.clone()
        };

        // append the function name to the root
        let full_path = build_item_path(&function_root, TypePath::from(f.function_name.clone()));

        println!("function path: `{full_path}`");

        self.enter_scope(ScopeKind::Function(full_path.to_string()));

        if let Some(params) = &f.params_opt {
            let param_types: Vec<Type> = params.iter().map(|p| p.param_type()).collect();

            for (param, mut param_type) in params.iter().zip(param_types) {
                if param_type == Type::SelfType(SelfType) {
                    if is_associated_func {
                        param_type = Type::UserDefined(function_root.clone());
                    }
                }

                let param_path = TypePath::from(param.param_name());

                println!("param path: `{param_path}`, param_type: {param_type:?}");

                let symbol = match param_type {
                    Type::FunctionPtr(fp) => Symbol::Function {
                        path: param_path.clone(),
                        function: FunctionItem {
                            attributes_opt: None,
                            visibility: Visibility::Private,
                            kw_func: Keyword::Anonymous,
                            function_name: f.function_name.clone(),
                            params_opt: fp.params_opt,
                            return_type_opt: fp.return_type_opt,
                            block_opt: None,
                            span: f.span.clone(),
                        },
                    },

                    Type::UserDefined(tp) => {
                        let type_path =
                            self.check_path(&tp, &path, "user-defined type".to_string())?;

                        match self.lookup(&type_path) {
                            Some(sym) => sym.clone(),
                            None => {
                                return Err(SemanticErrorKind::UndefinedType { name: tp.type_name })
                            }
                        }
                    }

                    ty => Symbol::Variable {
                        name: param.param_name(),
                        var_type: ty,
                    },
                };

                println!("parameter symbol: {symbol:?}");

                self.insert(param_path, symbol)?;
            }
        }

        let function_type = if let Some(block) = &f.block_opt {
            // split path into a vector to remove the last element (if needed), then convert back
            let mut path_vec = Vec::<Identifier>::from(path.clone());

            if is_associated_func {
                path_vec.pop(); // remove associated type name
            }

            self.analyse_expr(&Expression::Block(block.clone()), &TypePath::from(path_vec))?
        } else {
            if let Some(ty) = &f.return_type_opt {
                *ty.clone()
            } else {
                Type::UnitType(UnitType)
            }
        };

        println!("function type: {function_type}");

        println!("return type: {:?}", f.return_type_opt);

        // check that the function type matches the return type
        if let Some(return_type) = &f.return_type_opt {
            if function_type != *return_type.clone() {
                // if the `Ok` or `Err` type cannot be determined during analysis, do nothing
                if let Type::Result { ok_type, err_type } = function_type.clone() {
                    if *ok_type
                        == Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        })
                        || *err_type
                            == Type::InferredType(InferredType {
                                name: Identifier::from("_"),
                            })
                    {
                        ()
                    } else {
                        return Err(SemanticErrorKind::TypeMismatchReturnType {
                            expected: format!("`{}`", return_type),
                            found: format!("`{}`", function_type),
                        });
                    }
                }
            }
        }

        self.exit_scope();

        Ok(())
    }

    fn analyse_import(
        &mut self,
        import_decl: &ImportDecl,
        module_root: &TypePath,
    ) -> Result<(), SemanticErrorKind> {
        let mut import_paths: Vec<TypePath> = Vec::new();

        let mut segments = import_decl.import_tree.path_segments.clone();

        let mut import_root = if !segments.is_empty() {
            let mut paths = Vec::<TypePath>::from(segments.remove(0));

            if !paths.is_empty() {
                let mut path = paths.remove(0);

                for p in paths {
                    path = build_item_path(&path, p);
                }

                path
            } else {
                TypePath::from(Identifier::from(""))
            }
        } else {
            module_root.clone()
        };

        let num_segments = segments.len();

        for (i, seg) in segments.into_iter().enumerate() {
            let path = build_item_path(&import_root, seg.root);

            if let Some(subset) = seg.subset_opt {
                import_root = path.clone();

                for tree in subset.nested_trees {
                    for tree_seg in tree.path_segments {
                        for seg_path in Vec::<TypePath>::from(tree_seg) {
                            let path = build_item_path(&path.clone(), seg_path);
                            import_paths.push(path);
                        }
                    }
                }
            } else {
                if i == num_segments - 1 {
                    import_paths.push(path.clone());
                }

                if i < num_segments - 1 {
                    import_root = path.clone();
                }
            }
        }

        // TODO: handle `super` and `self` path roots
        // TODO: handle public imports / re-exports

        for path in import_paths {
            if let Some(module) = self.module_registry.get(&import_root).cloned() {
                for (item_path, symbol) in module.clone() {
                    let short_path = build_item_path(
                        &TypePath::from(path.type_name.clone()),
                        TypePath::from(item_path.type_name),
                    );
                    self.insert(short_path, symbol)?;
                }

                if let Some(sym) = module.get(&path) {
                    self.insert(TypePath::from(path.type_name), sym.clone())?;
                } else {
                    return Err(SemanticErrorKind::UndefinedSymbol {
                        name: path.to_string(),
                    });
                }
            } else if let Some(sym) = self.lookup(&path).cloned() {
                self.insert(path, sym)?;
            } else {
                return Err(SemanticErrorKind::UndefinedModule {
                    name: import_root.type_name,
                });
            }
        }

        Ok(())
    }

    fn analyse_expr(
        &mut self,
        expression: &Expression,
        root: &TypePath,
    ) -> Result<Type, SemanticErrorKind> {
        match expression {
            Expression::Path(p) => {
                let path = match p.tree_opt.clone() {
                    Some(mut segments) => {
                        if let Some(id) = segments.pop() {
                            let root = match &p.path_root {
                                PathRoot::Identifier(id) => TypePath::from(id.clone()),
                                PathRoot::SelfKeyword => TypePath::from(Identifier::from("self")),
                                PathRoot::SelfType(_) => TypePath::from(Identifier::from("Self")),

                                path_root => {
                                    return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                        name: Identifier::from(&path_root.to_string()),
                                    })
                                }
                            };

                            build_item_path(
                                &root,
                                TypePath {
                                    associated_type_path_prefix_opt: Some(segments.to_vec()),
                                    type_name: id,
                                },
                            )
                        } else {
                            match &p.path_root {
                                PathRoot::Identifier(i) => {
                                    build_item_path(root, TypePath::from(i.clone()))
                                }
                                PathRoot::SelfType(_) => root.clone(),
                                PathRoot::SelfKeyword => root.clone(),

                                path_root => {
                                    return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                        name: Identifier::from(&path_root.to_string()),
                                    })
                                }
                            }
                        }
                    }

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => TypePath::from(i.clone()),
                        PathRoot::SelfType(_) => root.clone(),
                        PathRoot::SelfKeyword => root.clone(),

                        path_root => {
                            return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                name: Identifier::from(&path_root.to_string()),
                            })
                        }
                    },
                };

                let variable_path = self.check_path(&path, root, String::from("variable"))?;

                println!("variable path: {variable_path}");

                if let Some(sym) = self.lookup(&variable_path) {
                    println!("variable symbol: {sym}");

                    Ok(sym.symbol_type())
                } else {
                    Err(SemanticErrorKind::UndefinedVariable {
                        name: path.type_name,
                    })
                }
            }

            Expression::Literal(l) => match l {
                Literal::Int { value, .. } => match value {
                    Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                    Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
                },
                Literal::UInt { value, .. } => match value {
                    UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                    UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                    UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                    UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
                },
                Literal::BigUInt { value, .. } => match value {
                    BigUInt::U256(_) => Ok(Type::U256(BigUInt::U256(U256::default()))),
                    BigUInt::U512(_) => Ok(Type::U512(BigUInt::U512(U512::default()))),
                },
                Literal::Float { value, .. } => match value {
                    Float::F32(_) => Ok(Type::F32(Float::F32(F32::default()))),
                    Float::F64(_) => Ok(Type::F64(Float::F64(F64::default()))),
                },
                Literal::Byte { .. } => Ok(Type::Byte(Byte::from(u8::default()))),
                Literal::Bytes { value, .. } => match value {
                    Bytes::B2(_) => Ok(Type::B2(Bytes::B2(B2::default()))),
                    Bytes::B4(_) => Ok(Type::B4(Bytes::B4(B4::default()))),
                    Bytes::B8(_) => Ok(Type::B8(Bytes::B8(B8::default()))),
                    Bytes::B16(_) => Ok(Type::B16(Bytes::B16(B16::default()))),
                    Bytes::B32(_) => Ok(Type::B32(Bytes::B32(B32::default()))),
                },
                Literal::Hash { value, .. } => match value {
                    Hash::H160(_) => Ok(Type::H160(Hash::H160(H160::default()))),
                    Hash::H256(_) => Ok(Type::H256(Hash::H256(H256::default()))),
                    Hash::H512(_) => Ok(Type::H512(Hash::H512(H512::default()))),
                },
                Literal::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),
                Literal::Char { .. } => Ok(Type::Char(Char::from(char::default()))),
                Literal::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
            },

            Expression::MethodCall(mc) => {
                let receiver = wrap_into_expression(*mc.receiver.clone());
                let receiver_type = self.analyse_expr(&receiver, root)?;

                // convert receiver expression to path expression (i.e., check if receiver
                // is a valid path)
                let receiver_as_path_expr = PathExpr::from(receiver);
                let receiver_path = TypePath::from(receiver_as_path_expr);

                let symbol = self.lookup(&receiver_path).cloned();

                println!("receiver path: `{receiver_path:?}`, symbol: {symbol:?}");

                // check if path expression's type is that of an existing type and analyse
                match symbol {
                    Some(
                        Symbol::Struct { path, .. }
                        | Symbol::TupleStruct { path, .. }
                        | Symbol::Enum { path, .. },
                    ) => {
                        if Type::UserDefined(path.clone()) == receiver_type {
                            let method_path =
                                build_item_path(&path, TypePath::from(mc.method_name.clone()));
                            self.analyse_call_or_method_call_expr(method_path, mc.args_opt.clone())
                        } else {
                            Err(SemanticErrorKind::TypeMismatchVariable {
                                name: receiver_path.type_name,
                                expected: format!("`{path}`"),
                                found: format!("`{}`", receiver_type),
                            })
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedType {
                        name: receiver_path.type_name,
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: receiver_path.type_name,
                        expected: "struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Expression::FieldAccess(fa) => {
                let object = wrap_into_expression(*fa.object.clone());

                // convert object to path expression (i.e., check if object
                // is a valid path)
                let object_as_path_expr = PathExpr::from(object.clone());

                let object_path = TypePath::from(object_as_path_expr);
                let object_type = self.analyse_expr(&object, &object_path)?;

                match self.lookup(&object_path).cloned() {
                    Some(Symbol::Struct { struct_def, .. }) => match &struct_def.fields_opt {
                        Some(fields) => match fields.iter().find(|f| f.field_name == fa.field_name)
                        {
                            Some(sdf) => Ok(*sdf.field_type.clone()),
                            _ => Err(SemanticErrorKind::UndefinedField {
                                struct_name: struct_def.struct_name,
                                field_name: fa.field_name.clone(),
                            }),
                        },
                        _ => Ok(Type::UnitType(UnitType)),
                    },

                    None => Err(SemanticErrorKind::UndefinedType {
                        name: Identifier::from(&format!("{}", object_type)),
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: Identifier::from(&format!("{}", object_type)),
                        expected: "struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Expression::Call(c) => {
                let callee = wrap_into_expression(c.callee.clone());

                let callee_path = self.check_path(
                    &TypePath::from(PathExpr::from(callee)),
                    root,
                    String::from("function"),
                )?;

                self.analyse_call_or_method_call_expr(callee_path, c.args_opt.clone())
            }

            Expression::Index(i) => {
                let array_type =
                    self.analyse_expr(&wrap_into_expression(*i.array.clone()), root)?;

                let index_type =
                    self.analyse_expr(&wrap_into_expression(*i.index.clone()), root)?;

                match index_type {
                    Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) => (),

                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "array index".to_string(),
                            found: format!("`{}`", index_type),
                        })
                    }
                }

                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "array".to_string(),
                        found: format!("`{}`", array_type),
                    }),
                }
            }

            Expression::TupleIndex(ti) => {
                let tuple_type =
                    self.analyse_expr(&wrap_into_expression(*ti.tuple.clone()), root)?;

                match tuple_type {
                    Type::Tuple(elem_types) => {
                        if ti.index < UInt::from(elem_types.len()) {
                            Ok(Type::Tuple(elem_types))
                        } else {
                            Err(SemanticErrorKind::TupleIndexOutOfBounds {
                                len: ti.index,
                                i: UInt::from(elem_types.len()),
                            })
                        }
                    }
                    Type::UserDefined(ref tp) => match self.lookup(&tp) {
                        Some(sym) => match sym {
                            Symbol::TupleStruct { .. } => Ok(Type::UserDefined(tp.clone())),

                            _ => Err(SemanticErrorKind::UnexpectedSymbol {
                                name: tp.type_name.clone(),
                                expected: "tuple struct".to_string(),
                                found: sym.to_string(),
                            }),
                        },
                        None => Err(SemanticErrorKind::UndefinedType {
                            name: tp.type_name.clone(),
                        }),
                    },
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "tuple or tuple struct".to_string(),
                        found: format!("`{}`", tuple_type),
                    }),
                }
            }

            Expression::Unwrap(u) => {
                self.analyse_expr(&wrap_into_expression(*u.value_expr.clone()), root)
            }

            Expression::Unary(u) => {
                let expr_type =
                    self.analyse_expr(&wrap_into_expression(*u.value_expr.clone()), root)?;

                match u.unary_op {
                    UnaryOp::Negate => match &expr_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::F32(_)
                        | Type::F64(_) => Ok(expr_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric value".to_string(),
                            found: format!("`{}`", expr_type),
                        }),
                    },
                    UnaryOp::Not => match &expr_type {
                        Type::Bool(_) => Ok(expr_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "boolean".to_string(),
                            found: format!("`{}`", expr_type),
                        }),
                    },
                }
            }

            Expression::Reference(r) => {
                let reference_op = r.reference_op.clone();
                let inner_type = self.analyse_expr(&r.expression, root)?;

                Ok(Type::Reference {
                    reference_op,
                    inner_type: Box::new(inner_type),
                })
            }

            Expression::Dereference(d) => {
                self.analyse_expr(&wrap_into_expression(d.assignee_expr.clone()), root)
            }

            Expression::TypeCast(tc) => {
                let value_type =
                    self.analyse_expr(&wrap_into_expression(*tc.value.clone()), root)?;

                let new_type = *tc.new_type.clone();

                match (&value_type, &new_type) {
                    (
                        Type::I32(_) | Type::I64(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::F32(_)
                        | Type::F64(_),
                    )
                    | (
                        Type::U8(_) | Type::Byte(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::F32(_)
                        | Type::F64(_)
                        | Type::Byte(_)
                        | Type::Char(_),
                    )
                    | (
                        Type::U16(_) | Type::U32(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::F32(_)
                        | Type::F64(_)
                        | Type::Char(_),
                    )
                    | (
                        Type::U64(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::F32(_)
                        | Type::F64(_),
                    )
                    | (Type::U256(_) | Type::U512(_), Type::U256(_) | Type::U512(_))
                    | (Type::F32(_) | Type::F64(_), Type::F32(_) | Type::F64(_))
                    | (
                        Type::B2(_) | Type::B4(_) | Type::B16(_) | Type::B32(_),
                        Type::B2(_) | Type::B4(_) | Type::B16(_) | Type::B32(_),
                    )
                    | (
                        Type::H160(_) | Type::H256(_) | Type::H512(_),
                        Type::H160(_) | Type::H256(_) | Type::H512(_),
                    )
                    | (Type::Char(_), Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_)) => {
                        Ok(new_type)
                    }
                    (t, u) => Err(SemanticErrorKind::TypeCastError {
                        from: format!("`{}`", t),
                        to: format!("`{}`", u),
                    }),
                }
            }

            Expression::Binary(b) => {
                let lhs_type = self.analyse_expr(&wrap_into_expression(*b.lhs.clone()), root)?;

                let rhs_type = self.analyse_expr(&wrap_into_expression(*b.rhs.clone()), root)?;

                match (&lhs_type, &rhs_type) {
                    (
                        Type::I32(_),
                        Type::I32(_) | Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_),
                    ) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32` or unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (
                        Type::I64(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_),
                    ) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "integer or unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U16(_), Type::U8(_) | Type::U16(_)) => {
                        Ok(Type::U16(UInt::U16(u16::default())))
                    }

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16` or `u8`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U32(_), Type::U8(_) | Type::U16(_) | Type::U32(_)) => {
                        Ok(Type::U32(UInt::U32(u32::default())))
                    }

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32` or smaller unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U64(_), Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_)) => {
                        Ok(Type::U64(UInt::U64(u64::default())))
                    }

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64` or smaller unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U512(_), Type::U256(_) | Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512` or `u256`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::F64(_), Type::F32(_) | Type::F64(_)) => {
                        Ok(Type::F64(Float::F64(F64::default())))
                    }

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64` or `f32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "numeric values with matching types".to_string(),
                        found: format!("`{} {} {}`", &lhs_type, b.binary_op, &rhs_type),
                    }),
                }
            }

            Expression::Comparison(c) => {
                let lhs_type = self.analyse_expr(&wrap_into_expression(c.lhs.clone()), root)?;

                let rhs_type = self.analyse_expr(&wrap_into_expression(c.rhs.clone()), root)?;

                match (&lhs_type, &rhs_type) {
                    (
                        Type::I32(_),
                        Type::I32(_) | Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_),
                    ) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32` or unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (
                        Type::I64(_),
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_),
                    ) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "integer or unsigned integer".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::U512(_), Type::U256(_) | Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "numeric values with matching types".to_string(),
                        found: format!("`{} {} {}`", &lhs_type, c.comparison_op, &rhs_type),
                    }),
                }
            }

            Expression::Grouped(g) => self.analyse_expr(&g.inner_expression, root),

            Expression::Range(r) => match (&r.from_expr_opt, &r.to_expr_opt) {
                (None, None) => Ok(Type::UnitType(UnitType)),
                (None, Some(to)) => {
                    let to_type = self.analyse_expr(&wrap_into_expression(*to.clone()), root)?;

                    match to_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_) => Ok(to_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type".to_string(),
                            found: format!("`{}`", to_type),
                        }),
                    }
                }
                (Some(from), None) => {
                    let from_type =
                        self.analyse_expr(&wrap_into_expression(*from.clone()), root)?;

                    match from_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_) => Ok(from_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type".to_string(),
                            found: format!("`{}`", from_type),
                        }),
                    }
                }
                (Some(from), Some(to)) => {
                    let from_type =
                        self.analyse_expr(&wrap_into_expression(*from.clone()), root)?;

                    match from_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_) => (),
                        _ => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "numeric type".to_string(),
                                found: format!("`{}`", from_type),
                            })
                        }
                    }

                    let to_type = self.analyse_expr(&wrap_into_expression(*to.clone()), root)?;

                    match to_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_) => (),
                        _ => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "numeric type".to_string(),
                                found: format!("`{}`", to_type),
                            })
                        }
                    }

                    if from_type == to_type {
                        Ok(to_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchValues {
                            expected: format!("`{}`", from_type),
                            found: format!("`{}`", to_type),
                        })
                    }
                }
            },

            Expression::Assignment(a) => {
                let assignee = wrap_into_expression(a.lhs.clone());
                let assignee_type = self.analyse_expr(&assignee, root)?;

                let value_type = self.analyse_expr(&wrap_into_expression(a.rhs.clone()), root)?;

                if value_type != assignee_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: format!("`{}`", assignee_type),
                        found: format!("`{}`", value_type),
                    });
                }

                let assignee_as_path_expr = PathExpr::from(assignee);
                let assignee_path = self.check_path(
                    &TypePath::from(assignee_as_path_expr),
                    root,
                    String::from("assignee expression"),
                )?;

                match self.lookup(&assignee_path).cloned() {
                    Some(Symbol::Variable { var_type, .. }) => match var_type == assignee_type {
                        true => {
                            self.analyse_expr(&wrap_into_expression(a.rhs.clone()), root)?;
                            Ok(var_type)
                        }
                        false => Err(SemanticErrorKind::TypeMismatchVariable {
                            name: assignee_path.type_name,
                            expected: format!("`{}`", assignee_type),
                            found: format!("`{}`", var_type),
                        }),
                    },
                    Some(Symbol::Constant { constant_name, .. }) => {
                        Err(SemanticErrorKind::ConstantReassignment {
                            name: constant_name,
                        })
                    }
                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: assignee_path.type_name,
                        expected: format!("`{}`", assignee_type),
                        found: sym.to_string(),
                    }),
                    _ => Err(SemanticErrorKind::UndefinedVariable {
                        name: assignee_path.type_name,
                    }),
                }
            }

            Expression::CompoundAssignment(ca) => {
                let assignee = wrap_into_expression(ca.lhs.clone());
                let assignee_type = self.analyse_expr(&assignee, root)?;

                let value_type = self.analyse_expr(&wrap_into_expression(ca.rhs.clone()), root)?;

                let assignee_as_path_expr = PathExpr::from(assignee);
                let assignee_path = self.check_path(
                    &TypePath::from(assignee_as_path_expr),
                    root,
                    String::from("assignee expression"),
                )?;

                match self.lookup(&assignee_path) {
                    Some(Symbol::Variable { .. }) => match (&assignee_type, &value_type) {
                        (
                            Type::I32(_),
                            Type::I32(_) | Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_),
                        ) => Ok(Type::I32(Int::I32(i32::default()))),

                        (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`i32` or unsigned integer".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (
                            Type::I64(_),
                            Type::I32(_)
                            | Type::I64(_)
                            | Type::U8(_)
                            | Type::U16(_)
                            | Type::U32(_)
                            | Type::U64(_),
                        ) => Ok(Type::I64(Int::I64(i64::default()))),

                        (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "integer or unsigned integer".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                        (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u8`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::U16(_), Type::U8(_) | Type::U16(_)) => {
                            Ok(Type::U16(UInt::U16(u16::default())))
                        }

                        (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u16`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::U32(_), Type::U8(_) | Type::U16(_) | Type::U32(_)) => {
                            Ok(Type::U32(UInt::U32(u32::default())))
                        }

                        (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u32`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (
                            Type::U64(_),
                            Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_),
                        ) => Ok(Type::U64(UInt::U64(u64::default()))),

                        (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u64`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::U256(_), Type::U256(_)) => {
                            Ok(Type::U256(BigUInt::U256(U256::default())))
                        }

                        (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u256`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::U512(_), Type::U256(_) | Type::U512(_)) => {
                            Ok(Type::U512(BigUInt::U512(U512::default())))
                        }

                        (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`u512`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                        (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`f32`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::F64(_), Type::F32(_) | Type::F64(_)) => {
                            Ok(Type::F64(Float::F64(F64::default())))
                        }

                        (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`f64`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "numeric values with matching types".to_string(),
                            found: format!(
                                "`{} {} {}`",
                                &assignee_type, ca.compound_assignment_op, &value_type
                            ),
                        }),
                    },
                    Some(Symbol::Constant { constant_name, .. }) => {
                        Err(SemanticErrorKind::ConstantReassignment {
                            name: constant_name.clone(),
                        })
                    }
                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: assignee_path.type_name,
                        expected: format!("`{}`", assignee_type),
                        found: sym.to_string(),
                    }),
                    _ => Err(SemanticErrorKind::UndefinedVariable {
                        name: assignee_path.type_name,
                    }),
                }
            }

            Expression::Return(r) => match &r.expression_opt {
                Some(expr) => self.analyse_expr(&expr.clone(), root),
                _ => Ok(Type::UnitType(UnitType)),
            },

            Expression::Break(_) => Ok(Type::UnitType(UnitType)),

            Expression::Continue(_) => Ok(Type::UnitType(UnitType)),

            Expression::Underscore(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from("_"),
            })),

            Expression::Closure(c) => {
                let params_opt =
                    match &c.closure_params {
                        ClosureParams::Some(params) => {
                            let mut function_params: Vec<FunctionOrMethodParam> = Vec::new();

                            for param in params {
                                let param_type = param.type_ann_opt.clone().unwrap_or(Box::new(
                                    Type::InferredType(InferredType {
                                        name: Identifier::from("_"),
                                    }),
                                ));

                                let function_param = FunctionParam {
                                    param_name: param.param_name.clone(),
                                    param_type,
                                };

                                function_params
                                    .push(FunctionOrMethodParam::FunctionParam(function_param))
                            }

                            Some(function_params)
                        }
                        _ => None,
                    };

                self.enter_scope(ScopeKind::Function(
                    TypePath::from(Identifier::from("")).to_string(),
                ));

                if let Some(params) = &params_opt {
                    let param_types: Vec<Type> = params.iter().map(|p| p.param_type()).collect();

                    for (param, param_type) in params.iter().zip(param_types) {
                        let param_path = TypePath::from(param.param_name());

                        self.insert(
                            param_path,
                            Symbol::Variable {
                                name: param.param_name(),
                                var_type: param_type,
                            },
                        )?;
                    }
                }

                let return_type = match &c.return_type_opt {
                    Some(ty) => Ok(*ty.clone()),
                    _ => Ok(Type::UnitType(UnitType)),
                }?;

                let expression_type = self.analyse_expr(&c.body_expression, root)?;

                self.exit_scope();

                if expression_type != return_type {
                    if let Type::Result { ok_type, err_type } = expression_type.clone() {
                        if *ok_type
                            == Type::InferredType(InferredType {
                                name: Identifier::from("_"),
                            })
                            || *err_type
                                == Type::InferredType(InferredType {
                                    name: Identifier::from("_"),
                                })
                        {
                            ()
                        } else {
                            return Err(SemanticErrorKind::TypeMismatchReturnType {
                                expected: format!("`{}`", return_type),
                                found: format!("`{}`", expression_type),
                            });
                        }
                    }
                }

                let function_ptr = FunctionPtr {
                    params_opt,
                    return_type_opt: Some(Box::new(return_type)),
                };

                Ok(Type::FunctionPtr(function_ptr))
            }

            Expression::Array(a) => match &a.elements_opt {
                Some(elements) => match elements.first() {
                    Some(expr) => {
                        let mut element_count = 0u64;

                        let first_element_type = self.analyse_expr(expr, root)?;

                        element_count += 1;

                        for elem in elements.iter().skip(1) {
                            let element_type = self.analyse_expr(elem, root)?;

                            element_count += 1;

                            if element_type != first_element_type {
                                return Err(SemanticErrorKind::TypeMismatchArray {
                                    expected: format!("`{}`", first_element_type),
                                    found: format!("`{}`", element_type),
                                });
                            }
                        }

                        Ok(Type::Array {
                            element_type: Box::new(first_element_type),
                            num_elements: UInt::U64(element_count),
                        })
                    }
                    _ => {
                        let element_type = Type::UnitType(UnitType);
                        let array = Type::Array {
                            element_type: Box::new(element_type),
                            num_elements: UInt::U64(0u64),
                        };

                        Ok(array)
                    }
                },
                _ => Ok(Type::UnitType(UnitType)),
            },

            Expression::Tuple(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for elem in t.tuple_elements.elements.iter() {
                    let ty = self.analyse_expr(elem, root)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Expression::Struct(s) => {
                let type_path = build_item_path(root, TypePath::from(s.struct_path.clone()));

                match self.lookup(&type_path).cloned() {
                    Some(Symbol::Struct { struct_def, path }) => {
                        self.insert(
                            TypePath::from(s.struct_path.clone()),
                            Symbol::Struct {
                                path: path.clone(),
                                struct_def: struct_def.clone(),
                            },
                        )?;

                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for field in &struct_fields.unwrap() {
                                let field_name = field.field_name.clone();
                                let field_value = *field.field_value.clone();
                                let field_type = self.analyse_expr(&field_value, root)?;
                                field_map.insert(field_name, field_type);
                            }
                        }

                        if struct_def.fields_opt.is_some() {
                            for def_fld in &struct_def.fields_opt.unwrap() {
                                match field_map.get(&def_fld.field_name) {
                                    Some(obj_fld_type) if *obj_fld_type == *def_fld.field_type => {
                                        ()
                                    }
                                    Some(obj_fld_type) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: path.type_name,
                                            expected: format!("`{}`", *def_fld.field_type),
                                            found: format!("`{}`", obj_fld_type),
                                        })
                                    }
                                    _ => {
                                        return Err(SemanticErrorKind::MissingStructField {
                                            expected: format!(
                                                "`{}: {}`",
                                                &def_fld.field_name, *def_fld.field_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: type_path.type_name,
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: type_path.type_name,
                        expected: "struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Expression::TupleStruct(ts) => {
                let type_path = build_item_path(root, TypePath::from(ts.struct_path.clone()));

                match self.lookup(&type_path).cloned() {
                    Some(Symbol::TupleStruct {
                        tuple_struct_def,
                        path,
                    }) => {
                        self.insert(
                            TypePath::from(ts.struct_path.clone()),
                            Symbol::TupleStruct {
                                path: path.clone(),
                                tuple_struct_def: tuple_struct_def.clone(),
                            },
                        )?;

                        let elements_opt = ts.struct_elements_opt.clone();
                        let fields_opt = tuple_struct_def.fields_opt;

                        match (&elements_opt, &fields_opt) {
                            (None, None) => Ok(Type::UnitType(UnitType)),

                            (None, Some(fields)) => Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: tuple_struct_def.struct_name.clone(),
                                expected: fields.len(),
                                found: 0,
                            }),

                            (Some(elements), None) => {
                                Err(SemanticErrorKind::ArgumentCountMismatch {
                                    name: tuple_struct_def.struct_name.clone(),
                                    expected: 0,
                                    found: elements.len(),
                                })
                            }
                            (Some(elements), Some(fields)) => {
                                if elements.len() != fields.len() {
                                    return Err(SemanticErrorKind::ArgumentCountMismatch {
                                        name: tuple_struct_def.struct_name.clone(),
                                        expected: fields.len(),
                                        found: elements.len(),
                                    });
                                }

                                for (elem, field) in elements.iter().zip(fields) {
                                    let elem_type = self.analyse_expr(
                                        &elem,
                                        &TypePath::from(Identifier::from("")),
                                    )?;
                                    let field_type = *field.field_type.clone();

                                    if elem_type != field_type {
                                        return Err(SemanticErrorKind::TypeMismatchArgument {
                                            name: tuple_struct_def.struct_name.clone(),
                                            expected: format!("`{}`", field_type),
                                            found: format!("`{}`", elem_type),
                                        });
                                    }
                                }

                                Ok(Type::UserDefined(path))
                            }
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: type_path.type_name,
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: type_path.type_name,
                        expected: "tuple struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Expression::Mapping(m) => match &m.pairs_opt {
                Some(pairs) => match pairs.first() {
                    Some(pair) => {
                        let key_type = self.analyse_patt(&pair.k.clone())?;
                        let value_type = self.analyse_expr(&pair.v.clone(), root)?;

                        for pair in pairs.iter().skip(1) {
                            let pair_key_type = self.analyse_patt(&pair.k.clone())?;
                            let pair_value_type = self.analyse_expr(&pair.v.clone(), root)?;

                            if (&pair_key_type, &pair_value_type) != (&key_type, &value_type) {
                                return Err(SemanticErrorKind::UnexpectedType {
                                    expected: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &key_type, &value_type
                                    ),
                                    found: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &pair_key_type, &pair_value_type
                                    ),
                                });
                            }
                        }

                        Ok(Type::Mapping {
                            key_type: Box::new(key_type),
                            value_type: Box::new(value_type),
                        })
                    }
                    _ => {
                        let key_type = Box::new(Type::UnitType(UnitType));
                        let value_type = Box::new(Type::UnitType(UnitType));

                        Ok(Type::Mapping {
                            key_type,
                            value_type,
                        })
                    }
                },
                _ => Ok(Type::UnitType(UnitType)),
            },

            Expression::Block(b) => match &b.statements_opt {
                Some(stmts) => {
                    self.enter_scope(ScopeKind::LocalBlock);

                    let mut cloned_iter = stmts.iter().peekable().clone();

                    for stmt in stmts {
                        cloned_iter.next();

                        match stmt {
                            Statement::Expression(e) => match e {
                                Expression::Return(_)
                                | Expression::Break(_)
                                | Expression::Continue(_) => {
                                    self.analyse_expr(e, root)?;

                                    match cloned_iter.peek() {
                                        Some(_) => self.logger.warn("unreachable code"),
                                        _ => (),
                                    }
                                }
                                _ => {
                                    self.analyse_expr(e, root)?;
                                }
                            },
                            _ => match self.analyse_stmt(stmt, root) {
                                Ok(_) => (),
                                Err(e) => return Err(e),
                            },
                        }
                    }

                    let ty = match stmts.last() {
                        Some(stmt) => match stmt {
                            Statement::Expression(e) => self.analyse_expr(e, root)?,
                            _ => Type::UnitType(UnitType),
                        },
                        _ => Type::UnitType(UnitType),
                    };

                    self.exit_scope();

                    Ok(ty)
                }

                _ => Ok(Type::UnitType(UnitType)),
            },

            Expression::If(i) => {
                self.analyse_expr(&Expression::Grouped(*i.condition.clone()), root)?;

                let if_block_type =
                    self.analyse_expr(&Expression::Block(*i.if_block.clone()), root)?;

                let else_if_blocks_type = match &i.else_if_blocks_opt {
                    Some(blocks) => match blocks.first() {
                        Some(_) => {
                            for block in blocks.iter() {
                                let block_type =
                                    self.analyse_expr(&Expression::If(*block.clone()), root)?;

                                if block_type != if_block_type {
                                    return Err(SemanticErrorKind::TypeMismatchValues {
                                        expected: format!("`{}`", if_block_type),
                                        found: format!("`{}`", block_type),
                                    });
                                }
                            }

                            if_block_type.clone()
                        }
                        _ => Type::UnitType(UnitType),
                    },
                    _ => Type::UnitType(UnitType),
                };

                let trailing_else_block_type = match &i.trailing_else_block_opt {
                    Some(block) => self.analyse_expr(&Expression::Block(block.clone()), root)?,
                    _ => Type::UnitType(UnitType),
                };

                if i.else_if_blocks_opt.is_some() && else_if_blocks_type != if_block_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: format!("`{}`", if_block_type),
                        found: format!("`{}`", else_if_blocks_type),
                    });
                }

                if trailing_else_block_type != if_block_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: format!("`{}`", if_block_type),
                        found: format!("`{}`", trailing_else_block_type),
                    });
                }

                Ok(if_block_type)
            }

            Expression::Match(m) => {
                self.enter_scope(ScopeKind::MatchExpr);

                let scrutinee_type =
                    self.analyse_expr(&wrap_into_expression(m.scrutinee.clone()), root)?;

                let patt_type = self.analyse_patt(&m.final_arm.matched_pattern.clone())?;

                if patt_type != scrutinee_type {
                    if let Type::InferredType(_) = patt_type {
                        ()
                    } else {
                        return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                            loc: "scrutinee and matched pattern".to_string(),
                            expected: format!("`{}`", scrutinee_type),
                            found: format!("`{}`", patt_type),
                        });
                    }
                }

                let expr_type = self.analyse_expr(&m.final_arm.arm_expression.clone(), root)?;

                if let Some(arms) = &m.match_arms_opt {
                    for arm in arms.iter() {
                        let arm_patt_type = self.analyse_patt(&arm.matched_pattern)?;

                        if arm_patt_type != patt_type {
                            if let Type::InferredType(_) = patt_type {
                                ()
                            } else {
                                return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                    loc: "matched pattern".to_string(),
                                    expected: format!("`{}`", patt_type),
                                    found: format!("`{}`", arm_patt_type),
                                });
                            }
                        }

                        let arm_expr_type = self.analyse_expr(&arm.arm_expression.clone(), root)?;

                        if arm_expr_type != expr_type {
                            return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                loc: "match arm expression".to_string(),
                                expected: format!("`{}`", expr_type),
                                found: format!("`{}`", arm_expr_type),
                            });
                        }
                    }
                }

                self.exit_scope();

                Ok(expr_type)
            }

            Expression::ForIn(fi) => {
                self.enter_scope(ScopeKind::ForInLoop);

                let iter_type = self.analyse_expr(&fi.iterator.clone(), root)?;

                let element_type = match iter_type.clone() {
                    Type::Array { element_type, .. } | Type::Vec { element_type, .. } => {
                        *element_type
                    }

                    Type::Reference { inner_type, .. } => match *inner_type {
                        Type::Array { element_type, .. } | Type::Vec { element_type, .. } => {
                            *element_type
                        }
                        _ => {
                            return Err(SemanticErrorKind::TypeMismatchArray {
                                expected: "iterable type".to_string(),
                                found: iter_type.to_string(),
                            })
                        }
                    },

                    _ => {
                        return Err(SemanticErrorKind::TypeMismatchArray {
                            expected: "iterable type".to_string(),
                            found: iter_type.to_string(),
                        });
                    }
                };

                if let Pattern::IdentifierPatt(id) = *fi.pattern.clone() {
                    self.insert(
                        TypePath::from(id.name.clone()),
                        Symbol::Variable {
                            name: id.name,
                            var_type: element_type,
                        },
                    )?;
                }

                self.analyse_patt(&fi.pattern.clone())?;

                let ty = self.analyse_expr(&Expression::Block(fi.block.clone()), root)?;

                self.exit_scope();

                Ok(ty)
            }

            Expression::While(w) => {
                self.analyse_expr(&Expression::Grouped(*w.condition.clone()), root)?;
                let ty = self.analyse_expr(&Expression::Block(w.block.clone()), root)?;
                Ok(ty)
            }

            Expression::SomeExpr(s) => {
                let ty = self.analyse_expr(&s.expression.clone().inner_expression, root)?;

                let ty = if ty == Type::Tuple(Vec::new()) {
                    Type::UnitType(UnitType)
                } else {
                    ty
                };

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Expression::NoneExpr(_) => Ok(Type::Option {
                inner_type: Box::new(Type::UnitType(UnitType)),
            }),

            Expression::ResultExpr(r) => {
                let ty = self.analyse_expr(&r.expression.clone().inner_expression, root)?;

                let ty = if ty == Type::Tuple([].to_vec()) {
                    Type::UnitType(UnitType)
                } else {
                    ty
                };

                match r.kw_ok_or_err.clone() {
                    Keyword::Ok => Ok(Type::Result {
                        ok_type: Box::new(ty),
                        err_type: Box::new(Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        })),
                    }),
                    Keyword::Err => Ok(Type::Result {
                        ok_type: Box::new(Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        })),
                        err_type: Box::new(ty),
                    }),
                    keyword => Err(SemanticErrorKind::UnexpectedKeyword {
                        expected: "`Ok` or `Err`".to_string(),
                        found: format!("`{}`", keyword),
                    }),
                }
            }
        }
    }

    fn check_path(
        &mut self,
        path: &TypePath,
        root: &TypePath,
        expected_value: String,
    ) -> Result<TypePath, SemanticErrorKind> {
        let item_path = if self.lookup(path).is_some() {
            path.clone()
        } else {
            let full_path = build_item_path(root, path.clone());

            self.logger.debug(&format!(
                "cannot find symbol at path `{path}`, trying `{full_path}` …",
            ));

            if self.lookup(&full_path).is_some() {
                full_path
            } else {
                return Err(SemanticErrorKind::MissingValue {
                    expected: expected_value,
                });
            }
        };

        Ok(item_path)
    }

    fn analyse_call_or_method_call_expr(
        &mut self,
        path: TypePath,
        args_opt: Option<Vec<Expression>>,
    ) -> Result<Type, SemanticErrorKind> {
        match self.lookup(&path).cloned() {
            Some(Symbol::Function { function, .. }) => {
                let params = function.params_opt.clone();
                let return_type = function.return_type_opt.clone();

                match (&args_opt, &params) {
                    (None, None) => match return_type {
                        Some(ty) => Ok(*ty),
                        _ => Ok(Type::UnitType(UnitType)),
                    },
                    (None, Some(params)) => {
                        let mut self_counter: usize = 0;
                        let mut func_param_counter: usize = 0;

                        for param in params {
                            if let Type::SelfType(_) = param.param_type() {
                                self_counter += 1;
                            } else {
                                func_param_counter += 1;
                            }
                        }

                        if self_counter > 1 {
                            return Err(SemanticErrorKind::MethodParamCountError);
                        }

                        if self_counter != params.len() {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: function.function_name.clone(),
                                expected: self_counter,
                                found: self_counter + func_param_counter,
                            });
                        }

                        match return_type {
                            Some(ty) => Ok(*ty),
                            _ => Ok(Type::UnitType(UnitType)),
                        }
                    }
                    (Some(args), None) => Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: function.function_name.clone(),
                        expected: 0,
                        found: args.len(),
                    }),
                    (Some(args), Some(params)) => {
                        let mut self_counter: usize = 0;

                        for param in params {
                            if let Type::SelfType(_) = param.param_type() {
                                self_counter += 1;
                            }
                        }

                        if self_counter > 1 {
                            return Err(SemanticErrorKind::MethodParamCountError);
                        }

                        let num_func_params = params.len() - self_counter;

                        if args.len() != num_func_params {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: function.function_name.clone(),
                                expected: params.len(),
                                found: args.len(),
                            });
                        }

                        for (arg, param) in args.iter().zip(params) {
                            let arg_type =
                                self.analyse_expr(&arg, &TypePath::from(Identifier::from("")))?;
                            let param_type = param.param_type();

                            if arg_type != param_type {
                                return Err(SemanticErrorKind::TypeMismatchArgument {
                                    name: function.function_name.clone(),
                                    expected: format!("`{}`", param_type),
                                    found: format!("`{}`", arg_type),
                                });
                            }
                        }

                        match return_type {
                            Some(ty) => Ok(*ty),
                            _ => Ok(Type::UnitType(UnitType)),
                        }
                    }
                }
            }

            Some(Symbol::TupleStruct {
                path,
                tuple_struct_def,
            }) => {
                let elements = tuple_struct_def.fields_opt.clone();

                match (&args_opt, &elements) {
                    (None, None) => Ok(Type::UnitType(UnitType)),

                    (Some(args), None) => Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: tuple_struct_def.struct_name.clone(),
                        expected: 0,
                        found: args.len(),
                    }),

                    (None, Some(elements)) => Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: tuple_struct_def.struct_name.clone(),
                        expected: elements.len(),
                        found: 0,
                    }),

                    (Some(args), Some(elements)) => {
                        if args.len() != elements.len() {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: tuple_struct_def.struct_name.clone(),
                                expected: elements.len(),
                                found: args.len(),
                            });
                        }

                        for (arg, elem) in args.iter().zip(elements) {
                            let arg_type =
                                self.analyse_expr(&arg, &TypePath::from(Identifier::from("")))?;
                            let element_type = elem.field_type.clone();

                            if arg_type != *element_type {
                                return Err(SemanticErrorKind::TypeMismatchArgument {
                                    name: tuple_struct_def.struct_name.clone(),
                                    expected: format!("`{}`", element_type),
                                    found: format!("`{}`", arg_type),
                                });
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }
                }
            }
            None => Err(SemanticErrorKind::UndefinedFunction {
                name: path.type_name,
            }),
            Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                name: path.type_name,
                expected: "function".to_string(),
                found: sym.to_string(),
            }),
        }
    }

    fn analyse_patt(&mut self, pattern: &Pattern) -> Result<Type, SemanticErrorKind> {
        match pattern {
            Pattern::IdentifierPatt(i) => match self.lookup(&TypePath::from(i.name.clone())) {
                Some(Symbol::Variable { var_type, .. }) => Ok(var_type.clone()),
                Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: i.name.clone(),
                    expected: "variable".to_string(),
                    found: sym.to_string(),
                }),
                _ => Err(SemanticErrorKind::UndefinedVariable {
                    name: i.name.clone(),
                }),
            },

            Pattern::PathPatt(p) => {
                let path = TypePath::from(p.clone());

                if let Some(sym) = self.lookup(&path) {
                    Ok(sym.symbol_type())
                } else {
                    Err(SemanticErrorKind::UndefinedVariable {
                        name: path.type_name,
                    })
                }
            }

            Pattern::LiteralPatt(l) => match l {
                LiteralPatt::Int { value } => match value {
                    Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                    Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
                },
                LiteralPatt::UInt { value } => match value {
                    UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                    UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                    UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                    UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
                },
                LiteralPatt::BigUInt { value } => match value {
                    BigUInt::U256(_) => Ok(Type::U256(BigUInt::U256(U256::default()))),
                    BigUInt::U512(_) => Ok(Type::U512(BigUInt::U512(U512::default()))),
                },
                LiteralPatt::Float { value } => match value {
                    Float::F32(_) => Ok(Type::F32(Float::F32(F32::default()))),
                    Float::F64(_) => Ok(Type::F64(Float::F64(F64::default()))),
                },
                LiteralPatt::Byte { .. } => Ok(Type::Byte(Byte::from(u8::default()))),
                LiteralPatt::Bytes { value } => match value {
                    Bytes::B2(_) => Ok(Type::B2(Bytes::B2(B2::default()))),
                    Bytes::B4(_) => Ok(Type::B4(Bytes::B4(B4::default()))),
                    Bytes::B8(_) => Ok(Type::B8(Bytes::B8(B8::default()))),
                    Bytes::B16(_) => Ok(Type::B16(Bytes::B16(B16::default()))),
                    Bytes::B32(_) => Ok(Type::B32(Bytes::B32(B32::default()))),
                },
                LiteralPatt::Hash { value } => match value {
                    Hash::H160(_) => Ok(Type::H160(Hash::H160(H160::default()))),
                    Hash::H256(_) => Ok(Type::H256(Hash::H256(H256::default()))),
                    Hash::H512(_) => Ok(Type::H512(Hash::H512(H512::default()))),
                },
                LiteralPatt::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),
                LiteralPatt::Char { .. } => Ok(Type::Char(Char::from(char::default()))),
                LiteralPatt::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
            },

            Pattern::ReferencePatt(r) => Ok(Type::Reference {
                reference_op: r.reference_op,
                inner_type: Box::new(self.analyse_patt(&r.pattern)?),
            }),

            Pattern::GroupedPatt(g) => self.analyse_patt(&g.inner_pattern),

            Pattern::RangePatt(r) => match (&r.from_pattern_opt, &r.to_pattern_opt) {
                (None, None) => Ok(Type::UnitType(UnitType)),
                (None, Some(to)) => {
                    let to_type = self.analyse_patt(&to.clone())?;

                    match to_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::Byte(_)
                        | Type::Char(_) => Ok(to_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type, `byte` or `char`".to_string(),
                            found: format!("`{}`", to_type),
                        }),
                    }
                }
                (Some(from), None) => {
                    let from_type = self.analyse_patt(&from.clone())?;

                    match from_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::Byte(_)
                        | Type::Char(_) => Ok(from_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type, `byte` or `char`".to_string(),
                            found: format!("`{}`", from_type),
                        }),
                    }
                }
                (Some(from), Some(to)) => {
                    let from_type = self.analyse_patt(&from.clone())?;

                    match from_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::Byte(_)
                        | Type::Char(_) => (),
                        _ => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "numeric type, `byte` or `char`".to_string(),
                                found: format!("`{}`", from_type),
                            })
                        }
                    }

                    let to_type = self.analyse_patt(&to.clone())?;

                    match to_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_)
                        | Type::Byte(_)
                        | Type::Char(_) => (),
                        _ => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "numeric type, `byte` or `char`".to_string(),
                                found: format!("`{}`", from_type),
                            })
                        }
                    }

                    if from_type == to_type {
                        Ok(to_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchValues {
                            expected: format!("`{}`", from_type),
                            found: format!("`{}`", to_type),
                        })
                    }
                }
            },

            Pattern::TuplePatt(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for patt in t.tuple_patt_elements.elements.iter() {
                    let ty = self.analyse_patt(patt)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Pattern::StructPatt(s) => {
                let type_path = TypePath::from(s.struct_path.clone());

                match self.lookup(&type_path).cloned() {
                    Some(Symbol::Struct { struct_def, path }) => {
                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for field in &struct_fields.unwrap() {
                                let field_name = field.field_name.clone();
                                let field_value = field.field_value.clone();
                                let field_type = self.analyse_patt(&field_value)?.clone();
                                field_map.insert(field_name, field_type);
                            }
                        }

                        if struct_def.fields_opt.is_some() {
                            for def_fld in &struct_def.fields_opt.unwrap() {
                                match field_map.get(&def_fld.field_name) {
                                    Some(obj_fld_type) if *obj_fld_type == *def_fld.field_type => {
                                        ()
                                    }
                                    Some(obj_fld_type) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: path.type_name,
                                            expected: format!("`{}`", def_fld.field_type),
                                            found: format!("`{}`", obj_fld_type),
                                        })
                                    }
                                    _ => {
                                        return Err(SemanticErrorKind::MissingStructField {
                                            expected: format!(
                                                "`{}: {}`",
                                                &def_fld.field_name, *def_fld.field_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: type_path.type_name,
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: type_path.type_name,
                        expected: "struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Pattern::TupleStructPatt(ts) => {
                let type_path = TypePath::from(ts.struct_path.clone());

                match self.lookup(&type_path).cloned() {
                    Some(Symbol::TupleStruct {
                        tuple_struct_def,
                        path,
                    }) => {
                        let elements_opt = ts.struct_elements_opt.clone();
                        let fields_opt = tuple_struct_def.fields_opt;

                        match (&elements_opt, &fields_opt) {
                            (None, None) => Ok(Type::UnitType(UnitType)),

                            (None, Some(fields)) => Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: tuple_struct_def.struct_name.clone(),
                                expected: fields.len(),
                                found: 0,
                            }),

                            (Some(elements), None) => {
                                Err(SemanticErrorKind::ArgumentCountMismatch {
                                    name: tuple_struct_def.struct_name.clone(),
                                    expected: 0,
                                    found: elements.len(),
                                })
                            }
                            (Some(elements), Some(fields)) => {
                                if elements.len() != fields.len() {
                                    return Err(SemanticErrorKind::ArgumentCountMismatch {
                                        name: tuple_struct_def.struct_name.clone(),
                                        expected: fields.len(),
                                        found: elements.len(),
                                    });
                                }

                                for (elem, field) in elements.iter().zip(fields) {
                                    let elem_type = self.analyse_patt(&elem)?;
                                    let field_type = *field.field_type.clone();

                                    if elem_type != field_type {
                                        return Err(SemanticErrorKind::TypeMismatchArgument {
                                            name: tuple_struct_def.struct_name.clone(),
                                            expected: format!("`{}`", field_type),
                                            found: format!("`{}`", elem_type),
                                        });
                                    }
                                }

                                Ok(Type::UserDefined(path))
                            }
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: type_path.type_name,
                    }),

                    Some(sym) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: type_path.type_name,
                        expected: "struct".to_string(),
                        found: sym.to_string(),
                    }),
                }
            }

            Pattern::WildcardPatt(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from("_"),
            })),

            Pattern::RestPatt(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from(".."),
            })),

            Pattern::OrPatt(o) => {
                let first_patt_type = self.analyse_patt(&o.first_pattern.clone())?;

                for patt in o.subsequent_patterns.iter() {
                    let subsequent_patt_type = self.analyse_patt(patt)?;

                    if subsequent_patt_type != first_patt_type {
                        return Err(SemanticErrorKind::TypeMismatchOrPatt {
                            expected: first_patt_type.to_string(),
                            found: subsequent_patt_type.to_string(),
                        });
                    }
                }

                Ok(first_patt_type)
            }

            Pattern::SomePatt(s) => {
                let ty = self.analyse_patt(&s.pattern.clone().inner_pattern)?;

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Pattern::NonePatt(_) => Ok(Type::Option {
                inner_type: Box::new(Type::UnitType(UnitType)),
            }),

            Pattern::ResultPatt(r) => {
                let ty = self.analyse_patt(&r.pattern.clone().inner_pattern)?;

                match r.kw_ok_or_err.clone() {
                    Keyword::Ok => Ok(Type::Result {
                        ok_type: Box::new(ty),
                        err_type: Box::new(Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        })),
                    }),
                    Keyword::Err => Ok(Type::Result {
                        ok_type: Box::new(Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        })),
                        err_type: Box::new(ty),
                    }),
                    keyword => Err(SemanticErrorKind::UnexpectedKeyword {
                        expected: "`Ok` or `Err`".to_string(),
                        found: format!("`{}`", keyword),
                    }),
                }
            }
        }
    }

    fn log_error(&mut self, error_kind: SemanticErrorKind, span: &Span) {
        let error = CompilerError::new(error_kind, span.start(), &span.input());

        self.logger.error(&error.to_string());

        self.errors.push(error);
    }
}

fn wrap_into_expression<T>(value: T) -> Expression
where
    T: Clone + fmt::Debug + TryFrom<Expression>,
    Expression: From<T>,
{
    Expression::from(value.clone())
}
