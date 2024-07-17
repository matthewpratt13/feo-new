#![allow(dead_code)]

mod symbol_table;

#[cfg(test)]
mod tests;

use core::fmt;
use std::collections::HashMap;

use symbol_table::{Scope, ScopeKind, Symbol, SymbolTable};

use crate::{
    ast::{
        ArrayExpr, AssigneeExpr, BigUInt, Bool, Byte, Bytes, Char, ClosureParams, Expression,
        Float, FunctionItem, FunctionOrMethodParam, FunctionParam, FunctionPtr, Identifier,
        ImportDecl, InferredType, InherentImplItem, Int, Item, Keyword, Literal, MethodCallExpr,
        ModuleItem, NoneExpr, NonePatt, PathExpr, PathRoot, PathType, Pattern, ResultExpr,
        SelfType, SomeExpr, Statement, Str, TraitDefItem, TraitImplItem, Type, UInt, UnaryOp,
        UnderscoreExpr, Unit, Visibility,
    },
    error::{CompilerError, ErrorsEmitted, SemanticErrorKind},
    logger::{LogLevel, Logger},
    parser::{ty::build_item_path, Program},
    span::{Span, Spanned},
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

struct SemanticAnalyser {
    scope_stack: Vec<Scope>,
    module_registry: HashMap<PathType, SymbolTable>,
    errors: Vec<CompilerError<SemanticErrorKind>>,
    logger: Logger,
}

impl SemanticAnalyser {
    pub(crate) fn new(log_level: LogLevel, external_code: Option<SymbolTable>) -> Self {
        let mut logger = Logger::new(log_level);

        let mut external_symbols: SymbolTable = HashMap::new();

        let mut module_registry: HashMap<PathType, SymbolTable> = HashMap::new();

        // add external code (e.g., library functions) to the global scope if there is any
        if let Some(ext) = external_code {
            logger.debug(&format!("importing external code ..."));

            for (id, ref sym) in ext {
                if let Symbol::Module { path, symbols, .. } = sym.clone() {
                    module_registry.insert(path, symbols);
                }

                external_symbols.insert(id, sym.clone());
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
            .debug(&format!("entering new scope: `{:?}` ...", &scope_kind));

        self.scope_stack.push(Scope {
            scope_kind,
            symbols: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) -> Option<Scope> {
        if let Some(exited_scope) = self.scope_stack.pop() {
            self.logger
                .debug(&format!("exited scope: `{:?}`", exited_scope.scope_kind));

            Some(exited_scope)
        } else {
            None
        }
    }

    fn insert(&mut self, path: PathType, symbol: Symbol) -> Result<(), SemanticErrorKind> {
        if let Some(curr_scope) = self.scope_stack.last_mut() {
            self.logger.debug(&format!(
                "inserting path `{}` into scope `{:?}`",
                path, curr_scope.scope_kind
            ));

            curr_scope.symbols.insert(path, symbol);

            Ok(())
        } else {
            Err(SemanticErrorKind::UndefinedScope)
        }
    }

    fn lookup(&mut self, path: &PathType) -> Option<&Symbol> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.symbols.get(path) {
                self.logger.debug(&format!(
                    "found path `{}` in scope `{:?}`",
                    path, scope.scope_kind
                ));

                return Some(symbol);
            }
        }
        self.logger
            .warn(&format!("path `{}` not found in any scope", path));
        None
    }

    fn analyse_program(&mut self, program: &Program, path: PathType) -> Result<(), ErrorsEmitted> {
        self.logger.info("starting semantic analysis ...");

        let program_path = if let Some(Scope {
            scope_kind: ScopeKind::Lib,
            ..
        }) = self.scope_stack.last()
        {
            let mut prefix = vec![Identifier::from("lib")];

            if let Some(mut t) = path.associated_type_path_prefix_opt {
                if t.get(0) == Some(&Identifier::from("lib")) {
                    t.remove(0);
                }

                prefix.append(&mut t);
            }

            if path.type_name != Identifier::from("") {
                prefix.push(path.type_name);
            }

            PathType::from(prefix)
        } else {
            path
        };

        self.enter_scope(ScopeKind::RootModule(program_path.to_string()));

        let mut module_items: Vec<Item> = Vec::new();

        program.statements.clone().into_iter().for_each(|s| {
            match s {
                Statement::Item(i) => module_items.push(i),
                _ => (),
            };
        });

        let root_module = ModuleItem {
            outer_attributes_opt: None,
            visibility: Visibility::Pub,
            kw_module: Keyword::Module,
            module_name: program_path.type_name.clone(),
            inner_attributes_opt: None,
            items_opt: {
                if module_items.is_empty() {
                    None
                } else {
                    Some(module_items)
                }
            },
            span: Span::new("", 0, 0),
        };

        let mut module_contents: SymbolTable = HashMap::new();

        module_contents.insert(
            program_path.clone(),
            Symbol::Module {
                path: PathType::from(root_module.module_name.clone()),
                module: root_module,
                symbols: HashMap::new(),
            },
        );

        self.module_registry
            .insert(program_path.clone(), module_contents);

        for s in &program.statements {
            self.analyse_stmt(s, &program_path).map_err(|e| {
                self.log_error(e, &s.span());
                ErrorsEmitted
            })?;
        }

        self.logger
            .info("semantic analysis complete, no errors detected");

        Ok(())
    }

    fn analyse_stmt(
        &mut self,
        statement: &Statement,
        root: &PathType,
    ) -> Result<(), SemanticErrorKind> {
        match statement {
            Statement::Let(ls) => {
                self.logger
                    .debug(&format!("analysing statement: `{}` ...", statement));

                // variables declared must have a type and are assigned the unit type if not;
                // this prevents uninitialized variable errors
                let value_type = if let Some(v) = &ls.value_opt {
                    self.analyse_expr(v, root)?
                } else {
                    Type::UnitType(Unit)
                };

                // get the type annotation if there is one, otherwise assume the value's type
                let declared_type = match &ls.type_ann_opt {
                    Some(t) => t,
                    None => &value_type,
                };

                // check that the value matches the type annotation
                if &value_type != declared_type {
                    return Err(SemanticErrorKind::TypeMismatchDeclaredType {
                        actual_type: format!("`{}`", value_type),
                        declared_type: format!("`{}`", declared_type),
                    });
                }

                let assignee_path = PathType::from(ls.assignee.name.clone());

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
                        data: ls.value_opt.clone(),
                    },
                    Type::UserDefined(pt) => {
                        let type_path = build_item_path(root, pt.clone());

                        match self.lookup(&type_path) {
                            Some(s) => s.clone(),
                            None => Symbol::Variable {
                                name: ls.assignee.name.clone(),
                                var_type: value_type,
                                data: ls.value_opt.clone(),
                            },
                        }
                    }
                };

                // add the variable to the symbol table
                self.insert(assignee_path, symbol)?;
            }

            Statement::Item(i) => match i {
                Item::ImportDecl(id) => {
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    self.analyse_import(&id, root)?
                }

                Item::AliasDecl(ad) => {
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let alias_path = build_item_path(root, PathType::from(ad.alias_name.clone()));

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
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let value_type = match &cd.value_opt {
                        Some(v) => {
                            let value = wrap_into_expression(v.clone());
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
                        build_item_path(root, PathType::from(cd.constant_name.clone()));

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
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let assignee_type = match &s.assignee_opt {
                        Some(a) => {
                            let assignee = wrap_into_expression(*a.clone());
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

                    let static_var_path = build_item_path(root, PathType::from(s.var_name.clone()));

                    self.insert(
                        static_var_path,
                        Symbol::Variable {
                            name: s.var_name.clone(),
                            var_type: s.var_type.clone(),
                            data: {
                                if s.assignee_opt.is_some() {
                                    Some(Expression::from(*s.assignee_opt.clone().unwrap_or(
                                        Box::new(AssigneeExpr::UnderscoreExpr(UnderscoreExpr {
                                            underscore: Identifier::from("_"),
                                            span: Span::new("", 0, 0),
                                        })),
                                    )))
                                } else {
                                    None
                                }
                            },
                        },
                    )?;
                }

                Item::ModuleItem(m) => {
                    // TODO: sort out visibility
                    let module_path = build_item_path(root, PathType::from(m.module_name.clone()));

                    let scope_kind = ScopeKind::Module(module_path.to_string());

                    let mut module_scope = Scope {
                        scope_kind: scope_kind.clone(),
                        symbols: HashMap::new(),
                    };

                    self.enter_scope(scope_kind);

                    if let Some(v) = &m.items_opt {
                        for item in v.iter() {
                            self.analyse_stmt(&Statement::Item(item.clone()), &module_path)?;
                        }
                    }

                    if let Some(curr_scope) = self.scope_stack.pop() {
                        self.logger
                            .debug(&format!("exiting scope: `{:?}`", curr_scope.scope_kind));

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

                    self.logger.debug(&format!(
                        "inserting symbols into module at path: `{}`",
                        module_path
                    ));

                    self.module_registry
                        .insert(module_path, module_scope.symbols);
                }

                Item::TraitDef(t) => {
                    let trait_name_path = PathType::from(t.trait_name.clone());

                    let full_path = build_item_path(root, trait_name_path.clone());

                    self.insert(
                        full_path.clone(),
                        Symbol::Trait {
                            path: trait_name_path,
                            trait_def: t.clone(),
                        },
                    )?;

                    if let Some(v) = &t.trait_items_opt {
                        for item in v.iter() {
                            match item {
                                TraitDefItem::AliasDecl(ad) => {
                                    self.analyse_stmt(
                                        &Statement::Item(Item::AliasDecl(ad.clone())),
                                        &full_path,
                                    )?;
                                }

                                TraitDefItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                    &full_path,
                                )?,

                                TraitDefItem::FunctionItem(fi) => {
                                    let function_name_path =
                                        PathType::from(fi.function_name.clone());

                                    let function_def_path =
                                        build_item_path(&full_path, function_name_path.clone());

                                    self.insert(
                                        function_def_path,
                                        Symbol::Function {
                                            path: function_name_path,
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(fi, &full_path, true, false)?;
                                }
                            }
                        }
                    }
                }

                Item::EnumDef(e) => {
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let enum_name_path = PathType::from(e.enum_name.clone());

                    let enum_def_path = build_item_path(root, enum_name_path.clone());

                    self.insert(
                        enum_def_path,
                        Symbol::Enum {
                            path: enum_name_path,
                            enum_def: e.clone(),
                        },
                    )?;
                }

                Item::StructDef(s) => {
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let struct_name_path = PathType::from(s.struct_name.clone());

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
                    self.logger
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let struct_name_path = PathType::from(ts.struct_name.clone());

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

                    if let Some(v) = &i.associated_items_opt {
                        for item in v.iter() {
                            match item {
                                InherentImplItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                    &type_path,
                                )?,

                                InherentImplItem::FunctionItem(fi) => {
                                    let function_path = build_item_path(
                                        &type_path,
                                        PathType::from(fi.function_name.clone()),
                                    );

                                    self.insert(
                                        function_path.clone(),
                                        Symbol::Function {
                                            path: function_path,
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
                        Type::UserDefined(pt) => build_item_path(
                            &build_item_path(root, pt.clone()),
                            PathType::from(t.implemented_trait_path.type_name.clone()),
                        ),
                        ty => {
                            return Err(SemanticErrorKind::UnexpectedType {
                                expected: "user-defined type".to_string(),
                                found: format!("`{}`", ty),
                            })
                        }
                    };

                    if let Some(v) = &t.associated_items_opt {
                        for item in v.iter() {
                            match item {
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
                                        PathType::from(fi.function_name.clone()),
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
                        .debug(&format!("analysing statement: `{}` ...", statement));

                    let function_name_path = PathType::from(f.function_name.clone());

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
                self.logger.debug(&format!(
                    "analysing expression statement: `{}` ...",
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
        path: &PathType,
        is_associated_func: bool,
        is_trait_impl: bool,
    ) -> Result<(), SemanticErrorKind> {
        // What is the root path? [i.e., path to the function, excluding the function name]
        // Options:
        // * module (e.g., `lib::some_module` or just `lib`)
        // * object implementation (e.g., `lib::some_module::SomeObject`)
        // * trait implementation (e.g., `lib::some_module::SomeObject::SomeTrait`)

        let function_root = if is_trait_impl {
            if let Some(prefix) = &path.associated_type_path_prefix_opt {
                PathType::from(prefix.clone())
            } else {
                path.clone()
            }
        } else {
            path.clone()
        };

        println!("function root: {}", function_root);

        // append the function name to the root
        let full_path = build_item_path(&function_root, PathType::from(f.function_name.clone()));

        self.enter_scope(ScopeKind::Function(full_path.to_string()));

        if let Some(v) = &f.params_opt {
            let param_types: Vec<Type> = v.iter().map(|p| p.param_type()).collect();

            for (param, mut param_type) in v.iter().zip(param_types) {
                if param_type == Type::SelfType(SelfType) {
                    if is_associated_func {
                        param_type = Type::UserDefined(function_root.clone());
                    }
                }

                let param_path = PathType::from(param.param_name());

                self.insert(
                    param_path,
                    Symbol::Variable {
                        name: param.param_name(),
                        var_type: param_type,
                        data: {
                            if f.block_opt.is_some() {
                                Some(Expression::Block(f.block_opt.clone().unwrap()))
                            } else {
                                None
                            }
                        },
                    },
                )?;
            }
        }

        let function_type = if let Some(b) = &f.block_opt {
            // split path into a vector to remove the last element (if needed), then convert back
            let mut path_vec = Vec::<Identifier>::from(path.clone());

            if is_associated_func {
                path_vec.pop(); // remove associated type name
            }

            self.analyse_expr(&Expression::Block(b.clone()), &PathType::from(path_vec))?
        } else {
            if let Some(t) = &f.return_type_opt {
                *t.clone()
            } else {
                Type::UnitType(Unit)
            }
        };

        // check that the function type matches the return type
        if let Some(rt) = &f.return_type_opt {
            if function_type != *rt.clone() {
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
                            expected: format!("`{}`", rt),
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
        module_root: &PathType,
    ) -> Result<(), SemanticErrorKind> {
        let mut paths: Vec<PathType> = Vec::new();

        let mut segments = import_decl.import_tree.path_segments.clone();

        let mut import_root = if !segments.is_empty() {
            let mut paths = Vec::<PathType>::from(segments.remove(0));

            if !paths.is_empty() {
                let mut path = paths.remove(0);

                for p in paths {
                    path = build_item_path(&path, p);
                }

                path
            } else {
                PathType::from(Identifier::from(""))
            }
        } else {
            module_root.clone()
        };

        let mut segment_counter: usize = 0;
        let num_segments = segments.len();

        for seg in segments {
            segment_counter += 1;

            let path = build_item_path(&import_root, seg.root);

            if let Some(sub) = seg.subset_opt {
                import_root = path.clone();

                for t in sub.nested_trees {
                    for ps in t.path_segments {
                        for p in Vec::<PathType>::from(ps) {
                            let path = build_item_path(&path.clone(), p);
                            paths.push(path);
                        }
                    }
                }
            } else {
                if segment_counter == num_segments {
                    paths.push(path.clone());
                }

                if segment_counter < num_segments {
                    import_root = path.clone();
                }
            }
        }

        // TODO: handle `super` and `self` path roots
        // TODO: handle public imports / re-exports

        // TODO: how to insert associated items (e.g., constructors, other functions) when importing
        // TODO: from another module ?

        for p in paths {
            if let Some(m) = self.module_registry.get(&import_root) {
                if let Some(s) = m.get(&p) {
                    self.insert(PathType::from(p.type_name), s.clone())?;
                } else {
                    return Err(SemanticErrorKind::UndefinedSymbol {
                        name: p.to_string(),
                    });
                }
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
        root: &PathType,
    ) -> Result<Type, SemanticErrorKind> {
        match expression {
            Expression::Path(p) => {
                let path = match p.tree_opt.clone() {
                    Some(mut v) => {
                        if let Some(id) = v.pop() {
                            build_item_path(
                                root,
                                PathType {
                                    associated_type_path_prefix_opt: Some(v.to_vec()),
                                    type_name: id,
                                },
                            )
                        } else {
                            match &p.path_root {
                                PathRoot::Identifier(i) => {
                                    build_item_path(root, PathType::from(i.clone()))
                                }

                                PathRoot::SelfType(_) => root.clone(),

                                PathRoot::SelfKeyword => root.clone(),

                                pr => {
                                    return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                        name: Identifier::from(&pr.to_string()),
                                    })
                                }
                            }
                        }
                    }

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => PathType::from(i.clone()),

                        PathRoot::SelfType(_) => root.clone(),

                        PathRoot::SelfKeyword => root.clone(),

                        pr => {
                            return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                name: Identifier::from(&pr.to_string()),
                            })
                        }
                    },
                };

                println!("variable path: {}", path);

                if let Some(s) = self.lookup(&path) {
                    println!("variable symbol: {}", s);
                    Ok(s.symbol_type())
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
                    crate::ast::Hash::H160(_) => {
                        Ok(Type::H160(crate::ast::Hash::H160(H160::default())))
                    }
                    crate::ast::Hash::H256(_) => {
                        Ok(Type::H256(crate::ast::Hash::H256(H256::default())))
                    }
                    crate::ast::Hash::H512(_) => {
                        Ok(Type::H512(crate::ast::Hash::H512(H512::default())))
                    }
                },

                Literal::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),

                Literal::Char { .. } => Ok(Type::Char(Char::from(char::default()))),

                Literal::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
            },

            Expression::MethodCall(mc) => {
                // TODO: what happens when we call object-specific methods like `get()` (mapping) ?
                // TODO: these should be in the standard library, but can we access them from
                // TODO: here (i.e., in analysis, before compilation) ?
                // TODO: we need to find a way to interpret a `get()` call correctly and apply it
                // TODO: to an existing mapping in the symbol tree, in order to type-check, as one
                // TODO: would with index and tuple index expressions before further processing
                let receiver = wrap_into_expression(*mc.receiver.clone());

                let receiver_type = self.analyse_expr(&receiver, root)?;

                // convert receiver expression to path expression (i.e., check if receiver
                // is a valid path)
                let receiver_as_path_expr = PathExpr::from(receiver);

                // get path expression's type
                let receiver_path = PathType::from(receiver_as_path_expr);

                // check if path expression's type is that of an existing type and analyse
                match self.lookup(&receiver_path).cloned() {
                    Some(Symbol::Struct { path, .. } | Symbol::TupleStruct { path, .. }) => {
                        if path == receiver_path {
                            let method_path =
                                build_item_path(&path, PathType::from(mc.method_name.clone()));
                            self.analyse_call_or_method_call_expr(method_path, mc.args_opt.clone())
                        } else {
                            Err(SemanticErrorKind::TypeMismatchVariable {
                                name: receiver_path.type_name,
                                expected: path.to_string(),
                                found: format!("`{}`", receiver_type),
                            })
                        }
                    }

                    Some(Symbol::Variable { var_type, data, .. }) => {
                        if let Some(d) = data {
                            match var_type {
                                Type::Vec { .. } => match d {
                                    Expression::Array(a) => self.analyse_vec_method(mc, &a, root),
                                    _ => Err(SemanticErrorKind::UnexpectedType {
                                        expected: "array".to_string(),
                                        found: self.analyse_expr(&d, root)?.to_string(),
                                    }),
                                },
                                Type::Mapping { .. } => match d {
                                    Expression::Mapping(m) => {
                                        self.analyse_expr(&Expression::Mapping(m.clone()), root)?;

                                        self.analyse_mapping_method(mc, m.to_hashmap(), root)
                                    }
                                    _ => Err(SemanticErrorKind::UnexpectedType {
                                        expected: "`Mapping`".to_string(),
                                        found: self.analyse_expr(&d, root)?.to_string(),
                                    }),
                                },
                                Type::Option { .. } => match d {
                                    Expression::SomeExpr(s) => {
                                        self.analyse_option_method(mc, &s, root)
                                    }

                                    Expression::NoneExpr(_) => Ok(Type::Option {
                                        inner_type: Box::new(Type::UnitType(Unit)),
                                    }),

                                    _ => Err(SemanticErrorKind::UnexpectedType {
                                        expected: "`Option`".to_string(),
                                        found: self.analyse_expr(&d, root)?.to_string(),
                                    }),
                                },
                                Type::Result { .. } => match d {
                                    Expression::ResultExpr(r) => {
                                        self.analyse_result_method(mc, &r, root)
                                    }
                                    _ => Err(SemanticErrorKind::UnexpectedType {
                                        expected: "`Result`".to_string(),
                                        found: self.analyse_expr(&d, root)?.to_string(),
                                    }),
                                },
                                _ => Err(SemanticErrorKind::UnexpectedType {
                                    expected: "`Vec`, `Mapping, `Option` or `Result`".to_string(),
                                    found: self.analyse_expr(&d, root)?.to_string(),
                                }),
                            }
                        } else {
                            Err(SemanticErrorKind::MissingValue {
                                expected: "variable data".to_string(),
                            })
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedType {
                        name: receiver_path.type_name,
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: receiver_path.type_name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::FieldAccess(fa) => {
                let object = wrap_into_expression(*fa.object.clone());

                // convert object to path expression (i.e., check if object
                // is a valid path)
                let object_as_path_expr = PathExpr::from(object.clone());

                // get path expression's type
                let object_path = PathType::from(object_as_path_expr);

                let object_type = self.analyse_expr(&object, &object_path)?;

                println!("object path: {:?}", object_path);

                let symbol = self.lookup(&object_path).cloned();

                println!("object symbol: {:?}", symbol);

                match symbol {
                    Some(Symbol::Struct { struct_def, .. }) => match &struct_def.fields_opt {
                        Some(v) => match v.iter().find(|f| f.field_name == fa.field_name) {
                            Some(sdf) => Ok(*sdf.field_type.clone()),
                            _ => Err(SemanticErrorKind::UndefinedField {
                                struct_name: struct_def.struct_name,
                                field_name: fa.field_name.clone(),
                            }),
                        },
                        None => Ok(Type::UnitType(Unit)),
                    },

                    Some(Symbol::Variable { name, var_type, .. }) => {
                        if name == Identifier::from("self") {
                            if let Some(Symbol::Struct { struct_def, .. }) =
                                self.lookup(&PathType::from(var_type))
                            {
                                match &struct_def.fields_opt {
                                    Some(v) => {
                                        match v.iter().find(|f| f.field_name == fa.field_name) {
                                            Some(sdf) => Ok(*sdf.field_type.clone()),
                                            _ => Err(SemanticErrorKind::UndefinedField {
                                                struct_name: struct_def.struct_name.clone(),
                                                field_name: fa.field_name.clone(),
                                            }),
                                        }
                                    }
                                    None => Ok(Type::UnitType(Unit)),
                                }
                            } else {
                                Err(SemanticErrorKind::UnexpectedSymbol {
                                    name: Identifier::from(&format!("{}", object_type)),
                                    expected: "struct".to_string(),
                                    found: format!("`{}`", name),
                                })
                            }
                        } else {
                            Err(SemanticErrorKind::UnexpectedSymbol {
                                name: Identifier::from(&format!("{}", object_type)),
                                expected: "struct".to_string(),
                                found: format!("`{}`", name),
                            })
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedType {
                        name: Identifier::from(&format!("{}", object_type)),
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: Identifier::from(&format!("{}", object_type)),
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::Call(c) => {
                let callee = wrap_into_expression(c.callee.clone());

                let callee_path = PathType::from(PathExpr::from(callee));

                // !! throws an error if the call path is from another module (i.e., not the root)
                let callee_path = if self.lookup(&callee_path).is_some() {
                    callee_path
                } else {
                    build_item_path(root, callee_path)
                };

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
                            expected: "unsigned integer".to_string(),
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
                    Type::Tuple(t) => {
                        if ti.index < UInt::from(t.len()) {
                            Ok(Type::Tuple(t))
                        } else {
                            Err(SemanticErrorKind::TupleIndexOutOfBounds {
                                len: ti.index,
                                i: UInt::from(t.len()),
                            })
                        }
                    }
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "tuple".to_string(),
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
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::I64(_), Type::I32(_) | Type::I64(_)) => {
                        Ok(Type::I64(Int::I64(i64::default())))
                    }

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64` or `i32`".to_string(),
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
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: format!("`{}`", t),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
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

                    (Type::U512(_), Type::U512(_)) => {
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
                (None, None) => Ok(Type::UnitType(Unit)),
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

                let assignee_path = PathType::from(assignee_as_path_expr);

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
                        return Err(SemanticErrorKind::ConstantReassignment {
                            name: constant_name,
                        })
                    }

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: assignee_path.type_name,
                        expected: format!("`{}`", assignee_type),
                        found: s.to_string(),
                    }),
                    None => Err(SemanticErrorKind::UndefinedVariable {
                        name: assignee_path.type_name,
                    }),
                }
            }

            Expression::CompoundAssignment(ca) => {
                let assignee = wrap_into_expression(ca.lhs.clone());

                let assignee_type = self.analyse_expr(&assignee, root)?;

                let value_type = self.analyse_expr(&wrap_into_expression(ca.rhs.clone()), root)?;

                let assignee_as_path_expr = PathExpr::from(assignee);

                let assignee_path = PathType::from(assignee_as_path_expr);

                match self.lookup(&assignee_path) {
                    Some(Symbol::Variable { .. }) => match (&assignee_type, &value_type) {
                        (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                        (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`i32`".to_string(),
                            found: format!("`{}`", t),
                        }),

                        (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                        (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                            expected: "`i64`".to_string(),
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

                        (Type::U512(_), Type::U512(_)) => {
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
                            found: format!(
                                "`{} {} {}`",
                                &assignee_type, ca.compound_assignment_op, &value_type
                            ),
                        }),
                    },
                    Some(Symbol::Constant { constant_name, .. }) => {
                        return Err(SemanticErrorKind::ConstantReassignment {
                            name: constant_name.clone(),
                        })
                    }

                    Some(s) => {
                        return Err(SemanticErrorKind::UnexpectedSymbol {
                            name: assignee_path.type_name,
                            expected: format!("`{}`", assignee_type),
                            found: s.to_string(),
                        })
                    }
                    None => {
                        return Err(SemanticErrorKind::UndefinedVariable {
                            name: assignee_path.type_name,
                        })
                    }
                }
            }

            Expression::Return(r) => match &r.expression_opt {
                Some(e) => self.analyse_expr(&e.clone(), root),
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Break(_) => Ok(Type::UnitType(Unit)),

            Expression::Continue(_) => Ok(Type::UnitType(Unit)),

            Expression::Underscore(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from("_"),
            })),

            Expression::Closure(c) => {
                let params_opt = match &c.closure_params {
                    ClosureParams::Some(v) => {
                        let mut function_params: Vec<FunctionOrMethodParam> = Vec::new();

                        for cp in v {
                            let param_type =
                                cp.type_ann_opt
                                    .clone()
                                    .unwrap_or(Box::new(Type::InferredType(InferredType {
                                        name: Identifier::from("_"),
                                    })));

                            let function_param = FunctionParam {
                                param_name: cp.param_name.clone(),
                                param_type,
                            };

                            function_params
                                .push(FunctionOrMethodParam::FunctionParam(function_param))
                        }

                        Some(function_params)
                    }
                    ClosureParams::None => None,
                };

                self.enter_scope(ScopeKind::Function(
                    PathType::from(Identifier::from("")).to_string(),
                ));

                if let Some(v) = &params_opt {
                    let param_types: Vec<Type> = v.iter().map(|p| p.param_type()).collect();

                    for (param, param_type) in v.iter().zip(param_types) {
                        let param_path = PathType::from(param.param_name());

                        self.insert(
                            param_path,
                            Symbol::Variable {
                                name: param.param_name(),
                                var_type: param_type,
                                data: Some(Expression::Closure(c.clone())),
                            },
                        )?;
                    }
                }

                let return_type = match &c.return_type_opt {
                    Some(t) => Ok(*t.clone()),
                    None => Ok(Type::UnitType(Unit)),
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
                    function_name: Identifier::from(""),
                    params_opt,
                    return_type_opt: Some(Box::new(return_type)),
                };

                Ok(Type::FunctionPtr(function_ptr))
            }

            Expression::Array(a) => match &a.elements_opt {
                Some(v) => match v.first() {
                    Some(expr) => {
                        let mut element_count = 0u64;

                        let first_element_type = self.analyse_expr(expr, root)?;

                        element_count += 1;

                        for elem in v.iter().skip(1) {
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

                    None => {
                        let element_type = Type::UnitType(Unit);
                        let array = Type::Array {
                            element_type: Box::new(element_type),
                            num_elements: UInt::U64(0u64),
                        };

                        Ok(array)
                    }
                },
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Tuple(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for expr in t.tuple_elements.elements.iter() {
                    let ty = self.analyse_expr(expr, root)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Expression::Struct(s) => {
                let path_type = build_item_path(root, PathType::from(s.struct_path.clone()));

                println!("struct path: {}", path_type);

                match self.lookup(&path_type).cloned() {
                    Some(Symbol::Struct { struct_def, path }) => {
                        self.insert(
                            PathType::from(s.struct_path.clone()),
                            Symbol::Struct {
                                path: path.clone(),
                                struct_def: struct_def.clone(),
                            },
                        )?;

                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for sf in &struct_fields.unwrap() {
                                let field_name = sf.field_name.clone();
                                let field_value = *sf.field_value.clone();
                                let field_type = self.analyse_expr(&field_value, root)?;
                                field_map.insert(field_name, field_type);
                            }
                        }

                        if struct_def.fields_opt.is_some() {
                            for sdf in &struct_def.fields_opt.unwrap() {
                                match field_map.get(&sdf.field_name) {
                                    Some(expr_type) if *expr_type == *sdf.field_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: path.type_name,
                                            expected: format!("`{}`", *sdf.field_type),
                                            found: format!("`{}`", t),
                                        })
                                    }
                                    None => {
                                        return Err(SemanticErrorKind::MissingStructField {
                                            expected: format!(
                                                "`{}: {}`",
                                                &sdf.field_name, *sdf.field_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: path_type.type_name,
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: path_type.type_name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::Mapping(m) => match &m.pairs_opt {
                Some(v) => match v.first() {
                    Some(p) => {
                        let key_type = self.analyse_patt(&p.key.clone())?;

                        let value_type = self.analyse_expr(&p.value.clone(), root)?;

                        for pair in v.iter().skip(1) {
                            let pair_key_type = self.analyse_patt(&pair.key.clone())?;

                            let pair_value_type = self.analyse_expr(&pair.value.clone(), root)?;

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

                    None => {
                        let key_type = Box::new(Type::UnitType(Unit));
                        let value_type = Box::new(Type::UnitType(Unit));

                        Ok(Type::Mapping {
                            key_type,
                            value_type,
                        })
                    }
                },
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Block(b) => match &b.statements_opt {
                Some(vec) => {
                    self.enter_scope(ScopeKind::LocalBlock);

                    for stmt in vec {
                        match stmt {
                            Statement::Expression(e) => match e {
                                Expression::Return(_)
                                | Expression::Break(_)
                                | Expression::Continue(_) => {
                                    if vec.into_iter().next().is_some() {
                                        self.logger.warn("unreachable code")
                                    }
                                }
                                _ => {
                                    self.analyse_expr(e, root)?;
                                }
                            },
                            _ => {
                                self.analyse_stmt(stmt, root)?;
                            }
                        }
                    }

                    let ty = match vec.last() {
                        Some(s) => match s {
                            Statement::Expression(e) => self.analyse_expr(e, root)?,

                            _ => Type::UnitType(Unit),
                        },
                        None => Type::UnitType(Unit),
                    };

                    self.exit_scope();

                    Ok(ty)
                }

                None => Ok(Type::UnitType(Unit)),
            },

            Expression::If(i) => {
                self.analyse_expr(&Expression::Grouped(*i.condition.clone()), root)?;

                let if_block_type =
                    self.analyse_expr(&Expression::Block(*i.if_block.clone()), root)?;

                let else_if_blocks_type = match &i.else_if_blocks_opt {
                    Some(v) => match v.first() {
                        Some(_) => {
                            for block in v.iter() {
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
                        None => Type::UnitType(Unit),
                    },
                    None => Type::UnitType(Unit),
                };

                let trailing_else_block_type = match &i.trailing_else_block_opt {
                    Some(b) => self.analyse_expr(&Expression::Block(b.clone()), root)?,
                    None => Type::UnitType(Unit),
                };

                if else_if_blocks_type != if_block_type {
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
                    return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                        loc: "scrutinee and matched pattern".to_string(),
                        expected: format!("`{}`", scrutinee_type),
                        found: format!("`{}`", patt_type),
                    });
                }

                let expr_type = self.analyse_expr(&m.final_arm.arm_expression.clone(), root)?;

                if let Some(v) = &m.match_arms_opt {
                    for arm in v.iter() {
                        let arm_patt_type = self.analyse_patt(&arm.matched_pattern)?;

                        if arm_patt_type != patt_type {
                            return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                loc: "matched pattern".to_string(),
                                expected: format!("`{}`", patt_type),
                                found: format!("`{}`", arm_patt_type),
                            });
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

                self.analyse_patt(&fi.pattern.clone())?;
                self.analyse_expr(&fi.iterator.clone(), root)?;

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

                let ty = if ty == Type::Tuple([].to_vec()) {
                    Type::UnitType(Unit)
                } else {
                    ty
                };

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Expression::NoneExpr(_) => Ok(Type::Option {
                inner_type: Box::new(Type::UnitType(Unit)),
            }),

            Expression::ResultExpr(r) => {
                let ty = self.analyse_expr(&r.expression.clone().inner_expression, root)?;

                let ty = if ty == Type::Tuple([].to_vec()) {
                    Type::UnitType(Unit)
                } else {
                    ty
                };

                println!("result type: {:?}", ty);

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
                    k => Err(SemanticErrorKind::UnexpectedKeyword {
                        expected: "`Ok` or `Err`".to_string(),
                        found: format!("`{}`", k),
                    }),
                }
            }
        }
    }

    fn analyse_call_or_method_call_expr(
        &mut self,
        path: PathType,
        args_opt: Option<Vec<Expression>>,
    ) -> Result<Type, SemanticErrorKind> {
        match self.lookup(&path) {
            Some(Symbol::Function { function, .. }) => {
                let params = function.params_opt.clone();
                let return_type = function.return_type_opt.clone();

                match (&args_opt, &params) {
                    (None, None) => match return_type {
                        Some(t) => Ok(*t),
                        None => Ok(Type::UnitType(Unit)),
                    },
                    (None, Some(_)) | (Some(_), None) => {
                        return Err(SemanticErrorKind::ArgumentCountMismatch {
                            name: path.type_name,
                            expected: params.unwrap_or(Vec::new()).len(),
                            found: args_opt.unwrap_or(Vec::new()).len(),
                        })
                    }
                    (Some(a), Some(p)) => {
                        if a.len() != p.len() {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                name: path.type_name,
                                expected: p.len(),
                                found: a.len(),
                            });
                        }

                        for (arg, param) in a.iter().zip(p) {
                            let arg_type =
                                self.analyse_expr(&arg, &PathType::from(Identifier::from("")))?;
                            let param_type = param.param_type();
                            if arg_type != param_type {
                                return Err(SemanticErrorKind::TypeMismatchArgument {
                                    name: path.type_name,
                                    expected: format!("`{}`", param_type),
                                    found: format!("`{}`", arg_type),
                                });
                            }
                        }

                        match return_type {
                            Some(t) => Ok(*t),
                            None => Ok(Type::UnitType(Unit)),
                        }
                    }
                }
            }

            None => Err(SemanticErrorKind::UndefinedFunction {
                name: path.type_name,
            }),

            Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                name: path.type_name,
                expected: "function".to_string(),
                found: s.to_string(),
            }),
        }
    }

    fn analyse_vec_method(
        &mut self,
        method_call_expr: &MethodCallExpr,
        vec: &ArrayExpr,
        root: &PathType,
    ) -> Result<Type, SemanticErrorKind> {
        self.analyse_expr(&Expression::MethodCall(method_call_expr.clone()), root)?;

        if Identifier::from("get") == method_call_expr.method_name
            || Identifier::from("remove") == method_call_expr.method_name
        {
            if let Some(a) = &method_call_expr.args_opt {
                if a.len() == 1 {
                    let expr = a.get(0).cloned().unwrap_or(Expression::NoneExpr(NoneExpr {
                        kw_none: Keyword::None,
                        span: Span::new("", 0, 0),
                    }));

                    let index = if let Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) =
                        self.analyse_expr(&expr, root)?
                    {
                        match expr {
                            Expression::Literal(ref l) => match l {
                                Literal::UInt { value, .. } => value,
                                _ => {
                                    return Err(SemanticErrorKind::TypeMismatchValues {
                                        expected: "unsigned integer".to_string(),
                                        found: self.analyse_expr(&expr, root)?.to_string(),
                                    })
                                }
                            },
                            _ => {
                                return Err(SemanticErrorKind::UnexpectedExpression {
                                    expected: "unsigned integer literal expression".to_string(),
                                    found: expr.to_string(),
                                })
                            }
                        }
                    } else {
                        return Err(SemanticErrorKind::TypeMismatchValues {
                            expected: "unsigned integer".to_string(),
                            found: self.analyse_expr(&expr, root)?.to_string(),
                        });
                    };

                    let array = if let Some(v) = vec.elements_opt.clone() {
                        v
                    } else {
                        Vec::new()
                    };

                    if let Some(e) = array.get(index.to_usize()) {
                        if method_call_expr.method_name == Identifier::from("remove") {
                            self.analyse_expr(e, root)
                        } else {
                            Ok(Type::Option {
                                inner_type: Box::new(self.analyse_expr(e, root)?),
                            })
                        }
                    } else {
                        Err(SemanticErrorKind::MissingValue {
                            expected: format!("element at index: {}", index),
                        })
                    }
                } else {
                    Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: method_call_expr.method_name.clone(),
                        expected: 1,
                        found: a.len(),
                    })
                }
            } else {
                Err(SemanticErrorKind::MissingValue {
                    expected: "function argument".to_string(),
                })
            }
        } else if Identifier::from("pop") == method_call_expr.method_name
            || Identifier::from("last") == method_call_expr.method_name
        {
            if let Some(a) = &method_call_expr.args_opt {
                return Err(SemanticErrorKind::ArgumentCountMismatch {
                    name: method_call_expr.method_name.clone(),
                    expected: 0,
                    found: a.len(),
                });
            }

            if let Some(v) = &vec.elements_opt {
                if let Some(e) = v.last() {
                    Ok(Type::Option {
                        inner_type: Box::new(self.analyse_expr(e, root)?),
                    })
                } else {
                    Err(SemanticErrorKind::MissingValue {
                        expected: "final array element".to_string(),
                    })
                }
            } else {
                Err(SemanticErrorKind::MissingValue {
                    expected: "array elements".to_string(),
                })
            }
        } else {
            Err(SemanticErrorKind::UndefinedFunction {
                name: method_call_expr.method_name.clone(),
            })
        }
    }

    // type-check values taken / produced by method calls (i.e., return values)
    fn analyse_mapping_method(
        &mut self,
        method_call_expr: &MethodCallExpr,
        mapping: HashMap<Pattern, Expression>,
        root: &PathType,
    ) -> Result<Type, SemanticErrorKind> {
        if Identifier::from("get") == method_call_expr.method_name
            || Identifier::from("remove") == method_call_expr.method_name
        {
            if let Some(a) = &method_call_expr.args_opt {
                if a.len() == 1 {
                    let key = Pattern::try_from(a.get(0).unwrap().clone()).unwrap_or(
                        Pattern::NonePatt(NonePatt {
                            kw_none: Keyword::None,
                        }),
                    );

                    if let Some(e) = mapping.get(&key) {
                        Ok(Type::Option {
                            inner_type: Box::new(self.analyse_expr(e, root)?),
                        })
                    } else {
                        Err(SemanticErrorKind::MissingValue {
                            expected: format!("value to match key: {}", key),
                        })
                    }
                } else {
                    Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: method_call_expr.method_name.clone(),
                        expected: 1,
                        found: a.len(),
                    })
                }
            } else {
                Err(SemanticErrorKind::MissingValue {
                    expected: "key".to_string(),
                })
            }
        } else {
            Err(SemanticErrorKind::UndefinedFunction {
                name: method_call_expr.method_name.clone(),
            })
        }
    }

    fn analyse_option_method(
        &mut self,
        method_call_expr: &MethodCallExpr,
        option: &SomeExpr,
        root: &PathType,
    ) -> Result<Type, SemanticErrorKind> {
        self.analyse_expr(&Expression::MethodCall(method_call_expr.clone()), root)?;

        if Identifier::from("unwrap") == method_call_expr.method_name {
            if let Some(a) = &method_call_expr.args_opt {
                return Err(SemanticErrorKind::ArgumentCountMismatch {
                    name: method_call_expr.method_name.clone(),
                    expected: 0,
                    found: a.len(),
                });
            }

            self.analyse_expr(&Expression::SomeExpr(option.clone()), root)
        } else if Identifier::from("expect") == method_call_expr.method_name {
            if let Some(a) = &method_call_expr.args_opt {
                if a.len() == 1 {
                    let expr = a.get(0).cloned().unwrap_or(Expression::NoneExpr(NoneExpr {
                        kw_none: Keyword::None,
                        span: Span::new("", 0, 0),
                    }));

                    if let Type::Str { .. } = self.analyse_expr(&expr, root)? {
                        self.analyse_expr(&Expression::SomeExpr(option.clone()), root)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchArgument {
                            name: Identifier::from("msg"),
                            expected: "str".to_string(),
                            found: self.analyse_expr(&expr, root)?.to_string(),
                        })
                    }
                } else {
                    Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: method_call_expr.method_name.clone(),
                        expected: 1,
                        found: a.len(),
                    })
                }
            } else {
                Err(SemanticErrorKind::MissingValue {
                    expected: "error message".to_string(),
                })
            }
        } else {
            Err(SemanticErrorKind::UndefinedFunction {
                name: method_call_expr.method_name.clone(),
            })
        }
    }

    fn analyse_result_method(
        &mut self,
        method_call_expr: &MethodCallExpr,
        result: &ResultExpr,
        root: &PathType,
    ) -> Result<Type, SemanticErrorKind> {
        self.analyse_expr(&Expression::MethodCall(method_call_expr.clone()), root)?;

        if Identifier::from("unwrap") == method_call_expr.method_name {
            if let Some(a) = &method_call_expr.args_opt {
                return Err(SemanticErrorKind::ArgumentCountMismatch {
                    name: method_call_expr.method_name.clone(),
                    expected: 0,
                    found: a.len(),
                });
            }

            self.analyse_expr(&Expression::ResultExpr(result.clone()), root)
        } else if Identifier::from("expect") == method_call_expr.method_name {
            if let Some(a) = &method_call_expr.args_opt {
                if a.len() == 1 {
                    let expr = a.get(0).cloned().unwrap_or(Expression::NoneExpr(NoneExpr {
                        kw_none: Keyword::None,
                        span: Span::new("", 0, 0),
                    }));

                    if let Type::Str { .. } = self.analyse_expr(&expr, root)? {
                        self.analyse_expr(&Expression::ResultExpr(result.clone()), root)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchArgument {
                            name: Identifier::from("msg"),
                            expected: "str".to_string(),
                            found: self.analyse_expr(&expr, root)?.to_string(),
                        })
                    }
                } else {
                    Err(SemanticErrorKind::ArgumentCountMismatch {
                        name: method_call_expr.method_name.clone(),
                        expected: 1,
                        found: a.len(),
                    })
                }
            } else {
                Err(SemanticErrorKind::MissingValue {
                    expected: "error message".to_string(),
                })
            }
        } else {
            Err(SemanticErrorKind::UndefinedFunction {
                name: method_call_expr.method_name.clone(),
            })
        }
    }

    fn analyse_patt(&mut self, pattern: &Pattern) -> Result<Type, SemanticErrorKind> {
        match pattern {
            Pattern::IdentifierPatt(i) => match self.lookup(&PathType::from(i.name.clone())) {
                Some(Symbol::Variable { var_type, .. }) => Ok(var_type.clone()),
                Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                    name: i.name.clone(),
                    expected: "variable".to_string(),
                    found: s.to_string(),
                }),
                None => Err(SemanticErrorKind::UndefinedVariable {
                    name: i.name.clone(),
                }),
            },

            Pattern::PathPatt(p) => {
                let path = PathType::from(p.clone());

                if let Some(s) = self.lookup(&path) {
                    Ok(s.symbol_type())
                } else {
                    Err(SemanticErrorKind::UndefinedVariable {
                        name: path.type_name,
                    })
                }
            }

            Pattern::Literal(l) => match l {
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
                    crate::ast::Hash::H160(_) => {
                        Ok(Type::H160(crate::ast::Hash::H160(H160::default())))
                    }
                    crate::ast::Hash::H256(_) => {
                        Ok(Type::H256(crate::ast::Hash::H256(H256::default())))
                    }
                    crate::ast::Hash::H512(_) => {
                        Ok(Type::H512(crate::ast::Hash::H512(H512::default())))
                    }
                },

                Literal::Str { .. } => Ok(Type::Str(Str::from(String::default().as_str()))),

                Literal::Char { .. } => Ok(Type::Char(Char::from(char::default()))),

                Literal::Bool { .. } => Ok(Type::Bool(Bool::from(bool::default()))),
            },

            Pattern::ReferencePatt(r) => Ok(Type::Reference {
                reference_op: r.reference_op,
                inner_type: Box::new(self.analyse_patt(&r.pattern)?),
            }),

            Pattern::GroupedPatt(g) => self.analyse_patt(&g.inner_pattern),

            Pattern::RangePatt(r) => match (&r.from_pattern_opt, &r.to_pattern_opt) {
                (None, None) => Ok(Type::UnitType(Unit)),
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
                let path_type = PathType::from(s.struct_path.clone());

                match self.lookup(&path_type).cloned() {
                    Some(Symbol::Struct { struct_def, path }) => {
                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for spf in &struct_fields.unwrap() {
                                let field_name = spf.field_name.clone();
                                let field_value = spf.field_value.clone();
                                let field_type = self.analyse_patt(&field_value)?.clone();
                                field_map.insert(field_name, field_type);
                            }
                        }

                        if struct_def.fields_opt.is_some() {
                            for sdf in &struct_def.fields_opt.unwrap() {
                                match field_map.get(&sdf.field_name) {
                                    Some(expr_type) if *expr_type == *sdf.field_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: path.type_name,
                                            expected: format!("`{}`", sdf.field_type),
                                            found: format!("`{}`", t),
                                        })
                                    }
                                    None => {
                                        return Err(SemanticErrorKind::MissingStructField {
                                            expected: format!(
                                                "`{}: {}`",
                                                &sdf.field_name, *sdf.field_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: path_type.type_name,
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: path_type.type_name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Pattern::TupleStructPatt(ts) => {
                let path_type = PathType::from(ts.struct_path.clone());

                match self.lookup(&path_type).cloned() {
                    Some(Symbol::TupleStruct {
                        tuple_struct_def,
                        path,
                    }) => {
                        let mut element_map: HashMap<usize, Type> = HashMap::new();
                        let mut element_counter = 0usize;

                        let tuple_struct_elements = ts.struct_elements_opt.clone();

                        if tuple_struct_elements.is_some() {
                            for tse in &tuple_struct_elements.unwrap() {
                                let element_type = self.analyse_patt(&tse)?.clone();
                                element_map.insert(element_counter, element_type);
                                element_counter += 1;
                            }
                        }

                        if tuple_struct_def.elements_opt.is_some() {
                            for (i, tsde) in
                                tuple_struct_def.elements_opt.unwrap().iter().enumerate()
                            {
                                match element_map.get(&i) {
                                    Some(patt_type) if *patt_type == *tsde.element_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: path.type_name,
                                            expected: format!("`{}`", tsde.element_type),
                                            found: format!("`{}`", t),
                                        })
                                    }
                                    None => {
                                        return Err(SemanticErrorKind::MissingTupleStructElement {
                                            expected: format!(
                                                "`{}.{}: {}`",
                                                &path_type, i, *tsde.element_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        Ok(Type::UserDefined(path))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct {
                        name: path_type.type_name,
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: path_type.type_name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Pattern::WildcardPatt(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from("_"),
            })),

            Pattern::RestPatt(_) => Ok(Type::InferredType(InferredType {
                name: Identifier::from(".."),
            })),

            Pattern::SomePatt(s) => {
                let ty = self.analyse_patt(&s.pattern.clone().inner_pattern)?;

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Pattern::NonePatt(_) => Ok(Type::Option {
                inner_type: Box::new(Type::UnitType(Unit)),
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
                    k => Err(SemanticErrorKind::UnexpectedKeyword {
                        expected: "`Ok` or `Err`".to_string(),
                        found: format!("`{}`", k),
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
