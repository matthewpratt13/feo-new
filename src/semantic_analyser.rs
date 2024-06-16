#![allow(dead_code)]

mod symbol_table;

use core::fmt;
use std::collections::HashMap;

use symbol_table::{Scope, ScopeKind, Symbol, SymbolTable};

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, ClosureParams, Expression, Float, FunctionItem,
        FunctionOrMethodParam, FunctionParam, FunctionPtr, Hash, Identifier, ImportTree,
        InferredType, InherentImplItem, Int, Item, Keyword, Literal, PathExpr, PathRoot, PathType,
        Pattern, Statement, Str, TraitDefItem, TraitImplItem, Type, UInt, UnaryOp, Unit,
    },
    error::{CompilerError, ErrorsEmitted, SemanticErrorKind},
    logger::{LogLevel, Logger},
    parser::Module,
    span::{Span, Spanned},
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

struct SemanticAnalyser {
    scope_stack: Vec<Scope>,
    module_registry: HashMap<Identifier, SymbolTable>,
    errors: Vec<CompilerError<SemanticErrorKind>>,
    logger: Logger,
}

impl SemanticAnalyser {
    fn new(log_level: LogLevel, external_code: Option<SymbolTable>) -> Self {
        let mut global_scope = Scope {
            scope_kind: ScopeKind::Global,
            symbols: HashMap::new(),
        };

        // add external code (e.g., library functions) to the global scope if there is any
        if let Some(ext) = external_code {
            for (id, sym) in ext {
                global_scope.symbols.insert(id, sym);
            }
        }

        SemanticAnalyser {
            scope_stack: vec![global_scope],
            module_registry: HashMap::new(),
            errors: Vec::new(),
            logger: Logger::new(log_level),
        }
    }

    fn enter_scope(&mut self, scope_kind: ScopeKind) {
        self.logger
            .debug(&format!("entering new scope: {:?}...", &scope_kind));

        self.scope_stack.push(Scope {
            scope_kind,
            symbols: HashMap::new(),
        });
    }

    fn exit_scope(&mut self) {
        if let Some(exited_scope) = self.scope_stack.pop() {
            self.logger
                .debug(&format!("exiting scope: {:?}...", exited_scope.scope_kind));
        }
    }

    fn insert(&mut self, name: Identifier, symbol: Symbol) -> Result<(), SemanticErrorKind> {
        if let Some(curr_scope) = self.scope_stack.last_mut() {
            curr_scope.symbols.insert(name.clone(), symbol.clone());

            self.logger.debug(&format!(
                "inserted symbol `{}` into scope: {:?}",
                name, curr_scope.scope_kind
            ));

            Ok(())
        } else {
            Err(SemanticErrorKind::UndefinedScope)
        }
    }

    fn lookup(&mut self, name: &Identifier) -> Option<&Symbol> {
        for scope in self.scope_stack.iter().rev() {
            if let Some(symbol) = scope.symbols.get(name) {
                self.logger.debug(&format!(
                    "found symbol `{}` in scope: {:?}",
                    name, scope.scope_kind
                ));

                return Some(symbol);
            }
        }
        self.logger
            .warn(&format!("symbol `{}` not found in any scope", name));
        None
    }

    fn analyse(&mut self, module: &Module) -> Result<(), ErrorsEmitted> {
        self.logger.info("starting semantic analysis...");

        for s in &module.statements {
            self.analyse_stmt(s).map_err(|e| {
                self.log_error(e, &s.span());
                ErrorsEmitted
            })?
        }

        self.logger
            .info("semantic analysis complete, no errors detected");

        Ok(())
    }

    fn analyse_stmt(&mut self, statement: &Statement) -> Result<(), SemanticErrorKind> {
        self.logger
            .debug(&format!("analysing statement: {:?}...", statement));

        match statement {
            Statement::Let(ls) => {
                let var_type = match &ls.type_ann_opt {
                    Some(t) => t.clone(),
                    None => Type::InferredType(InferredType {
                        name: Identifier::from("_"),
                    }),
                };

                // variables must be declared with a type and are assigned the unit type if not;
                // this prevents uninitialized variable errors
                let value_type = match &ls.value_opt {
                    Some(v) => self.analyse_expr(v)?,
                    None => Type::UnitType(Unit),
                };

                if value_type != var_type {
                    return Err(SemanticErrorKind::TypeMismatchVariable {
                        name: ls.assignee.name.clone(),
                        expected: var_type.to_string(),
                        found: value_type.to_string(),
                    });
                }

                self.insert(ls.assignee.name.clone(), Symbol::Variable(value_type))?;

                self.logger
                    .info(&format!("analysed let statement: {:?}", ls));
            }

            Statement::Item(i) => match i {
                Item::ImportDecl(id) => self.analyse_import(&id.import_tree)?,

                Item::AliasDecl(ad) => match &ad.original_type_opt {
                    Some(t) => {
                        // alias points to an existing type (`t`)
                        self.insert(ad.alias_name.clone(), Symbol::Variable(t.clone()))?;

                        self.logger
                            .info(&format!("initialized alias declaration: {:?}", ad));
                    }
                    None => {
                        let ty = PathType {
                            associated_type_path_prefix_opt: None,
                            type_name: ad.alias_name.clone(),
                        };

                        // alias is its own user-defined opaque type
                        self.insert(
                            ad.alias_name.clone(),
                            Symbol::Variable(Type::UserDefined(ty)),
                        )?;

                        self.logger
                            .info(&format!("initialized alias declaration: {:?}", ad));
                    }
                },

                Item::ConstantDecl(cd) => {
                    let value_type = match &cd.value_opt {
                        Some(v) => {
                            let value = wrap_into_expression(v.clone())?;
                            self.analyse_expr(&value)?
                        }

                        None => Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        }),
                    };

                    let constant_type = *cd.constant_type.clone();

                    if value_type != constant_type {
                        return Err(SemanticErrorKind::TypeMismatchVariable {
                            name: cd.constant_name.clone(),
                            expected: constant_type.to_string(),
                            found: value_type.to_string(),
                        });
                    }

                    self.insert(
                        cd.constant_name.clone(),
                        Symbol::Variable(*cd.constant_type.clone()),
                    )?;

                    self.logger
                        .info(&format!("initialized constant declaration: {:?}", cd));
                }

                Item::StaticVarDecl(s) => {
                    let assignee_type = match &s.assignee_opt {
                        Some(a) => {
                            let assignee = wrap_into_expression(*a.clone())?;
                            self.analyse_expr(&assignee)?
                        }

                        None => Type::InferredType(InferredType {
                            name: Identifier::from("_"),
                        }),
                    };

                    let var_type = s.var_type.clone();

                    if assignee_type != var_type {
                        return Err(SemanticErrorKind::TypeMismatchVariable {
                            name: s.var_name.clone(),
                            expected: var_type.to_string(),
                            found: assignee_type.to_string(),
                        });
                    }

                    self.insert(s.var_name.clone(), Symbol::Variable(s.var_type.clone()))?;

                    self.logger
                        .info(&format!("initialized static variable declaration: {:?}", s));
                }

                Item::ModuleItem(m) => {
                    let mut module_scope = Scope {
                        scope_kind: ScopeKind::Module,
                        symbols: HashMap::new(),
                    };

                    self.enter_scope(ScopeKind::Module);

                    if let Some(v) = &m.items_opt {
                        for item in v.iter() {
                            self.analyse_stmt(&Statement::Item(item.clone()))?;
                        }
                    }

                    if let Some(curr_scope) = self.scope_stack.pop() {
                        module_scope = curr_scope;
                    }

                    self.insert(
                        m.module_name.clone(),
                        Symbol::Module {
                            module: m.clone(),
                            symbols: module_scope.symbols.clone(),
                        },
                    )?;

                    self.module_registry
                        .insert(m.module_name.clone(), module_scope.symbols);

                    self.logger
                        .info(&format!("initialized new module scope: {:?}", m));
                }

                Item::TraitDef(t) => {
                    self.insert(t.trait_name.clone(), Symbol::Trait(t.clone()))?;

                    self.logger
                        .info(&format!("initialized trait definition: {:?}", t));

                    if let Some(v) = &t.trait_items_opt {
                        for item in v.iter() {
                            match item {
                                TraitDefItem::AliasDecl(ad) => self
                                    .analyse_stmt(&Statement::Item(Item::AliasDecl(ad.clone())))?,

                                TraitDefItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,

                                TraitDefItem::FunctionItem(fi) => {
                                    self.analyse_function_def(fi)?;

                                    self.logger.info(
                                        &format!("function definition {:?} acknowledged, but not initialized", fi),
                                    );
                                }
                            }
                        }
                    }
                }

                Item::EnumDef(e) => {
                    self.insert(e.enum_name.clone(), Symbol::Enum(e.clone()))?;
                    self.logger
                        .info(&format!("initialized enum definition: {:?}", e));
                }

                Item::StructDef(s) => {
                    self.insert(s.struct_name.clone(), Symbol::Struct(s.clone()))?;
                    self.logger
                        .info(&format!("initialized struct definition: {:?}", s));
                }

                Item::TupleStructDef(ts) => {
                    self.insert(ts.struct_name.clone(), Symbol::TupleStruct(ts.clone()))?;
                    self.logger
                        .info(&format!("initialized struct definition: {:?}", ts));
                }

                Item::InherentImplDef(i) => {
                    if let Some(v) = &i.associated_items_opt {
                        for item in v.iter() {
                            match item {
                                InherentImplItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,

                                InherentImplItem::FunctionItem(fi) => {
                                    self.insert(
                                        fi.function_name.clone(),
                                        Symbol::Function {
                                            associated_type_opt: Some(i.nominal_type.clone()),
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(&fi)?;

                                    self.logger.info(&format!(
                                        "initialized associated function definition: {:?}",
                                        fi
                                    ));
                                }
                            }
                        }
                    }
                }

                Item::TraitImplDef(t) => {
                    if let Some(v) = &t.associated_items_opt {
                        for item in v.iter() {
                            match item {
                                TraitImplItem::AliasDecl(ad) => self
                                    .analyse_stmt(&Statement::Item(Item::AliasDecl(ad.clone())))?,

                                TraitImplItem::ConstantDecl(cd) => self.analyse_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,

                                TraitImplItem::FunctionItem(fi) => {
                                    self.insert(
                                        fi.function_name.clone(),
                                        Symbol::Function {
                                            associated_type_opt: Some(
                                                t.implemented_trait_path.clone(),
                                            ),
                                            function: fi.clone(),
                                        },
                                    )?;

                                    self.analyse_function_def(&fi)?;

                                    self.logger.info(&format!(
                                        "initialized associated function definition: {:?}",
                                        fi
                                    ));
                                }
                            }
                        }
                    }
                }

                Item::FunctionItem(f) => {
                    self.insert(
                        f.function_name.clone(),
                        Symbol::Function {
                            associated_type_opt: None,
                            function: f.clone(),
                        },
                    )?;

                    self.analyse_function_def(f)?;

                    self.logger
                        .info(&format!("initialized function definition: {:?}", f));
                }
            },

            Statement::Expression(expr) => {
                self.analyse_expr(expr)?;
                self.logger.info(&format!("analysed expression: {}", expr));
            }
        }

        Ok(())
    }

    fn analyse_function_def(&mut self, f: &FunctionItem) -> Result<(), SemanticErrorKind> {
        self.enter_scope(ScopeKind::Function);

        if let Some(v) = &f.params_opt {
            let param_types: Vec<Type> = v.iter().map(|p| p.param_type()).collect();

            for (param, param_type) in v.iter().zip(param_types) {
                self.insert(param.param_name().clone(), Symbol::Variable(param_type))?;
            }
        }

        let expression_type = if let Some(b) = &f.block_opt {
            if let Some(v) = &b.statements_opt {
                for stmt in v {
                    self.analyse_stmt(stmt)?;
                }
            }

            self.analyse_expr(&Expression::Block(b.clone()))?
        } else {
            Type::InferredType(InferredType {
                name: Identifier::from("_"),
            })
        };

        self.exit_scope();

        if let Some(t) = &f.return_type_opt {
            if expression_type != *t.clone() {
                return Err(SemanticErrorKind::TypeMismatchReturnType {
                    expected: t.to_string(),
                    found: expression_type.to_string(),
                });
            }
        }

        Ok(())
    }

    fn analyse_import(&mut self, tree: &ImportTree) -> Result<(), SemanticErrorKind> {
        let mut path: Vec<PathType> = Vec::new();

        for ps in tree.path_segments.iter() {
            path.push(ps.root.clone());
        }

        let name = match path.last() {
            Some(pt) => Identifier::from(&pt.to_string()),
            None => Identifier::from(""),
        };

        if let Some(m) = self.module_registry.get(&name) {
            if let Some(s) = m.get(&name) {
                self.insert(name.clone(), s.clone())?;
            } else {
                return Err(SemanticErrorKind::UndefinedSymbol { name });
            }
        } else {
            return Err(SemanticErrorKind::UndefinedModule { name });
        }

        self.logger
            .info(&format!("import path declared: {:?}", path));

        Ok(())
    }

    fn analyse_expr(&mut self, expression: &Expression) -> Result<Type, SemanticErrorKind> {
        self.logger
            .debug(&format!("analysing expression: {}...", &expression));

        match expression {
            Expression::Path(p) => {
                let name = match &p.tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        None => match &p.path_root {
                            PathRoot::Identifier(i) => i.clone(),

                            PathRoot::SelfType(s) => return Ok(Type::SelfType(s.clone())),

                            pr => {
                                return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                    name: Identifier::from(&pr.to_string()),
                                })
                            }
                        },
                    },

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => i.clone(),

                        PathRoot::SelfType(s) => return Ok(Type::SelfType(s.clone())),

                        pr => {
                            return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                name: Identifier::from(&pr.to_string()),
                            })
                        }
                    },
                };

                match self.lookup(&name) {
                    Some(Symbol::Variable(t)) => Ok(t.clone()),
                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name,
                        expected: "variable".to_string(),
                        found: s.to_string(),
                    }),
                    None => Err(SemanticErrorKind::UndefinedVariable { name }),
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
                let receiver = wrap_into_expression(*mc.receiver.clone())?;

                let receiver_type = self.analyse_expr(&receiver)?;

                // convert receiver expression to path expression (i.e., check if receiver
                // is a valid path)
                let path_expr = PathExpr::try_from(receiver.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: receiver.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                // get path expression's type
                let type_name = PathType::from(path_expr).type_name;

                // check if path expression's type is that of an existing type and analyse
                match self.lookup(&type_name) {
                    Some(Symbol::Struct(s)) => {
                        if s.struct_name == type_name {
                            self.analyse_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            Err(SemanticErrorKind::TypeMismatchVariable {
                                name: type_name,
                                expected: s.struct_name.to_string(),
                                found: receiver_type.to_string(),
                            })
                        }
                    }

                    Some(Symbol::TupleStruct(ts)) => {
                        if ts.struct_name == type_name {
                            self.analyse_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            Err(SemanticErrorKind::TypeMismatchVariable {
                                name: type_name,
                                expected: ts.struct_name.to_string(),
                                found: receiver_type.to_string(),
                            })
                        }
                    }

                    Some(Symbol::Enum(e)) => {
                        if e.enum_name == type_name {
                            self.analyse_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            Err(SemanticErrorKind::TypeMismatchVariable {
                                name: type_name,
                                expected: e.enum_name.to_string(),
                                found: receiver_type.to_string(),
                            })
                        }
                    }

                    None => Err(SemanticErrorKind::UndefinedType { name: type_name }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: type_name,
                        expected: "struct or enum".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::FieldAccess(fa) => {
                let object_type = self.analyse_expr(&wrap_into_expression(*fa.object.clone())?)?;

                match self.lookup(&Identifier::from(&object_type.to_string())) {
                    Some(Symbol::Struct(s)) => match &s.fields_opt {
                        Some(v) => match v.iter().find(|f| f.field_name == fa.field_name) {
                            Some(sdf) => Ok(*sdf.field_type.clone()),
                            _ => Err(SemanticErrorKind::UndefinedField {
                                struct_name: s.struct_name.clone(),
                                field_name: fa.field_name.clone(),
                            }),
                        },
                        None => Ok(Type::UnitType(Unit)),
                    },

                    None => Err(SemanticErrorKind::UndefinedType {
                        name: Identifier::from(&object_type.to_string()),
                    }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: Identifier::from(&object_type.to_string()),
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::Call(c) => {
                let callee = wrap_into_expression(c.callee.clone())?;

                let path_expr = PathExpr::try_from(callee.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: callee.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                let name = PathType::from(path_expr).type_name;

                self.analyse_call_or_method_call_expr(name, c.args_opt.clone())
            }

            Expression::Index(i) => {
                let array_type = self.analyse_expr(&wrap_into_expression(*i.array.clone())?)?;

                let index_type = self.analyse_expr(&wrap_into_expression(*i.index.clone())?)?;

                match index_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_) => (),
                    _ => {
                        return Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric type (excluding large unsigned integers)"
                                .to_string(),
                            found: index_type.to_string(),
                        })
                    }
                }

                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => Err(SemanticErrorKind::UnexpectedType {
                        expected: "array".to_string(),
                        found: array_type.to_string(),
                    }),
                }
            }

            Expression::TupleIndex(ti) => {
                let tuple_type = self.analyse_expr(&wrap_into_expression(*ti.tuple.clone())?)?;

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
                        found: tuple_type.to_string(),
                    }),
                }
            }

            Expression::Unwrap(u) => {
                self.analyse_expr(&wrap_into_expression(*u.value_expr.clone())?)
            }

            Expression::Unary(u) => {
                let expr_type = self.analyse_expr(&wrap_into_expression(*u.value_expr.clone())?)?;

                match u.unary_op {
                    UnaryOp::Negate => match &expr_type {
                        Type::I32(_)
                        | Type::I64(_)
                        | Type::U8(_)
                        | Type::U16(_)
                        | Type::U32(_)
                        | Type::U64(_)
                        | Type::U256(_)
                        | Type::U512(_) => Ok(expr_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "numeric value".to_string(),
                            found: expr_type.to_string(),
                        }),
                    },
                    UnaryOp::Not => match &expr_type {
                        Type::Bool(_) => Ok(expr_type),
                        _ => Err(SemanticErrorKind::UnexpectedType {
                            expected: "boolean".to_string(),
                            found: expr_type.to_string(),
                        }),
                    },
                }
            }

            Expression::Reference(r) => {
                let reference_op = r.reference_op.clone();
                let inner_type = self.analyse_expr(&r.expression)?;

                Ok(Type::Reference {
                    reference_op,
                    inner_type: Box::new(inner_type),
                })
            }

            Expression::Dereference(d) => {
                self.analyse_expr(&wrap_into_expression(d.assignee_expr.clone())?)
            }

            Expression::TypeCast(tc) => {
                let value_type = self.analyse_expr(&wrap_into_expression(*tc.value.clone())?)?;

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
                        from: t.to_string(),
                        to: u.to_string(),
                    }),
                }
            }

            Expression::Binary(b) => {
                let lhs_type = self.analyse_expr(&wrap_into_expression(*b.lhs.clone())?)?;

                let rhs_type = self.analyse_expr(&wrap_into_expression(*b.rhs.clone())?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I32(_) | Type::I64(_)) => {
                        Ok(Type::I64(Int::I64(i64::default())))
                    }

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64` or `i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U8(_) | Type::U16(_)) => {
                        Ok(Type::U16(UInt::U16(u16::default())))
                    }

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16` or `u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U8(_) | Type::U16(_) | Type::U32(_)) => {
                        Ok(Type::U32(UInt::U32(u32::default())))
                    }

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32` or smaller unsigned integer".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_)) => {
                        Ok(Type::U64(UInt::U64(u64::default())))
                    }

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64` or smaller unsigned integer".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U256(_) | Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512` or `u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F32(_) | Type::F64(_)) => {
                        Ok(Type::F64(Float::F64(F64::default())))
                    }

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64` or `f32`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "numeric values with matching types".to_string(),
                        found: format!("`{} {} {}`", &lhs_type, b.binary_op, &rhs_type),
                    }),
                }
            }

            Expression::Comparison(c) => {
                let lhs_type = self.analyse_expr(&wrap_into_expression(c.lhs.clone())?)?;

                let rhs_type = self.analyse_expr(&wrap_into_expression(c.rhs.clone())?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "numeric values with matching types".to_string(),
                        found: format!("`{} {} {}`", &lhs_type, c.comparison_op, &rhs_type),
                    }),
                }
            }

            Expression::Grouped(g) => self.analyse_expr(&g.inner_expression),

            Expression::Range(r) => match (&r.from_expr_opt, &r.to_expr_opt) {
                (None, None) => Ok(Type::UnitType(Unit)),
                (None, Some(to)) => {
                    let to_type = self.analyse_expr(&wrap_into_expression(*to.clone())?)?;

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
                            found: to_type.to_string(),
                        }),
                    }
                }
                (Some(from), None) => {
                    let from_type = self.analyse_expr(&wrap_into_expression(*from.clone())?)?;

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
                            found: from_type.to_string(),
                        }),
                    }
                }
                (Some(from), Some(to)) => {
                    let from_type = self.analyse_expr(&wrap_into_expression(*from.clone())?)?;

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
                                found: from_type.to_string(),
                            })
                        }
                    }

                    let to_type = self.analyse_expr(&wrap_into_expression(*to.clone())?)?;

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
                                found: to_type.to_string(),
                            })
                        }
                    }

                    if from_type == to_type {
                        Ok(to_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchValues {
                            expected: from_type.to_string(),
                            found: to_type.to_string(),
                        })
                    }
                }
            },

            Expression::Assignment(a) => {
                let assignee = wrap_into_expression(a.lhs.clone())?;

                let assignee_path = PathExpr::try_from(assignee.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: assignee.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                let assignee_type = self.analyse_expr(&assignee)?;

                let assignee_name = Identifier::from(&assignee_path.path_root.to_string());

                let value_type = self.analyse_expr(&wrap_into_expression(a.rhs.clone())?)?;

                if value_type != assignee_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: assignee_type.to_string(),
                        found: value_type.to_string(),
                    });
                }

                match self.lookup(&assignee_name).cloned() {
                    Some(Symbol::Variable(var_type)) => match var_type == assignee_type {
                        true => {
                            self.analyse_expr(&wrap_into_expression(a.rhs.clone())?)?;
                            Ok(var_type)
                        }
                        false => Err(SemanticErrorKind::TypeMismatchVariable {
                            name: assignee_name,
                            expected: assignee_type.to_string(),
                            found: var_type.to_string(),
                        }),
                    },

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name: assignee_name,
                        expected: assignee_type.to_string(),
                        found: s.to_string(),
                    }),
                    None => Err(SemanticErrorKind::UndefinedVariable {
                        name: assignee_name,
                    }),
                }
            }

            Expression::CompoundAssignment(ca) => {
                let lhs_type = self.analyse_expr(&wrap_into_expression(ca.lhs.clone())?)?;

                let rhs_type = self.analyse_expr(&wrap_into_expression(ca.rhs.clone())?)?;

                match (&lhs_type, &rhs_type) {
                    (Type::I32(_), Type::I32(_)) => Ok(Type::I32(Int::I32(i32::default()))),

                    (Type::I32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::I64(_), Type::I64(_)) => Ok(Type::I64(Int::I64(i64::default()))),

                    (Type::I64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U8(_), Type::U8(_)) => Ok(Type::U8(UInt::U8(u8::default()))),

                    (Type::U8(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u8`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U16(_), Type::U16(_)) => Ok(Type::U16(UInt::U16(u16::default()))),

                    (Type::U16(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u16`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U32(_), Type::U32(_)) => Ok(Type::U32(UInt::U32(u32::default()))),

                    (Type::U32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U64(_), Type::U64(_)) => Ok(Type::U64(UInt::U64(u64::default()))),

                    (Type::U64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u64`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U256(_), Type::U256(_)) => {
                        Ok(Type::U256(BigUInt::U256(U256::default())))
                    }

                    (Type::U256(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u256`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::U512(_), Type::U512(_)) => {
                        Ok(Type::U512(BigUInt::U512(U512::default())))
                    }

                    (Type::U512(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u512`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F32(_), Type::F32(_)) => Ok(Type::F32(Float::F32(F32::default()))),

                    (Type::F32(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f32`".to_string(),
                        found: t.to_string(),
                    }),

                    (Type::F64(_), Type::F64(_)) => Ok(Type::F64(Float::F64(F64::default()))),

                    (Type::F64(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`f64`".to_string(),
                        found: t.to_string(),
                    }),

                    _ => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "numeric values with matching types".to_string(),
                        found: format!(
                            "`{} {} {}`",
                            &lhs_type, ca.compound_assignment_op, &rhs_type
                        ),
                    }),
                }
            }

            Expression::Return(r) => match &r.expression_opt {
                Some(e) => self.analyse_expr(&e.clone()),
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

                self.enter_scope(ScopeKind::Function);

                if let Some(v) = &params_opt {
                    let param_types: Vec<Type> = v.iter().map(|p| p.param_type()).collect();

                    for (param, param_type) in v.iter().zip(param_types) {
                        self.insert(param.param_name().clone(), Symbol::Variable(param_type))?;
                    }
                }

                let return_type = match &c.return_type_opt {
                    Some(t) => Ok(*t.clone()),
                    None => Ok(Type::UnitType(Unit)),
                }?;

                let expression_type = self.analyse_expr(&c.body_expression)?;

                self.exit_scope();

                if expression_type != return_type {
                    return Err(SemanticErrorKind::TypeMismatchReturnType {
                        expected: return_type.to_string(),
                        found: expression_type.to_string(),
                    });
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

                        let first_element_type = self.analyse_expr(expr)?;

                        element_count += 1;

                        for elem in v.iter().skip(1) {
                            let element_type = self.analyse_expr(elem)?;

                            element_count += 1;

                            if element_type != first_element_type {
                                return Err(SemanticErrorKind::TypeMismatchArray {
                                    expected: first_element_type.to_string(),
                                    found: element_type.to_string(),
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
                    let ty = self.analyse_expr(expr)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Expression::Struct(s) => {
                let name = PathType::from(s.struct_path.clone()).type_name;

                match self.lookup(&name).cloned() {
                    Some(Symbol::Struct(struct_def)) => {
                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for sf in &struct_fields.unwrap() {
                                let field_name = sf.field_name.clone();
                                let field_value = *sf.field_value.clone();
                                let field_type = self.analyse_expr(&field_value)?;
                                field_map.insert(field_name, field_type);
                            }
                        }

                        let struct_def_fields = struct_def.fields_opt.clone();

                        if struct_def_fields.is_some() {
                            for sdf in &struct_def_fields.unwrap() {
                                match field_map.get(&sdf.field_name) {
                                    Some(expr_type) if *expr_type == *sdf.field_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: name.clone(),
                                            expected: sdf.field_type.clone().to_string(),
                                            found: t.to_string(),
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

                        let path_type = PathType {
                            associated_type_path_prefix_opt: None,
                            type_name: name,
                        };

                        Ok(Type::UserDefined(path_type))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct { name }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Expression::Mapping(m) => match &m.pairs_opt {
                Some(v) => match v.first() {
                    Some(p) => {
                        let key_type =
                            self.analyse_patt(&Pattern::IdentifierPatt(p.key.clone()))?;

                        let value_type = self.analyse_expr(&p.value.clone())?;

                        for pair in v.iter().skip(1) {
                            let pair_key_type =
                                self.analyse_patt(&Pattern::IdentifierPatt(pair.key.clone()))?;

                            let pair_value_type = self.analyse_expr(&pair.value.clone())?;

                            if (&pair_key_type, &pair_value_type) != (&key_type, &value_type) {
                                return Err(SemanticErrorKind::UnexpectedType {
                                    expected: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &key_type.to_string(),
                                        &value_type.to_string()
                                    ),
                                    found: format!(
                                        "{{ key: `{}`, value: `{}` }}",
                                        &pair_key_type.to_string(),
                                        &pair_value_type.to_string()
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

                    let ty = match vec.last() {
                        Some(s) => match s {
                            Statement::Expression(e) => self.analyse_expr(e)?,

                            _ => {
                                self.analyse_stmt(s)?;
                                Type::UnitType(Unit)
                            }
                        },
                        None => Type::UnitType(Unit),
                    };

                    self.exit_scope();

                    Ok(ty)
                }

                None => Ok(Type::UnitType(Unit)),
            },

            Expression::If(i) => {
                self.analyse_expr(&Expression::Grouped(*i.condition.clone()))?;

                let if_block_type = self.analyse_expr(&Expression::Block(*i.if_block.clone()))?;

                let else_if_blocks_type = match &i.else_if_blocks_opt {
                    Some(v) => match v.first() {
                        Some(_) => {
                            for block in v.iter() {
                                let block_type =
                                    self.analyse_expr(&Expression::If(*block.clone()))?;

                                if block_type != if_block_type {
                                    return Err(SemanticErrorKind::TypeMismatchValues {
                                        expected: if_block_type.to_string(),
                                        found: block_type.to_string(),
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
                    Some(b) => self.analyse_expr(&Expression::Block(b.clone()))?,
                    None => Type::UnitType(Unit),
                };

                if else_if_blocks_type != if_block_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: if_block_type.to_string(),
                        found: else_if_blocks_type.to_string(),
                    });
                }

                if trailing_else_block_type != if_block_type {
                    return Err(SemanticErrorKind::TypeMismatchValues {
                        expected: if_block_type.to_string(),
                        found: trailing_else_block_type.to_string(),
                    });
                }

                Ok(if_block_type)
            }

            Expression::Match(m) => {
                self.enter_scope(ScopeKind::MatchExpr);

                let scrutinee_type =
                    self.analyse_expr(&wrap_into_expression(m.scrutinee.clone())?)?;

                let patt_type = self.analyse_patt(&m.final_arm.matched_pattern.clone())?;

                if patt_type != scrutinee_type {
                    return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                        loc: "scrutinee and matched pattern".to_string(),
                        expected: scrutinee_type.to_string(),
                        found: patt_type.to_string(),
                    });
                }

                let expr_type = self.analyse_expr(&m.final_arm.arm_expression.clone())?;

                if let Some(v) = &m.match_arms_opt {
                    for arm in v.iter() {
                        let arm_patt_type = self.analyse_patt(&arm.matched_pattern)?;

                        if arm_patt_type != patt_type {
                            return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                loc: "matched pattern".to_string(),
                                expected: patt_type.to_string(),
                                found: arm_patt_type.to_string(),
                            });
                        }

                        let arm_expr_type = self.analyse_expr(&arm.arm_expression.clone())?;

                        if arm_expr_type != expr_type {
                            return Err(SemanticErrorKind::TypeMismatchMatchExpr {
                                loc: "match arm expression".to_string(),
                                expected: expr_type.to_string(),
                                found: arm_expr_type.to_string(),
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
                self.analyse_expr(&fi.iterator.clone())?;

                let ty = self.analyse_expr(&Expression::Block(fi.block.clone()))?;

                self.exit_scope();

                Ok(ty)
            }

            Expression::While(w) => {
                self.analyse_expr(&Expression::Grouped(*w.condition.clone()))?;
                let ty = self.analyse_expr(&Expression::Block(w.block.clone()))?;
                Ok(ty)
            }

            Expression::SomeExpr(s) => {
                let ty = self.analyse_expr(&s.expression.clone().inner_expression)?;

                Ok(Type::Option {
                    inner_type: Box::new(ty),
                })
            }

            Expression::NoneExpr(_) => Ok(Type::Option {
                inner_type: Box::new(Type::UnitType(Unit)),
            }),

            Expression::ResultExpr(r) => {
                let ty = self.analyse_expr(&r.expression.clone().inner_expression)?;

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
                        found: k.to_string(),
                    }),
                }
            }
        }
    }

    fn analyse_call_or_method_call_expr(
        &mut self,
        name: Identifier,
        args_opt: Option<Vec<Expression>>,
    ) -> Result<Type, SemanticErrorKind> {
        match self.lookup(&name) {
            Some(Symbol::Function { function, .. }) => {
                let args = args_opt.clone();
                let params = function.params_opt.clone();
                let return_type = function.return_type_opt.clone();

                match (&args, &params) {
                    (None, None) => Ok(Type::UnitType(Unit)),
                    (None, Some(_)) | (Some(_), None) => {
                        return Err(SemanticErrorKind::ArgumentCountMismatch {
                            name,
                            expected: params.unwrap_or(Vec::new()).len(),
                            found: args.unwrap_or(Vec::new()).len(),
                        })
                    }
                    (Some(a), Some(p)) => {
                        if a.len() != p.len() {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                name,
                                expected: p.len(),
                                found: a.len(),
                            });
                        }

                        for (arg, param) in a.iter().zip(p) {
                            let arg_type = self.analyse_expr(&arg)?;
                            let param_type = param.param_type();
                            if arg_type != param_type {
                                return Err(SemanticErrorKind::TypeMismatchArgument {
                                    name,
                                    expected: param_type.to_string(),
                                    found: arg_type.to_string(),
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

            None => Err(SemanticErrorKind::UndefinedFunction { name }),

            Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                name,
                expected: "function".to_string(),
                found: s.to_string(),
            }),
        }
    }

    fn analyse_patt(&mut self, pattern: &Pattern) -> Result<Type, SemanticErrorKind> {
        match pattern {
            Pattern::IdentifierPatt(i) => match self.lookup(&i.name) {
                Some(Symbol::Variable(var_type)) => Ok(var_type.clone()),
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
                let name = match &p.tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        None => match &p.path_root {
                            PathRoot::Identifier(i) => i.clone(),

                            PathRoot::SelfType(s) => return Ok(Type::SelfType(s.clone())),

                            pr => {
                                return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                    name: Identifier::from(&pr.to_string()),
                                })
                            }
                        },
                    },

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => i.clone(),

                        PathRoot::SelfType(s) => return Ok(Type::SelfType(s.clone())),

                        pr => {
                            return Err(SemanticErrorKind::InvalidVariableIdentifier {
                                name: Identifier::from(&pr.to_string()),
                            })
                        }
                    },
                };

                match self.lookup(&name) {
                    Some(Symbol::Variable(t)) => Ok(t.clone()),
                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name,
                        expected: "variable".to_string(),
                        found: s.to_string(),
                    }),
                    None => Err(SemanticErrorKind::UndefinedVariable { name }),
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
                    Hash::H160(_) => Ok(Type::H160(Hash::H160(H160::default()))),
                    Hash::H256(_) => Ok(Type::H256(Hash::H256(H256::default()))),
                    Hash::H512(_) => Ok(Type::H512(Hash::H512(H512::default()))),
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
                            found: to_type.to_string(),
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
                            found: from_type.to_string(),
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
                                found: from_type.to_string(),
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
                                found: from_type.to_string(),
                            })
                        }
                    }

                    if from_type == to_type {
                        Ok(to_type)
                    } else {
                        Err(SemanticErrorKind::TypeMismatchValues {
                            expected: from_type.to_string(),
                            found: to_type.to_string(),
                        })
                    }
                }
            },

            Pattern::TuplePatt(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for (patt, _) in t.tuple_patt_elements.elements.iter() {
                    let ty = self.analyse_patt(patt)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Pattern::StructPatt(s) => {
                let name = PathType::from(s.struct_path.clone()).type_name;

                match self.lookup(&name).cloned() {
                    Some(Symbol::Struct(struct_def)) => {
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

                        let struct_def_fields = struct_def.fields_opt.clone();

                        if struct_def_fields.is_some() {
                            for sdf in &struct_def_fields.unwrap() {
                                match field_map.get(&sdf.field_name) {
                                    Some(expr_type) if *expr_type == *sdf.field_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: name.clone(),
                                            expected: sdf.field_type.clone().to_string(),
                                            found: t.to_string(),
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

                        let path_type = PathType {
                            associated_type_path_prefix_opt: None,
                            type_name: name,
                        };

                        Ok(Type::UserDefined(path_type))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct { name }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name,
                        expected: "struct".to_string(),
                        found: s.to_string(),
                    }),
                }
            }

            Pattern::TupleStructPatt(ts) => {
                let name = PathType::from(ts.struct_path.clone()).type_name;

                match self.lookup(&name).cloned() {
                    Some(Symbol::TupleStruct(tuple_struct_def)) => {
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

                        let tuple_struct_def_elements = tuple_struct_def.elements_opt.clone();

                        if tuple_struct_def_elements.is_some() {
                            for (i, tsde) in tuple_struct_def_elements.unwrap().iter().enumerate() {
                                match element_map.get(&i) {
                                    Some(patt_type) if *patt_type == *tsde.element_type => (),
                                    Some(t) => {
                                        return Err(SemanticErrorKind::TypeMismatchVariable {
                                            name: name.clone(),
                                            expected: tsde.element_type.clone().to_string(),
                                            found: t.to_string(),
                                        })
                                    }
                                    None => {
                                        return Err(SemanticErrorKind::MissingTupleStructElement {
                                            expected: format!(
                                                "`{}.{}: {}`",
                                                &name, i, *tsde.element_type
                                            ),
                                        })
                                    }
                                }
                            }
                        }

                        let path_type = PathType {
                            associated_type_path_prefix_opt: None,
                            type_name: name,
                        };

                        Ok(Type::UserDefined(path_type))
                    }

                    None => Err(SemanticErrorKind::UndefinedStruct { name }),

                    Some(s) => Err(SemanticErrorKind::UnexpectedSymbol {
                        name,
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
                        found: k.to_string(),
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

fn wrap_into_expression<T>(value: T) -> Result<Expression, SemanticErrorKind>
where
    T: Clone + fmt::Debug + TryFrom<Expression>,
    Expression: TryFrom<T>,
{
    Expression::try_from(value.clone()).map_err(|_| SemanticErrorKind::ConversionError {
        from: format!("`{:?}`", &value.clone()),
        into: "`Expression`".to_string(),
    })
}
