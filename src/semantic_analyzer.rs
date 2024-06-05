#![allow(dead_code)]

mod symbol_table;

use std::collections::HashMap;

use crate::{
    ast::{
        BigUInt, Bool, Byte, Bytes, Char, Expression, Float, FunctionItem, FunctionOrMethodParam,
        Hash, Identifier, ImportTree, InferredType, InherentImplItem, Int, Item, Literal, PathExpr,
        PathRoot, PathType, Pattern, SelfType, Statement, Str, TraitDefItem, TraitImplItem, Type,
        UInt, UnderscoreExpr, Unit, ValueExpr,
    },
    error::{CompilerError, ErrorsEmitted, SemanticErrorKind},
    parser::Module,
    span::{Span, Spanned},
    B16, B2, B32, B4, B8, F32, F64, H160, H256, H512, U256, U512,
};

use self::symbol_table::{Symbol, SymbolTable};

struct SemanticAnalyzer {
    symbol_table: SymbolTable,
    errors: Vec<CompilerError<SemanticErrorKind>>,
    // TODO: add `Logger`
}

impl SemanticAnalyzer {
    fn new() -> Self {
        SemanticAnalyzer {
            symbol_table: SymbolTable::new(),
            errors: Vec::new(),
        }
    }

    fn analyze(&mut self, module: &Module) -> Result<(), ErrorsEmitted> {
        // TODO: log info – starting semantic analysis

        for s in &module.statements {
            self.analyze_stmt(s).map_err(|e| {
                self.log_error(e, &s.span());
                ErrorsEmitted
            })?
        }

        // TODO: log info – semantic analysis complete, no errors detected

        Ok(())
    }

    fn analyze_stmt(&mut self, statement: &Statement) -> Result<(), SemanticErrorKind> {
        match statement {
            Statement::Let(ls) => {
                let expr_type = match &ls.value_opt {
                    Some(v) => self.analyze_expr(v)?,
                    None => Type::UnitType(Unit),
                };

                self.symbol_table
                    .insert(ls.assignee.name.clone(), Symbol::Variable(expr_type))
                    .map_err(|e| match e {
                        SemanticErrorKind::DuplicateVariable { .. } => {
                            SemanticErrorKind::DuplicateVariable {
                                name: ls.assignee.name.clone(),
                            }
                        }
                        _ => e,
                    })?;
            }

            Statement::Item(i) => match i {
                Item::ImportDecl(id) => self.analyze_import(&id.import_tree)?,

                Item::AliasDecl(ad) => match &ad.original_type_opt {
                    Some(t) => {
                        self.symbol_table
                            .insert(ad.alias_name.clone(), Symbol::Variable(t.clone()))?;

                        // TODO: log info – alias declaration initialized
                    }
                    None => {
                        let ty = PathType {
                            associated_type_path_prefix_opt: None,
                            type_name: ad.alias_name.clone(),
                        };

                        self.symbol_table.insert(
                            ad.alias_name.clone(),
                            Symbol::Variable(Type::UserDefined(ty)),
                        )?;

                        // TODO: log info – alias declaration initialized
                    }
                },

                Item::ConstantDecl(cd) => {
                    let value_type = match &cd.value_opt {
                        Some(v) => {
                            let value = Expression::try_from(v.clone()).map_err(|_| {
                                SemanticErrorKind::ConversionError {
                                    from: format!("`{:?}`", &v),
                                    into: "`Expression`".to_string(),
                                }
                            })?;

                            self.analyze_expr(&value)?
                        }

                        None => Type::UnitType(Unit),
                    };

                    let constant_type = *cd.constant_type.clone();

                    if value_type != constant_type {
                        self.log_error(
                            SemanticErrorKind::TypeMismatchVariable {
                                name: cd.constant_name.clone(),
                                expected: constant_type.to_string(),
                                found: value_type.to_string(),
                            },
                            &cd.value_opt
                                .clone()
                                .unwrap_or(ValueExpr::UnderscoreExpr(UnderscoreExpr {
                                    underscore: Identifier::from("_"),
                                    span: cd.span.clone(),
                                }))
                                .span(),
                        );
                    }

                    self.symbol_table.insert(
                        cd.constant_name.clone(),
                        Symbol::Variable(*cd.constant_type.clone()),
                    )?;

                    // TODO: log info – constant declaration initialized
                }

                Item::StaticVarDecl(s) => {
                    let assignee_type = match &s.assignee_opt {
                        Some(a) => {
                            let assignee = Expression::try_from(*a.clone()).map_err(|_| {
                                SemanticErrorKind::ConversionError {
                                    from: format!("`{:?}`", &a),
                                    into: "`Expression`".to_string(),
                                }
                            })?;

                            self.analyze_expr(&assignee)?
                        }

                        None => Type::UnitType(Unit),
                    };

                    let var_type = s.var_type.clone();

                    if assignee_type != var_type {
                        return Err(SemanticErrorKind::TypeMismatchVariable {
                            name: s.var_name.clone(),
                            expected: var_type.to_string(),
                            found: assignee_type.to_string(),
                        });
                    }

                    self.symbol_table
                        .insert(s.var_name.clone(), Symbol::Variable(s.var_type.clone()))?;

                    // TODO: log info – static variable declaration initialized
                }

                Item::ModuleItem(m) => {
                    if m.items_opt.is_some() {
                        for item in m.items_opt.as_ref().unwrap() {
                            let _ = self.analyze_stmt(&Statement::Item(item.clone()));
                        }
                    } else {
                        let outer_attributes = m.outer_attributes_opt.as_ref();

                        let inner_attributes = m.inner_attributes_opt.as_ref();

                        if outer_attributes.is_some() {
                            return Err(SemanticErrorKind::UnexpectedAttribute {
                                name: format!("{:?}", &outer_attributes.unwrap()),
                                msg: format!(
                                    "Outer attributes out of context – expected module items"
                                ),
                            });
                        } else if inner_attributes.is_some() {
                            return Err(SemanticErrorKind::UnexpectedAttribute {
                                name: format!("{:?}", &inner_attributes.unwrap()),
                                msg: format!(
                                    "Inner attributes out of context – expected module items"
                                ),
                            });
                        }
                    }
                }

                Item::TraitDef(t) => {
                    self.symbol_table
                        .insert(t.trait_name.clone(), Symbol::Trait(t.clone()))
                        .map_err(|e| match e {
                            SemanticErrorKind::DuplicateVariable { .. } => {
                                SemanticErrorKind::DuplicateVariable {
                                    name: t.trait_name.clone(),
                                }
                            }
                            _ => e,
                        })?;

                    // TODO: log info – trait definition initialized

                    let trait_items = t.trait_items_opt.as_ref();

                    if trait_items.is_some() {
                        for item in trait_items.unwrap() {
                            match item {
                                TraitDefItem::FunctionItem(fi) => {
                                    self.analyze_function_def(&fi);
                                }

                                // TODO: log info – function definition acknowledged, but not initialized
                                TraitDefItem::AliasDecl(ad) => self
                                    .analyze_stmt(&Statement::Item(Item::AliasDecl(ad.clone())))?,
                                TraitDefItem::ConstantDecl(cd) => self.analyze_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,
                            }
                        }
                    }
                }

                Item::EnumDef(ed) => {
                    self.symbol_table
                        .insert(ed.enum_name.clone(), Symbol::Enum(ed.clone()))
                        .map_err(|err| match err {
                            SemanticErrorKind::DuplicateVariable { .. } => {
                                SemanticErrorKind::DuplicateVariable {
                                    name: ed.enum_name.clone(),
                                }
                            }
                            _ => err,
                        })?;

                    // TODO: log info – enum definition initialized
                }

                Item::StructDef(s) => {
                    self.symbol_table
                        .insert(s.struct_name.clone(), Symbol::Struct(s.clone()))
                        .map_err(|e| match e {
                            SemanticErrorKind::DuplicateVariable { .. } => {
                                SemanticErrorKind::DuplicateVariable {
                                    name: s.struct_name.clone(),
                                }
                            }
                            _ => e,
                        })?;

                    // TODO: log info – struct definition initialized
                }

                Item::TupleStructDef(ts) => {
                    self.symbol_table
                        .insert(ts.struct_name.clone(), Symbol::TupleStruct(ts.clone()))
                        .map_err(|e| match e {
                            SemanticErrorKind::DuplicateVariable { .. } => {
                                SemanticErrorKind::DuplicateVariable {
                                    name: ts.struct_name.clone(),
                                }
                            }
                            _ => e,
                        })?;

                    // TODO: log info – struct definition initialized
                }

                Item::InherentImplDef(i) => {
                    if i.associated_items_opt.is_some() {
                        for item in i.associated_items_opt.as_ref().unwrap() {
                            match item {
                                InherentImplItem::ConstantDecl(cd) => self.analyze_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,
                                InherentImplItem::FunctionItem(fi) => {
                                    self.analyze_function_def(&fi);

                                    self.symbol_table.insert(
                                        fi.function_name.clone(),
                                        Symbol::Function {
                                            associated_type_opt: Some(i.nominal_type.clone()),
                                            function: fi.clone(),
                                        },
                                    );

                                    // TODO: log info – associated function definition initialized
                                }
                            }
                        }
                    } else {
                        let attributes = i.attributes_opt.as_ref();

                        if attributes.is_some() {
                            return Err(SemanticErrorKind::UnexpectedAttribute {
                                name: format!("{:?}", &attributes.unwrap()),
                                msg: format!(
                                    "Outer attributes out of context – expected associated items"
                                ),
                            });
                        }
                    }
                }

                Item::TraitImplDef(t) => {
                    if t.associated_items_opt.is_some() {
                        for item in t.associated_items_opt.as_ref().unwrap() {
                            match item {
                                TraitImplItem::AliasDecl(ad) => self
                                    .analyze_stmt(&Statement::Item(Item::AliasDecl(ad.clone())))?,
                                TraitImplItem::ConstantDecl(cd) => self.analyze_stmt(
                                    &Statement::Item(Item::ConstantDecl(cd.clone())),
                                )?,
                                TraitImplItem::FunctionItem(fi) => {
                                    self.analyze_function_def(&fi);

                                    self.symbol_table.insert(
                                        fi.function_name.clone(),
                                        Symbol::Function {
                                            associated_type_opt: Some(
                                                t.implemented_trait_path.clone(),
                                            ),
                                            function: fi.clone(),
                                        },
                                    );

                                    // TODO: log info – associated function definition initialized
                                }
                            }
                        }
                    } else {
                        let attributes = t.attributes_opt.as_ref();

                        if attributes.is_some() {
                            return Err(SemanticErrorKind::UnexpectedAttribute {
                                name: format!("{:?}", &attributes.unwrap()),
                                msg: format!(
                                    "Outer attributes out of context – expected associated items"
                                ),
                            });
                        }
                    }
                }

                Item::FunctionItem(f) => {
                    self.analyze_function_def(f)?;

                    self.symbol_table
                        .insert(
                            f.function_name.clone(),
                            Symbol::Function {
                                associated_type_opt: None,
                                function: f.clone(),
                            },
                        )
                        .map_err(|e| match e {
                            SemanticErrorKind::DuplicateVariable { .. } => {
                                SemanticErrorKind::DuplicateVariable {
                                    name: f.function_name.clone(),
                                }
                            }
                            _ => e,
                        })?;

                    // TODO: log info – function definition initialized
                }
            },

            Statement::Expression(expr) => {
                self.analyze_expr(expr)?;
            }
        }

        Ok(())
    }

    fn analyze_expr(&mut self, expression: &Expression) -> Result<Type, SemanticErrorKind> {
        match expression {
            Expression::Path(p) => {
                let name = match &p.tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        _ => match &p.path_root {
                            PathRoot::Identifier(i) => i.clone(),

                            PathRoot::SelfType(_) => Identifier::from("Self"),

                            PathRoot::SelfKeyword => Identifier::from("self"),

                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("package"),
                                })
                            }

                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("super"),
                                })
                            }
                        },
                    },

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => i.clone(),

                        PathRoot::SelfType(_) => Identifier::from("Self"),

                        PathRoot::SelfKeyword => Identifier::from("self"),

                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("package"),
                            })
                        }

                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("super"),
                            })
                        }
                    },
                };

                match self.symbol_table.get(&name) {
                    Some(_) => Ok(Type::UnitType(Unit)),
                    _ => Err(SemanticErrorKind::UndefinedPath { name }),
                }
            }

            Expression::Literal(l) => match l {
                Literal::Int { value, .. } => match value {
                    Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                    Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
                    Int::I128(_) => Ok(Type::I128(Int::I128(i128::default()))),
                },

                Literal::UInt { value, .. } => match value {
                    UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                    UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                    UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                    UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
                    UInt::U128(_) => Ok(Type::U128(UInt::U128(u128::default()))),
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
                let receiver = Expression::try_from(*mc.receiver.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", &mc),
                        into: "`Expression`".to_string(),
                    }
                })?;

                let path_expr = PathExpr::try_from(receiver.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: receiver.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                let type_name = PathType::from(path_expr).type_name;

                match self.symbol_table.get(&type_name) {
                    Some(Symbol::Struct(s)) => {
                        if s.struct_name == type_name {
                            self.analyze_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            todo!() // type mismatch (variable)
                        }
                    }

                    Some(Symbol::TupleStruct(ts)) => {
                        if ts.struct_name == type_name {
                            self.analyze_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            todo!() // type mismatch (variable)
                        }
                    }

                    Some(Symbol::Enum(e)) => {
                        if e.enum_name == type_name {
                            self.analyze_call_or_method_call_expr(
                                mc.method_name.clone(),
                                mc.args_opt.clone(),
                            )
                        } else {
                            todo!() // type mismatch (variable)
                        }
                    }

                    _ => todo!(), // undefined type (receiver type)
                }
            }

            Expression::FieldAccess(fa) => {
                let object_type =
                    self.analyze_expr(&Expression::try_from(*fa.object.clone()).map_err(
                        |_| SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &fa),
                            into: "`Expression`".to_string(),
                        },
                    )?)?;

                match self
                    .symbol_table
                    .get(&Identifier::from(&object_type.to_string()))
                {
                    Some(Symbol::Struct(s)) => match &s.fields_opt {
                        Some(v) => match v.iter().find(|f| f.field_name == fa.field_name) {
                            Some(sdf) => Ok(*sdf.field_type.clone()),
                            _ => todo!(), // undefined field
                        },
                        None => Ok(Type::UnitType(Unit)),
                    },

                    _ => todo!(), // undefined type
                }
            }

            Expression::Call(c) => {
                let callee = Expression::try_from(c.callee.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", &c),
                        into: "`Expression`".to_string(),
                    }
                })?;

                let path_expr = PathExpr::try_from(callee.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: callee.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                let name = PathType::from(path_expr).type_name;

                self.analyze_call_or_method_call_expr(name, c.args_opt.clone())
            }

            Expression::Index(i) => {
                let array_type =
                    self.analyze_expr(&Expression::try_from(*i.array.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &i),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let index_type =
                    self.analyze_expr(&Expression::try_from(*i.index.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &i),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                match index_type {
                    Type::I32(_)
                    | Type::I64(_)
                    | Type::U8(_)
                    | Type::U16(_)
                    | Type::U32(_)
                    | Type::U64(_)
                    | Type::U128(_) => (),
                    _ => todo!(), // type mismatch (variable)
                }

                match array_type {
                    Type::Array { element_type, .. } => Ok(*element_type),
                    _ => todo!(), // unexpected type (expected array)
                }
            }

            Expression::TupleIndex(_) => todo!(),

            Expression::Unwrap(u) => {
                self.analyze_expr(&Expression::try_from(*u.value_expr.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", &u),
                        into: "`Expression`".to_string(),
                    }
                })?)
            }

            Expression::Unary(_) => todo!(),

            Expression::Reference(r) => {
                let reference_op = r.reference_op.clone();
                let inner_type = self.analyze_expr(&*r.expression)?;

                Ok(Type::Reference {
                    reference_op,
                    inner_type: Box::new(inner_type),
                })
            }

            Expression::Dereference(d) => {
                self.analyze_expr(&Expression::try_from(d.assignee_expr.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", &d),
                        into: "`Expression`".to_string(),
                    }
                })?)
            }

            Expression::TypeCast(tc) => Ok(*tc.new_type.clone()),

            Expression::Binary(b) => {
                let lhs_clone = *b.lhs.clone();
                let rhs_clone = *b.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(*b.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(*b.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

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

                    (Type::I128(_), Type::I32(_) | Type::I64(_) | Type::I128(_)) => {
                        Ok(Type::I128(Int::I128(i128::default())))
                    }

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128` or smaller signed integer".to_string(),
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

                    (
                        Type::U128(_),
                        Type::U8(_) | Type::U16(_) | Type::U32(_) | Type::U64(_) | Type::U128(_),
                    ) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128` or smaller unsigned integer".to_string(),
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
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Comparison(c) => {
                let lhs_clone = c.lhs.clone();
                let rhs_clone = c.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(c.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(c.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

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

                    (Type::I128(_), Type::I128(_)) => Ok(Type::I128(Int::I128(i128::default()))),

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128`".to_string(),
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

                    (Type::U128(_), Type::U128(_)) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128`".to_string(),
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
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Grouped(g) => self.analyze_expr(&g.inner_expression),

            Expression::Range(_) => todo!(),

            Expression::Assignment(a) => {
                let expr = Expression::try_from(a.lhs.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: format!("`{:?}`", &a.lhs.clone()),
                        into: "`Expression`".to_string(),
                    }
                })?;

                let path = PathExpr::try_from(expr.clone()).map_err(|_| {
                    SemanticErrorKind::ConversionError {
                        from: expr.to_string(),
                        into: "`PathExpr`".to_string(),
                    }
                })?;

                let expr_type = self.analyze_expr(&expr)?;

                let name = Identifier(path.path_root.to_string());

                match self.symbol_table.get(&name) {
                    Some(Symbol::Variable(var_type)) if *var_type == expr_type => {
                        Ok(var_type.clone())
                    }

                    // TODO: add expression name (technically a type mismatch)
                    Some(t) => Err(SemanticErrorKind::UnexpectedType {
                        expected: expr_type.to_string(),
                        found: format!("`{}`", &t),
                    }),
                    None => Err(SemanticErrorKind::UndefinedVariable { name }),
                }
            }

            Expression::CompoundAssignment(ca) => {
                let lhs_clone = ca.lhs.clone();
                let rhs_clone = ca.rhs.clone();

                let lhs_type =
                    self.analyze_expr(&Expression::try_from(ca.lhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &lhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

                let rhs_type =
                    self.analyze_expr(&Expression::try_from(ca.rhs.clone()).map_err(|_| {
                        SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &rhs_clone),
                            into: "`Expression`".to_string(),
                        }
                    })?)?;

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

                    (Type::I128(_), Type::I128(_)) => Ok(Type::I128(Int::I128(i128::default()))),

                    (Type::I128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`i128`".to_string(),
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

                    (Type::U128(_), Type::U128(_)) => Ok(Type::U128(UInt::U128(u128::default()))),

                    (Type::U128(_), t) => Err(SemanticErrorKind::TypeMismatchBinaryExpr {
                        expected: "`u128`".to_string(),
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
                        expected: "matching numeric types".to_string(),
                        found: format!("`({:?}, {:?})`", &lhs_type, &rhs_type),
                    }),
                }
            }

            Expression::Return(r) => match &r.expression_opt {
                Some(e) => self.analyze_expr(&*e.clone()),
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Break(_) => Ok(Type::UnitType(Unit)),

            Expression::Continue(_) => Ok(Type::UnitType(Unit)),

            Expression::Underscore(_) => Ok(Type::InferredType(InferredType {
                underscore: Identifier::from("_"),
            })),

            Expression::Closure(c) => {
                let return_type = match &c.return_type_opt {
                    Some(t) => Ok(*t.clone()),
                    None => Ok(Type::UnitType(Unit)),
                }?;

                let expression_type = self.analyze_expr(&*c.body_expression)?;

                if return_type != expression_type {
                    return Err(SemanticErrorKind::TypeMismatchReturnType {
                        expected: return_type.to_string(),
                        found: expression_type.to_string(),
                    });
                }

                Ok(return_type)
            }

            Expression::Array(a) => match &a.elements_opt {
                Some(v) => match v.first() {
                    Some(expr) => {
                        let mut element_count = 0u128;

                        let first_element_type = self.analyze_expr(expr)?;

                        element_count += 1;

                        for elem in v.iter().skip(1) {
                            let element_type = self.analyze_expr(elem)?;

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
                            num_elements: UInt::U128(element_count),
                        })
                    }

                    None => {
                        let element_type = Type::UnitType(Unit);
                        let array = Type::Array {
                            element_type: Box::new(element_type),
                            num_elements: UInt::U128(0u128),
                        };

                        Ok(array)
                    }
                },
                None => Ok(Type::UnitType(Unit)),
            },

            Expression::Tuple(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for expr in t.tuple_elements.elements.iter() {
                    let ty = self.analyze_expr(expr)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Expression::Struct(s) => {
                let struct_path_root = s.struct_path.path_root.clone();
                let struct_tree_opt = s.struct_path.tree_opt.clone();

                let name = match &struct_tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        None => match &struct_path_root {
                            PathRoot::Identifier(i) => i.clone(),
                            PathRoot::SelfType(_) => Identifier::from("Self"),
                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("package"),
                                })
                            }
                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("super"),
                                })
                            }
                            PathRoot::SelfKeyword => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("self"),
                                })
                            }
                        },
                    },
                    None => match &struct_path_root {
                        PathRoot::Identifier(i) => i.clone(),
                        PathRoot::SelfType(_) => Identifier::from("Self"),
                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("package"),
                            })
                        }
                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("super"),
                            })
                        }
                        PathRoot::SelfKeyword => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("self"),
                            })
                        }
                    },
                };

                let symbol_table_clone = self.symbol_table.clone();

                match symbol_table_clone.get(&name) {
                    Some(Symbol::Struct(struct_def)) => {
                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for sf in &struct_fields.unwrap() {
                                let field_name = sf.field_name.clone();
                                let field_value = *sf.field_value.clone();
                                let field_type = self.analyze_expr(&field_value)?;
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
                    _ => Err(SemanticErrorKind::UndefinedStruct { name }),
                }
            }

            Expression::Mapping(m) => match &m.pairs_opt {
                Some(v) => match v.first() {
                    Some(p) => {
                        let key_type =
                            self.analyze_patt(&Pattern::IdentifierPatt(p.key.clone()))?;

                        let value_type = self.analyze_expr(&*p.value.clone())?;

                        for pair in v.iter().skip(1) {
                            let pair_key_type =
                                self.analyze_patt(&Pattern::IdentifierPatt(pair.key.clone()))?;

                            let pair_value_type = self.analyze_expr(&*pair.value.clone())?;

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
                Some(v) => match v.last() {
                    Some(s) => match s {
                        Statement::Let(ls) => match &ls.value_opt {
                            Some(e) => self.analyze_expr(e),
                            None => Ok(Type::UnitType(Unit)),
                        },

                        Statement::Item(_) => Ok(Type::UnitType(Unit)),

                        Statement::Expression(e) => self.analyze_expr(e),
                    },

                    None => Ok(Type::UnitType(Unit)),
                },

                None => Ok(Type::UnitType(Unit)),
            },

            Expression::If(i) => {
                let if_block_type = self.analyze_expr(&Expression::Block(*i.if_block.clone()))?;

                let else_if_blocks_type = match &i.else_if_blocks_opt {
                    Some(v) => match v.first() {
                        Some(_) => {
                            for block in v.iter() {
                                let block_type =
                                    self.analyze_expr(&Expression::If(*block.clone()))?;

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
                    Some(b) => self.analyze_expr(&Expression::Block(b.clone()))?,
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
                let scrutinee_type =
                    self.analyze_expr(&Expression::try_from(m.scrutinee.clone()).map_err(
                        |_| SemanticErrorKind::ConversionError {
                            from: format!("`{:?}`", &m.scrutinee),
                            into: "Expression".to_string(),
                        },
                    )?)?;

                let match_arms_type = match &m.match_arms_opt {
                    Some(v) => match v.first() {
                        Some(_) => {
                            for arm in v.iter() {
                                let arm_type = self.analyze_patt(&arm.matched_pattern)?;

                                if arm_type != scrutinee_type {
                                    return Err(SemanticErrorKind::UnexpectedType {
                                        expected: scrutinee_type.to_string(),
                                        found: arm_type.to_string(),
                                    });
                                }
                            }

                            scrutinee_type.clone()
                        }
                        None => Type::UnitType(Unit),
                    },
                    None => Type::UnitType(Unit),
                };

                let final_arm_type = self.analyze_patt(&m.final_arm.matched_pattern.clone())?;

                if match_arms_type != scrutinee_type {
                    return Err(SemanticErrorKind::UnexpectedType {
                        expected: scrutinee_type.to_string(),
                        found: match_arms_type.to_string(),
                    });
                }

                if final_arm_type != scrutinee_type {
                    return Err(SemanticErrorKind::UnexpectedType {
                        expected: scrutinee_type.to_string(),
                        found: final_arm_type.to_string(),
                    });
                }

                Ok(scrutinee_type)
            }

            Expression::ForIn(fi) => self.analyze_expr(&Expression::Block(fi.block.clone())),

            Expression::While(w) => self.analyze_expr(&Expression::Block(w.block.clone())),

            Expression::SomeExpr(s) => self.analyze_expr(&*s.expression.clone().inner_expression),

            Expression::NoneExpr(_) => Ok(Type::UnitType(Unit)),

            Expression::ResultExpr(r) => self.analyze_expr(&*r.expression.clone().inner_expression),
        }
    }

    fn analyze_call_or_method_call_expr(
        &mut self,
        name: Identifier,
        args_opt: Option<Vec<Expression>>,
    ) -> Result<Type, SemanticErrorKind> {
        match self.symbol_table.get(&name) {
            Some(Symbol::Function { function, .. }) => {
                let args = args_opt.clone();
                let params = function.params_opt.clone();
                let return_type = function.return_type_opt.clone();

                match (&args, &params) {
                    (None, None) => Ok(Type::UnitType(Unit)),
                    (None, Some(_)) | (Some(_), None) => {
                        return Err(SemanticErrorKind::ArgumentCountMismatch {
                            expected: params.unwrap_or(Vec::new()).len(),
                            found: args.unwrap_or(Vec::new()).len(),
                        })
                    }
                    (Some(a), Some(p)) => {
                        if a.len() != p.len() {
                            return Err(SemanticErrorKind::ArgumentCountMismatch {
                                expected: p.len(),
                                found: a.len(),
                            });
                        }

                        for (arg, param) in a.iter().zip(p) {
                            let arg_type = self.analyze_expr(&arg)?;
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
            _ => Err(SemanticErrorKind::UndefinedFunction { name }),
        }
    }

    fn analyze_patt(&mut self, pattern: &Pattern) -> Result<Type, SemanticErrorKind> {
        match pattern {
            Pattern::IdentifierPatt(i) => match self.symbol_table.get(&i.name) {
                Some(Symbol::Variable(var_type)) => Ok(var_type.clone()),
                Some(s) => Err(SemanticErrorKind::UnexpectedType {
                    expected: "variable type".to_string(),
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
                        _ => match &p.path_root {
                            PathRoot::Identifier(i) => i.clone(),

                            PathRoot::SelfType(_) => Identifier::from("Self"),

                            PathRoot::SelfKeyword => Identifier::from("self"),

                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("package"),
                                })
                            }

                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidPathIdentifier {
                                    name: Identifier::from("super"),
                                })
                            }
                        },
                    },

                    _ => match &p.path_root {
                        PathRoot::Identifier(i) => i.clone(),

                        PathRoot::SelfType(_) => Identifier::from("Self"),

                        PathRoot::SelfKeyword => Identifier::from("self"),

                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("package"),
                            })
                        }

                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidPathIdentifier {
                                name: Identifier::from("super"),
                            })
                        }
                    },
                };

                match self.symbol_table.get(&name) {
                    Some(_) => Ok(Type::UnitType(Unit)),
                    _ => Err(SemanticErrorKind::UndefinedPath { name }),
                }
            }

            Pattern::Literal(l) => match l {
                Literal::Int { value, .. } => match value {
                    Int::I32(_) => Ok(Type::I32(Int::I32(i32::default()))),
                    Int::I64(_) => Ok(Type::I64(Int::I64(i64::default()))),
                    Int::I128(_) => Ok(Type::I128(Int::I128(i128::default()))),
                },

                Literal::UInt { value, .. } => match value {
                    UInt::U8(_) => Ok(Type::U8(UInt::U8(u8::default()))),
                    UInt::U16(_) => Ok(Type::U16(UInt::U16(u16::default()))),
                    UInt::U32(_) => Ok(Type::U32(UInt::U32(u32::default()))),
                    UInt::U64(_) => Ok(Type::U64(UInt::U64(u64::default()))),
                    UInt::U128(_) => Ok(Type::U128(UInt::U128(u128::default()))),
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
                inner_type: Box::new(self.analyze_patt(&*r.pattern)?),
            }),

            Pattern::GroupedPatt(g) => self.analyze_patt(&g.inner_pattern),

            Pattern::RangePatt(_) => todo!(),

            Pattern::TuplePatt(t) => {
                let mut element_types: Vec<Type> = Vec::new();

                for (patt, _) in t.tuple_patt_elements.elements.iter() {
                    let ty = self.analyze_patt(patt)?;
                    element_types.push(ty)
                }

                Ok(Type::Tuple(element_types))
            }

            Pattern::StructPatt(s) => {
                let struct_path_root = s.struct_path.path_root.clone();
                let struct_tree_opt = s.struct_path.tree_opt.clone();

                let name = match &struct_tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        None => match &struct_path_root {
                            PathRoot::Identifier(i) => i.clone(),
                            PathRoot::SelfType(_) => Identifier::from("Self"),
                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("package"),
                                })
                            }
                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("super"),
                                })
                            }
                            PathRoot::SelfKeyword => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("self"),
                                })
                            }
                        },
                    },
                    None => match &struct_path_root {
                        PathRoot::Identifier(i) => i.clone(),
                        PathRoot::SelfType(_) => Identifier::from("Self"),
                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("package"),
                            })
                        }
                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("super"),
                            })
                        }
                        PathRoot::SelfKeyword => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("self"),
                            })
                        }
                    },
                };

                let symbol_table = self.symbol_table.clone();

                match symbol_table.get(&name) {
                    Some(Symbol::Struct(struct_def)) => {
                        let mut field_map: HashMap<Identifier, Type> = HashMap::new();

                        let struct_fields = s.struct_fields_opt.clone();

                        if struct_fields.is_some() {
                            for spf in &struct_fields.unwrap() {
                                let field_name = spf.field_name.clone();
                                let field_value = spf.field_value.clone();
                                let field_type = self.analyze_patt(&field_value)?;
                                field_map.insert(field_name, field_type);
                            }
                        }

                        let struct_def_fields = struct_def.fields_opt.clone();

                        if struct_def_fields.is_some() {
                            for sdf in &struct_def_fields.unwrap() {
                                match field_map.get(&sdf.field_name) {
                                    Some(patt_type) if *patt_type == *sdf.field_type => (),
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
                            path_root: PathRoot::Identifier(name.clone()),
                            tree_opt: None,
                        };

                        Ok(Type::UserDefined(path_type))
                    }
                    _ => Err(SemanticErrorKind::UndefinedStruct { name: name.clone() }),
                }
            }

            Pattern::TupleStructPatt(ts) => {
                let struct_path_root = ts.struct_path.path_root.clone();
                let struct_tree_opt = ts.struct_path.tree_opt.clone();

                let name = match &struct_tree_opt {
                    Some(v) => match v.last() {
                        Some(i) => i.clone(),
                        None => match &struct_path_root {
                            PathRoot::Identifier(i) => i.clone(),
                            PathRoot::SelfType(_) => Identifier::from("Self"),
                            PathRoot::Package => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("package"),
                                })
                            }
                            PathRoot::Super => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("super"),
                                })
                            }
                            PathRoot::SelfKeyword => {
                                return Err(SemanticErrorKind::InvalidStructName {
                                    name: Identifier::from("self"),
                                })
                            }
                        },
                    },
                    None => match &struct_path_root {
                        PathRoot::Identifier(i) => i.clone(),
                        PathRoot::SelfType(_) => Identifier::from("Self"),
                        PathRoot::Package => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("package"),
                            })
                        }
                        PathRoot::Super => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("super"),
                            })
                        }
                        PathRoot::SelfKeyword => {
                            return Err(SemanticErrorKind::InvalidStructName {
                                name: Identifier::from("self"),
                            })
                        }
                    },
                };

                let symbol_table = self.symbol_table.clone();

                match symbol_table.get(&name) {
                    Some(Symbol::TupleStruct(tuple_struct_def)) => {
                        let mut element_map: HashMap<usize, Type> = HashMap::new();
                        let mut element_counter = 0usize;

                        let tuple_struct_elements = ts.struct_elements_opt.clone();

                        if tuple_struct_elements.is_some() {
                            for tse in &tuple_struct_elements.unwrap() {
                                let element_type = self.analyze_patt(&tse)?;
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
                            path_root: PathRoot::Identifier(name.clone()),
                            tree_opt: None,
                        };

                        Ok(Type::UserDefined(path_type))
                    }
                    _ => Err(SemanticErrorKind::UndefinedStruct { name: name.clone() }),
                }
            }

            Pattern::WildcardPatt(_) => Ok(Type::InferredType(InferredType {
                underscore: Identifier::from("_"),
            })),

            Pattern::RestPatt(_) => Ok(Type::InferredType(InferredType {
                underscore: Identifier::from("_"),
            })),

            Pattern::SomePatt(s) => self.analyze_patt(&*s.pattern.clone().inner_pattern),

            Pattern::NonePatt(_) => Ok(Type::UnitType(Unit)),

            Pattern::ResultPatt(r) => self.analyze_patt(&*r.pattern.clone().inner_pattern),
        }
    }

    fn analyze_function_def(&mut self, func: &FunctionItem) -> Result<(), SemanticErrorKind> {
        let mut local_table = SymbolTable::with_parent(self.symbol_table.clone());

        if func.params_opt.is_some() {
            for param in &func.params_opt.clone().unwrap() {
                match param {
                    FunctionOrMethodParam::FunctionParam(f) => {
                        local_table.insert(
                            f.param_name.name.clone(),
                            Symbol::Variable(*f.param_type.clone()),
                        )?;
                    }
                    FunctionOrMethodParam::MethodParam(_) => {
                        local_table.insert(
                            Identifier::from("self"),
                            Symbol::Variable(Type::SelfType(SelfType)),
                        )?;
                    }
                }
            }
        }

        let mut analyzer = SemanticAnalyzer {
            symbol_table: local_table,
            errors: Vec::new(),
        };

        if func.block_opt.is_some() {
            let statements = func.block_opt.clone().unwrap().statements_opt;

            if statements.is_some() {
                for stmt in &statements.unwrap() {
                    if let Err(e) = analyzer.analyze_stmt(stmt) {
                        analyzer.log_error(e, &stmt.span())
                    }
                }
            }
        }
        self.errors.extend(analyzer.errors);
        Ok(())
    }

    fn analyze_import(&mut self, tree: &ImportTree) -> Result<(), SemanticErrorKind> {
        // TODO: check if module path exists and is accessible

        let mut path: Vec<PathType> = Vec::new();

        for ps in tree.path_segments.iter() {
            path.push(ps.root.clone());
        }

        let name = match path.last() {
            Some(pt) => Identifier::from(&pt.to_string()),
            None => Identifier::from(""),
        };

        self.symbol_table
            .insert(name.clone(), Symbol::Import(path))
            .map_err(|e| match e {
                SemanticErrorKind::DuplicateImport { .. } => {
                    SemanticErrorKind::DuplicateImport { name }
                }
                _ => e,
            })?;
        Ok(())
    }

    // TODO: update to push error to `self.errors` AND log to `self.logger` (new)
    fn log_error(&mut self, error_kind: SemanticErrorKind, span: &Span) {
        let error = CompilerError::new(error_kind, span.start(), &span.input());

        self.errors.push(error);
    }
}
