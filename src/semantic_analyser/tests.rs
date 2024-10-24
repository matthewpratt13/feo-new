use crate::{
    ast::{Identifier, ModuleItem, TypePath, Visibility},
    logger::LogLevel,
    parser::{self, Program},
};

use super::*;

fn setup(
    input: &str,
    log_level: LogLevel,
    print_tokens: bool,
    print_statements: bool,
    external_code: Option<HashMap<Identifier, Vec<Module>>>,
) -> Result<(SemanticAnalyser, Program), ()> {
    let mut parser = parser::test_utils::get_parser(input, LogLevel::Warning, print_tokens);

    let program = match parser.parse_tokens() {
        Ok(prog) => Ok(prog),
        Err(e) => Err(println!("error parsing tokens: {e:#?}")),
    }?;

    if print_statements {
        println!("{:#?}", program.statements)
    }

    Ok((SemanticAnalyser::new(log_level, external_code), program))
}

#[test]
fn analyse_closure() -> Result<(), ()> {
    let input = r#"
        func do_maths(f: func(x: u64) -> u64, x: u64) -> u64 {
           f(x)
        }

        func add_one(x: u64) -> u64 {
            let closure = |x: u64| -> u64 {
                x + 1
            };

            do_maths(closure, x)
        }
    "#;

    let (mut analyser, program) =
        setup(input, LogLevel::Debug, false, false, None).expect("error setting during test setup");

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_constant_reassign() -> Result<(), ()> {
    let input = r#"
    #[storage]
    const ADDRESS: h160 = $0x12345_12345_12345_12345_12345_12345_12345_12345;
    ADDRESS = $0x54321_54321_54321_54321_54321_54321_54321_54321;"#;

    let (mut analyser, program) = setup(input, LogLevel::Debug, false, false, None)
        .expect("unable to set up semantic analyser");

    match analyser.analyse_program(&program) {
        Ok(_) => Err(println!("expected `SemanticErrorKind::ConstantReassignment` error, but test was successful")),
        Err(errs) => match errs.last() {
            Some(err) => match err.error_kind() {
                SemanticErrorKind::ConstantReassignment { .. } => Ok(println!("test failed correctly with error: {err:#?}")),

                err => Err(println!("test failed with unexpected error. Expected `SemanticErrorKind::ConstantReassignment` error, found `{err}`"))
            },

            _ => Err(println!("test failed, but no errors detected. Expected `SemanticErrorKind::ConstantReassignment` error, found none"))
        },
    }
}

#[test]
fn analyse_control_flow() -> Result<(), ()> {
    let input = r#"
        func greater_than(x: i64) -> bool {
            if (x > 10) {
               true
            } else {
                match x {
                    ..=0 => false,
                    1..9 => false,
                    _ if x + 1 == 10 => false,
                    10 => false,
                    _ => true,
                } 
            }
        }

        func iterate(x: &mut [u64; 10]) -> [u64; 10] {
            for i in x {
                while (i >= 2) {
                    i = i * 2;
                }
            }

            return *x;
        }
    "#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_enum_variants() -> Result<(), ()> {
    let input = r#"
        enum Error {
            StdError,
            TupleStructError(str),
            StructError {
                expected: str,
                found:  str         
            }
        }

        func emit_error(err: Error) {
            let std_error = Error::StdError;
            let tuple_struct_error = Error::TupleStructError("foo");
            let struct_error = Error::StructError { expected: "foo", found: "bar" }; 

            return;
        }
    "#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_impl() -> Result<(), ()> {
    let input = r#"
    module erc_20 {
        trait Contract {
            #![interface]
    
            const CONTRACT_ADDRESS: h160;
            const CREATOR_ADDRESS: h160;
    
            func address() -> h160;
            func balance(&self) -> u64;
            func msg_sender() -> h160;
            func creator_address() -> h160;
        }

        trait ERC20 {
            #![interface]

            func approve(&self, spender: h160, amount: u256) -> Result<(), ()>;
            func transfer(&mut self, from: h160, to: h160, amount: u256) -> Result<(), ()>;
            func mint(&mut self, to: h160, amount: u256) -> Result<(), ()>;
            func burn(&mut self, from: h160, amount: u256) -> Result<(), ()>;
        }
    }

    module some_token {
        #![contract]
        
        import lib::erc_20::{ Contract, ERC20 };

        struct SomeToken {
            name: str,
            symbol: str,
            decimals: u64,
            total_supply: u64,
            balances: Mapping<h160, u64>,        
        }

        alias Foo = SomeToken;
        alias Bar;
            
        impl SomeToken {
            #[constructor]
            func new(name: str, symbol: str, decimals: u64, total_supply: u64, balances: Mapping<h160, u64>) -> SomeToken {
                SomeToken {
                    name: "SomeToken",
                    symbol: "STK",
                    decimals: 18,
                    total_supply: 1_000_000,
                    balances: {
                        $0x12345_ABCDE_12345_ABCDE_12345_ABCDE_12345_ABCDE: 1_000,
                    }
                }
            }
        }

        impl Contract for SomeToken {
            const CONTRACT_ADDRESS: h160 = $0x54321_ABCDE_54321_ABCDE_54321_ABCDE_54321_ABCDE;
            const CREATOR_ADDRESS: h160 = $0x67890_ABCDE_67890_ABCDE_67890_ABCDE_67890_ABCDE;

            func address() -> h160 {
                SomeToken::Contract::CONTRACT_ADDRESS
            }

            func balance(&self) -> u64 {
                self.total_supply
            }

            func msg_sender() -> h160 {
               SomeToken::Contract::CREATOR_ADDRESS
            }

            func creator_address() -> h160 {
               SomeToken::Contract::CREATOR_ADDRESS
            }
        }

        impl ERC20 for SomeToken {
            func approve(&self, spender: h160, amount: u256) -> Result<(), ()> {
                Ok(())
            }

            func transfer(&mut self, from: h160, to: h160, amount: u256) -> Result<(), ()> {
               Ok(())
            }

            func mint(&mut self, to: h160, amount: u256) -> Result<(), ()> {
               Ok(())
            }

            func burn(&mut self, from: h160, amount: u256) -> Result<(), ()> {
                Ok(())
            }
        }
    }"#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_import_decl() -> Result<(), ()> {
    let external_lib_root_id = Identifier::from("external_lib");

    let external_func = FunctionItem {
        attributes_opt: None,
        visibility: Visibility::Pub,
        kw_func: Keyword::Anonymous,
        function_name: Identifier::from("external_func"),
        generic_params_opt: None,
        params_opt: None,
        return_type_opt: None,
        block_opt: None,
        span: Span::default(),
    };

    let external_mod = ModuleItem {
        outer_attributes_opt: None,
        visibility: Visibility::Pub,
        kw_module: Keyword::Anonymous,
        module_name: Identifier::from("external_mod"),
        inner_attributes_opt: None,
        items_opt: Some(vec![Item::FunctionItem(external_func.clone())]),
        span: Span::default(),
    };

    let external_mod_path = TypePath {
        associated_type_path_prefix_opt: Some(vec![external_lib_root_id.clone()]),
        type_name: external_mod.module_name.clone(),
    };

    let external_func_path = TypePath {
        associated_type_path_prefix_opt: Some(vec![external_mod_path.to_identifier()]),
        type_name: external_func.function_name.clone(),
    };

    let mut symbols: SymbolTable = HashMap::new();
    symbols.insert(
        external_func_path.clone(),
        Symbol::Function {
            path: external_func_path,
            function: external_func,
        },
    );

    let mut table: SymbolTable = HashMap::new();
    table.insert(
        external_mod_path.clone(),
        Symbol::Module {
            path: external_mod_path,
            module: external_mod.clone(),
            symbols,
        },
    );

    let module = Module {
        name: external_mod.module_name,
        table,
    };

    let mut external_code: HashMap<Identifier, Vec<Module>> = HashMap::new();
    external_code.insert(external_lib_root_id, vec![module]);

    let input = r#" 
    import external_lib::external_mod::external_func;

    module some_mod { 
        struct SomeObject {}

        func some_func() -> SomeObject {
            external_func();
            SomeObject {}
        }
    }

    module another_mod {
        import lib::some_mod::{ SomeObject, some_func };

        struct AnotherObject {}

        func another_func() -> AnotherObject {
            external_func();
            AnotherObject {}
        }

        func call_some_func() -> SomeObject {
            some_func()
        }  
    }

    import lib::another_mod::{ AnotherObject, call_some_func, another_func };
    import lib::some_mod::SomeObject;
    
    func outer_func() -> SomeObject {
        call_some_func()
    }

    func call_another_func() -> AnotherObject {
        another_func()
    }
    "#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, Some(external_code))?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_let_stmt() -> Result<(), ()> {
    let input = r#"
    let a = 42;
    let b = 3.14;
    let c = (a as f64) + b;
    let d = [true, false, true, true];
    let e = (a, b, 12, d, "foo");
    "#;

    let (mut analyser, program) = setup(input, LogLevel::Debug, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_method_call() -> Result<(), ()> {
    let input = r#"
    module foo {
        #![contract]

        struct Foo {
            name: str,
            symbol: str,
            decimals: u64,
            total_supply: u64,
            balances: Mapping<h160, u256>
        }

        impl Foo {
            #[constructor]
            func new(name: str, symbol: str, balances: Mapping<h160, u256>) -> Foo {
                Foo {
                    name: name,
                    symbol: symbol,
                    decimals: 18,
                    total_supply: 1_000_000,
                    balances: balances
                }
            }
            
            func name(&self) -> str {
                self.name
            }

            func symbol(&self) -> str {
                self.symbol
            }
        }
    }

    import lib::foo::Foo;

    func main() {
        let foo = Foo::new("Foo", "FOO", { $0x12345_ABCDE_12345_ABCDE_12345_ABCDE_12345_ABCDE: 0x1234_ABCD });

        let name = foo.name();

        let symbol = foo.symbol();

        return;
    }"#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, None)
        .expect("unable to set up semantic analyser");

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_struct() -> Result<(), ()> {
    let input = r#"
    struct Foo { a: u64, b: str, c: u256 }

    impl Foo {
        func new(a: u64, b: str, c: u256) -> Foo {
            Foo {
                a: a,
                b: b,
                c: c
            }
        }
    }
    
    func add_a() -> f64 {
        let foo = Foo::new(42, "foo", 0x12345ABCDE);
        (foo.a as f64) + 3.14
    }

    func return_string() -> str {
        let foo = Foo::new(42, "foo", 0x12345ABCDE);
        foo.b
    }
    
    func subtract_c() -> u256 {
        let foo = Foo::new(42, "foo", 0x12345ABCDE);
        foo.c - (1_000 as u256)
    }"#;

    let (mut analyser, program) = setup(input, LogLevel::Trace, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}

#[test]
fn analyse_trait_def() -> Result<(), ()> {
    let input = r#"
    trait Contract {
        #![interface]

        const CONTRACT_ADDRESS: h160;
        const CREATOR_ADDRESS: h160;

        pub func address() -> h160;
        pub func balance(&self) -> u256;
        pub func msg_sender() -> h160;
        pub func creator_address() -> h160;
    }"#;

    let (mut analyser, program) = setup(input, LogLevel::Debug, false, false, None)?;

    match analyser.analyse_program(&program) {
        Ok(_) => Ok(()),
        Err(e) => Err(println!("{e:#?}")),
    }
}
