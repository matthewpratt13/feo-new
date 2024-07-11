use crate::{
    ast::{Identifier, ModuleItem, PathType, Visibility},
    logger::LogLevel,
    parser::{self, Module},
};

use super::*;

fn setup(
    input: &str,
    log_level: LogLevel,
    print_tokens: bool,
    print_statements: bool,
    external_code: Option<SymbolTable>,
) -> Result<(SemanticAnalyser, Module), ()> {
    let mut parser = parser::test_utils::get_parser(input, LogLevel::Debug, print_tokens);

    let module = match parser.parse_module() {
        Ok(m) => m,
        Err(_) => return Err(println!("{:#?}", parser.errors())),
    };

    if print_statements {
        println!("{:#?}", module.statements)
    }

    Ok((SemanticAnalyser::new(log_level, external_code), module))
}

#[test]
#[should_panic]
fn analyse_constant_reassign() {
    let input = r#"
    #[storage]
    const ADDRESS: h160 = $0x12345123451234512345;
    ADDRESS = $0x54321543215432154321;"#;

    let (mut analyser, module) = setup(input, LogLevel::Debug, false, false, None)
        .expect("unable to set up semantic analyser");

    match analyser.analyse_module(&module, PathType::from(Identifier::from(""))) {
        Ok(_) => println!("{:#?}", analyser.logger.messages()),
        Err(_) => panic!("{:#?}", analyser.logger.messages()),
    }
}

#[test]
fn analyse_import_decl() -> Result<(), ()> {
    let external_func = FunctionItem {
        attributes_opt: None,
        visibility: Visibility::Pub,
        kw_func: Keyword::Func,
        function_name: Identifier::from("external_func"),
        params_opt: None,
        return_type_opt: None,
        block_opt: None,
        span: Span::new("", 0, 0),
    };

    let external_module = ModuleItem {
        outer_attributes_opt: None,
        visibility: Visibility::Pub,
        kw_module: Keyword::Module,
        module_name: Identifier::from("external_module"),
        inner_attributes_opt: None,
        items_opt: Some(vec![Item::FunctionItem(external_func.clone())]),
        span: Span::new("", 0, 0),
    };

    let external_module_path = PathType {
        associated_type_path_prefix_opt: None,
        type_name: external_module.module_name.clone(),
    };

    let func_path = PathType {
        associated_type_path_prefix_opt: Some(Vec::<Identifier>::from(
            external_module_path.clone(),
        )),
        type_name: external_func.function_name.clone(),
    };

    let mut symbols: SymbolTable = HashMap::new();

    symbols.insert(
        func_path,
        Symbol::Function {
            path: PathType::from(external_func.function_name.clone()),
            function: external_func,
        },
    );

    let mut external_code: SymbolTable = HashMap::new();
    external_code.insert(
        external_module_path.clone(),
        Symbol::Module {
            path: external_module_path,
            module: external_module,
            symbols,
        },
    );

    let input = r#" 
    import external_module::external_func;

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
    }"#;

    let (mut analyser, module) = setup(input, LogLevel::Debug, false, false, Some(external_code))?;

    match analyser.analyse_module(&module, PathType::from(Identifier::from(""))) {
        Ok(_) => Ok(println!("{:#?}", analyser.logger.messages())),
        Err(_) => Err(println!("{:#?}", analyser.logger.messages())),
    }
}

#[test]
fn analyse_let_stmt() -> Result<(), ()> {
    let input = r#"
    let a = 42;
    let b = 3.14;
    let c = (a as f64) + b;"#;

    let (mut analyser, module) = setup(input, LogLevel::Debug, false, false, None)?;

    match analyser.analyse_module(&module, PathType::from(Identifier::from(""))) {
        Ok(_) => Ok(println!("{:#?}", analyser.logger.messages())),
        Err(_) => Err(println!("{:#?}", analyser.logger.messages())),
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

    let (mut analyser, module) = setup(input, LogLevel::Debug, false, false, None)?;

    match analyser.analyse_module(&module, PathType::from(Identifier::from(""))) {
        Ok(_) => Ok(println!("{:#?}", analyser.logger.messages())),
        Err(_) => Err(println!("{:#?}", analyser.logger.messages())),
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

    let (mut analyser, module) = setup(input, LogLevel::Debug, false, false, None)?;

    match analyser.analyse_module(&module, PathType::from(Identifier::from(""))) {
        Ok(_) => Ok(println!("{:#?}", analyser.logger.messages())),
        Err(_) => Err(println!("{:#?}", analyser.logger.messages())),
    }
}
