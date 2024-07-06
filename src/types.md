# Built-in Types

## Scalar Primitive Values

- `u8`: 8-bit unsigned integer. E.g., `let x: u8 = 255;`
- `u16`: 16-bit unsigned integer
- `u32`: 32-bit unsigned integer
- `u64`: 64-bit unsigned integer (default numeric type). E.g., `let x = 1234;`
- `u256`: 256-bit large unsigned integer (default large uint type). E.g., `let x = 0x123456ABCDEF;`
- `u512`: 512-bit large unsigned integer
- `i32`: 32-bit signed integer. E.g., `let x: i32 = -2_147_483_648;`
- `i64`: 64-bit signed integer (default signed integer type). E.g., `let x = -1234;`
- `f32`: 32-bit decimal floating-point literal. E.g., `let x: f32 = 12.34;`
- `f64`: 64-bit decimal floating-point literal (default float type). E.g., `let x = 12.34;`
- `h160`: 160-bit hash. E.g., `let owner: h160 = $0x12345123451234512345;` (EVM address)
- `h256`: 256-bit hash (default hash type). E.g., `let addr = $0x12345678123456781234567812345678;`
- `h512`: 512-bit hash
- `byte`: Single ASCII character (byte string). E.g., `let x: byte = b"x";`
- `b2`, `b4`, `b8`, `b16`: fixed-size byte string (2, 4, 8, 16 bytes). E.g., `let x: b4 = b"foo";`
- `b32`: 32-byte byte string (default byte string type). E.g., `let x = b"foobarbazfoobarbaz";`
- `char`: variable-size (1-4 bytes) Unicode scalar value. E.g., `let x = 'ß';` [not yet supported]
- `bool`: boolean value (`true` or `false`)

## Compound Primitive Values

- `[T; n]`: fixed-length array of type `T` with a capacity of `n` elements. E.g., `[1, 2, 3, 4]`
- `(T, U, ..)`: heterogeneous sequence of finite length (tuple). E.g., `(1, b"x", true, 0xABCDEF)`
- `str`: static string literal. E.g., `let foo = "bar";`

## Dynamic Types

- `String`: growable string. E.g., `let x = String::from("foobar");` [not yet supported]
- `Vec<T>`: growable array of type `T`. E.g., `let vec: Vec<u256> = Vec::new();`
- `Mapping<K, V>`: store of key-value pairs. E.g., `let balances: Mapping<h160, u256>;`

## Other Types

- `func(T) -> U`: function pointer. E.g.,
```
let balances: Mapping<h160, u256> = Mapping::new();

func get_balance(addr: h160) -> Option<u256> {
    balances.get(addr)
}

let balance_of: func(h160) -> Option<u256> = get_balance;

let alice_balance = balance_of($0x12345123451234512345).expect("balance not found");
```
- `&T` and `&mut T`: reference types. E.g., `let ref mut balances: &mut Mapping<h160, u256>;`

## Additional Modules in `stdlib` [not yet implemented]

- `Option<T>`: enum that checks for a potential `None` value. Unwraps to `T` if the value is `Some`
- `Result<T, E>`: enum that allows for error propagation.  Unwraps to `T` if the value is `Ok`
- `FixedPoint32`: struct that represents a fixed-point 32-bit decimal literal
```
struct FixedPoint32 {
    value: i32,
    fractional_bits: u8,
}
```
- `FixedPoint64`: struct that represents a fixed-point 64-bit decimal literal
- `Entity`: enum that represents an `Account` or a `Contract`. E.g.,
```
enum Entity<A: Hash, B> {
    Account { pub address: A, balance: Option<B> },
    Contract { pub address: A, balance: Option<B> },
}
```
