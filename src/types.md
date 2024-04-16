# Types

## Scalar Values

- `u8`: 8-bit unsigned integer. E.g., `let x: u8 = 255;`
- `u16`: 16-bit unsigned integer
- `u32`: 32-bit unsigned integer
- `u64`: 64-bit unsigned integer (default numeric type). E.g., `let x = 1234;`
- `u128`: 128-bit unsigned integer
- `u256`: 256-bit unsigned integer. E.g., `let x: u256 = 0x123456ABCDEF;`
- `u512`: 512-bit unsigned integer
- `h160`: 160-bit (20-byte) hash. E.g., `let owner: h160 = $0x12345123451234512345` (EVM address)
- `h256`: 256-bit (32-byte) hash. E.g., `let pub_key: h256 = $0x12345678123456781234567812345678;`
- `h512`: 512-bit (64-byte) hash
- `byte`: Single byte string. E.g., `let x = b"x";`
- `b2`, `b4`, `b8`, `b16`, `b32`: static byte string of 2, 4, 8, 16 or 32 bytes. E.g., `let x: b4 = b"foo";`
- `i32`: 32-bit signed integer. E.g., `let x: i32 = -2_147_483_648;`
- `i64`: 64-bit signed integer (default signed integer type). E.g., `let x = -1234;`
- `i128`: 128-bit signed integer
- `char`: 4-byte (`u32`) Unicode scalar value. E.g., `let x = 'ß';`
- `bool`: boolean value (`true` or `false`)

## Compound Values

- `[T; n]`: fixed-length array of type `T` with a capacity of `n` elements. E.g., `[1, 2, 3, 4]`
- `(T, U, ..)`: heterogeneous sequence of finite length (tuple). E.g., `(1, b"x", true, 0xABCDEF)`

## Dynamic Values

- `str`: growable string literal. E.g., `let foo = "bar";`
- `Vec<T>`: growable array of type `T`. E.g., `let vec: Vec<u256> = Vec::new();`
- `Mapping<K, V>`: store of key-value pairs. E.g., `let balances: Mapping<u160, u256>;`

## Other Types

- `func(T) -> U`: function pointer. E.g., `func(h160) -> u256`
- `&T` and `&mut T`: reference types. E.g., `&mut Mapping<u160, u256>`

## Additional Modules in `stdlib`

- `Option<T>`: enum that checks for a potential `None` value. Unwraps to `T` if the value is `Some`
- `Result<T, E>`: enum that allows for error propagation.  Unwraps to `T` if the value is `Ok`
