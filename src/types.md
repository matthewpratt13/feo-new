# Types

## Scalar Values

- `char`: 8-bit (`u8`) ASCII value
- `i32`: 32-bit signed integer
- `i64`: 64-bit signed integer
- `i128`: 128-bit signed integer
- `u8`: 8-bit unsigned integer (one byte)
- `u16`: 16-bit unsigned integer
- `u32`: 32-bit unsigned integer
- `u64`: 64-bit unsigned integer (default numeric type)
- `u128`: 128-bit unsigned integer
- `u256`: 256-bit unsigned integer (notated in hexadecimal format)
- `h160`: 160-bit (20-byte) hash; represents an EVM smart contract or EOA address
- `h256`: 256-bit (32-byte) hash
- `b2`–`b32`: static byte array (string literals) of 2–32 bytes
- `bool`: boolean value (`true` or `false`)

## Compound Values

- `[T; n]`: fixed-length array of a single type `T` with precisely `n` elements
- `(T, U, ..)`: heterogeneous sequence of finite length (tuple)

## Dynamic Values

- `String`: growable array of `u8`
- `Vec<T>`: growable array of type `T`
- `Mapping<K, V>`: data type that stores key-value pairs

## Other Types

- `func(T) -> U`: function pointer
- `&T` and `&mut T`: reference types

## Additional Modules in `stdlib`

- `Option<T>`: enum that checks for a potential `None` value. Unwraps to `T` if the value is `Some`
- `Result<T, E>`: enum that allows for error propagation.  Unwraps to `T` if the value is `Ok`
