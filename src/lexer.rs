#![allow(dead_code)]

use std::{iter::Peekable, str::Chars};

use crate::{
    ast::{self, BigUIntKind, HashKind, IntKind, UIntKind},
    error::{CompilerError, ErrorsEmitted, LexErrorKind},
    span::Span,
    token::{Token, TokenStream},
    H256, U256,
};

/// Struct that stores an input string and contains methods to render tokens (tokenize)
/// from characters in that string.
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    peekable_chars: Peekable<Chars<'a>>,
    errors: Vec<CompilerError<LexErrorKind>>,
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance.
    /// Initialize an empty `Vec` to store potential errors, and create an `Iterator`
    /// from the characters in the source string to traverse the input string
    /// and look ahead in the source code without moving forward.
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            peekable_chars: input.chars().peekable(),
            errors: Vec::new(),
        }
    }

    /// Get a list of the lexer's `CompilerError`.
    pub(crate) fn errors(&self) -> &[CompilerError<LexErrorKind>] {
        &self.errors
    }

    /// Main tokenizing function.
    /// Returns a stream of tokens generated from some input string (source code).
    pub(crate) fn lex(&mut self) -> Result<TokenStream, ErrorsEmitted> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.peek_current() {
            let start_pos = self.pos;

            match c {
                _ if c.is_whitespace() => self.skip_whitespace(),

                _ if c == '/' && self.peek_next() == Some('/') || self.peek_next() == Some('*') => {
                    tokens.push(self.tokenize_doc_comment()?);
                }

                _ if c == 'b' && self.peek_next() == Some('\"') => {
                    self.advance();
                    tokens.push(self.tokenize_bytes()?)
                }

                // not alphanumeric because identifiers / keywords cannot start with numbers;
                // however, numbers are allowed after the first character
                _ if c.is_ascii_alphabetic() || c == '_' => {
                    tokens.push(self.tokenize_identifier_or_keyword()?)
                }

                _ if c == '#' && self.peek_next() == Some('!') || self.peek_next() == Some('[') => {
                    self.advance();

                    if self.peek_current() == Some('[') {
                        self.advance();
                        tokens.push(self.tokenize_identifier_or_keyword()?);
                    } else if self.peek_current() == Some('!') && self.peek_next() == Some('[') {
                        self.advance();
                        self.advance();
                        tokens.push(self.tokenize_identifier_or_keyword()?);
                    } else {
                        let current_char = self
                            .peek_current()
                            .ok_or(self.log_error(LexErrorKind::UnexpectedEndOfInput));

                        self.log_error(LexErrorKind::UnexpectedChar {
                            expected: "`[` or `!`".to_string(),
                            found: current_char.map_err(|_| ErrorsEmitted(()))?,
                        });

                        return Err(ErrorsEmitted(()));
                    }
                }

                '(' | '[' | '{' | ')' | ']' | '}' => {
                    tokens.push(self.tokenize_delimiter()?);
                }

                '"' => tokens.push(self.tokenize_string()?),

                '\'' => tokens.push(self.tokenize_char()?),

                '$' => tokens.push(self.tokenize_hash()?), // `h160`, `h256` or `h512` literal

                // hexadecimal digit prefix (`0x` or `0X`)
                _ if c == '0'
                    && self
                        .peek_next()
                        .is_some_and(|x| &x.to_lowercase().to_string() == "x") =>
                {
                    tokens.push(self.tokenize_big_uint()?)
                }

                _ if c.is_digit(10)
                    || (c == '-' && self.peek_next().is_some_and(|c| c.is_digit(10))) =>
                {
                    tokens.push(self.tokenize_numeric()?)
                }

                ',' => {
                    self.advance();
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Comma { punc: ',', span })
                }

                ';' => {
                    self.advance();
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Semicolon { punc: ';', span })
                }

                _ if c == '&' && self.peek_next() == Some('m') => {
                    self.advance();
                    if let Ok(Token::Mut { .. }) = self.tokenize_identifier_or_keyword() {
                        let span = Span::new(self.input, start_pos, self.pos);
                        tokens.push(Token::AmpersandMut {
                            punc: "&mut".to_string(),
                            span,
                        });
                    } else {
                        tokens.push(self.tokenize_punctuation()?)
                    }
                }

                '!' | '#' | '%' | '&' | '*' | '+' | '/' | '-' | '.' | ':' | '<' | '=' | '>'
                | '?' | '@' | '\\' | '^' | '`' | '|' => tokens.push(self.tokenize_punctuation()?),

                _ if !self.peek_next().is_some() => tokens.push(Token::EOF),

                _ => {
                    self.log_error(LexErrorKind::UnrecognizedChar {
                        value: format!("{}", c),
                    });
                    return Err(ErrorsEmitted(()));
                }
            }
        }

        let stream = TokenStream::new(&tokens, self.input, 0, self.pos);
        Ok(stream)
    }

    /// Tokenize a doc comment, ignoring ordinary line and block comments.
    fn tokenize_doc_comment(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip first `/`

        if let Some('/') = self.peek_current() {
            self.advance(); // skip second `/`

            if self.peek_current() == Some('/') || self.peek_current() == Some('!') {
                self.advance(); // skip third `/` or `!`
                self.skip_whitespace();

                let comment_start_pos = self.pos; // only store data after `///` and any whitespace

                // advance until the end of the line
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }

                let comment = self.input[comment_start_pos..self.pos].trim().to_string();

                self.advance();

                let span = Span::new(self.input, start_pos, self.pos);

                Ok(Token::DocComment { comment, span })
            } else {
                // consume ordinary newline or trailing comment (`//`)
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }

                // replace actual source code with `""` as ordinary comments are discarded
                Ok(Token::LineComment {
                    span: Span::new("", start_pos, self.pos),
                })
            }
        } else if let Some('*') = self.peek_current() {
            // consume inline or block comment (`/• •/`)
            self.advance(); // skip `*`

            while let Some(c) = self.peek_current() {
                if c == '*' {
                    self.advance(); // skip closing `*`
                    self.advance(); // skip closing `/`
                    break;
                }

                self.advance();
            }

            // replace actual source code with `""` as ordinary comments are discarded
            Ok(Token::BlockComment {
                span: Span::new("", start_pos, self.pos),
            })
        } else {
            // return the first `/` as a `Token::Slash` instead of aborting
            let span = Span::new(self.input, start_pos, self.pos);
            Ok(Token::Slash { punc: '/', span })
        }
    }

    /// Tokenize an identifier or reserved keyword.
    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.peek_current() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let name = self.input[start_pos..self.pos].trim().to_string();
        let span = Span::new(self.input, start_pos, self.pos);

        // check if the input is a reserved keyword (including booleans and type annotations)
        // or return a `Token::Identifier`
        if is_keyword(&name) {
            match name.as_str() {
                "abstract" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "alias" => Ok(Token::Alias { name, span }),
                "as" => Ok(Token::As { name, span }),
                "break" => Ok(Token::Break { name, span }),
                "calldata" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "constructor" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "const" => Ok(Token::Const { name, span }),
                "continue" => Ok(Token::Continue { name, span }),
                "contract" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "else" => Ok(Token::Else { name, span }),
                "enum" => Ok(Token::Enum { name, span }),
                "event" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "extern" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "false" => Ok(Token::BoolLiteral {
                    value: {
                        if let Ok(n) = name.parse::<bool>() {
                            n
                        } else {
                            self.log_error(LexErrorKind::ParseBoolError);
                            return Err(ErrorsEmitted(()));
                        }
                    },
                    span,
                }),
                "for" => Ok(Token::For { name, span }),
                "func" => Ok(Token::Func { name, span }),
                "if" => Ok(Token::If { name, span }),
                "impl" => Ok(Token::Impl { name, span }),
                "import" => Ok(Token::Import { name, span }),
                "interface" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "in" => Ok(Token::In { name, span }),
                "let" => Ok(Token::Let { name, span }),
                "library" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "loop" => Ok(Token::Loop { name, span }),
                "match" => Ok(Token::Match { name, span }),
                "modifier" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "mod" => Ok(Token::Mod { name, span }),
                "mut" => Ok(Token::Mut { name, span }),
                "package" => Ok(Token::Package { name, span }),
                "payable" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "pub" => Ok(Token::Pub { name, span }),
                "ref" => Ok(Token::Ref { name, span }),
                "return" => Ok(Token::Return { name, span }),
                "script" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "self" => Ok(Token::SelfKeyword { name, span }),
                "static" => Ok(Token::Static { name, span }),
                "storage" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "struct" => Ok(Token::Struct { name, span }),
                "super" => Ok(Token::Super { name, span }),
                "test" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "topic" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "trait" => Ok(Token::Trait { name, span }),
                "true" => Ok(Token::BoolLiteral {
                    value: {
                        if let Ok(n) = name.parse::<bool>() {
                            n
                        } else {
                            self.log_error(LexErrorKind::ParseBoolError);
                            return Err(ErrorsEmitted(()));
                        }
                    },

                    span,
                }),
                "unsafe" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "view" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted(()))
                    }
                }
                "while" => Ok(Token::While { name, span }),
                "i32" => Ok(Token::I32Type { name, span }),
                "i64" => Ok(Token::I64Type { name, span }),
                "i128" => Ok(Token::I128Type { name, span }),
                "u8" => Ok(Token::U8Type { name, span }),
                "u16" => Ok(Token::U16Type { name, span }),
                "u32" => Ok(Token::U32Type { name, span }),
                "u64" => Ok(Token::U64Type { name, span }),
                "u128" => Ok(Token::U128Type { name, span }),
                "u256" => Ok(Token::U256Type { name, span }),
                "u512" => Ok(Token::U512Type { name, span }),
                "byte" => Ok(Token::ByteType { name, span }),
                "b2" => Ok(Token::B2Type { name, span }),
                "b3" => Ok(Token::B3Type { name, span }),
                "b4" => Ok(Token::B4Type { name, span }),
                "b5" => Ok(Token::B5Type { name, span }),
                "b6" => Ok(Token::B6Type { name, span }),
                "b7" => Ok(Token::B7Type { name, span }),
                "b8" => Ok(Token::B8Type { name, span }),
                "b9" => Ok(Token::B9Type { name, span }),
                "b10" => Ok(Token::B10Type { name, span }),
                "b11" => Ok(Token::B11Type { name, span }),
                "b12" => Ok(Token::B12Type { name, span }),
                "b13" => Ok(Token::B13Type { name, span }),
                "b14" => Ok(Token::B14Type { name, span }),
                "b15" => Ok(Token::B15Type { name, span }),
                "b16" => Ok(Token::B16Type { name, span }),
                "b17" => Ok(Token::B17Type { name, span }),
                "b18" => Ok(Token::B18Type { name, span }),
                "b19" => Ok(Token::B19Type { name, span }),
                "b20" => Ok(Token::B20Type { name, span }),
                "b21" => Ok(Token::B21Type { name, span }),
                "b22" => Ok(Token::B22Type { name, span }),
                "b23" => Ok(Token::B23Type { name, span }),
                "b24" => Ok(Token::B24Type { name, span }),
                "b25" => Ok(Token::B25Type { name, span }),
                "b26" => Ok(Token::B26Type { name, span }),
                "b27" => Ok(Token::B27Type { name, span }),
                "b28" => Ok(Token::B28Type { name, span }),
                "b29" => Ok(Token::B29Type { name, span }),
                "b30" => Ok(Token::B30Type { name, span }),
                "b31" => Ok(Token::B31Type { name, span }),
                "b32" => Ok(Token::B32Type { name, span }),
                "h160" => Ok(Token::H160Type { name, span }),
                "h256" => Ok(Token::H256Type { name, span }),
                "h512" => Ok(Token::H512Type { name, span }),
                "String" => Ok(Token::StringType { name, span }),
                "char" => Ok(Token::CharType { name, span }),
                "bool" => Ok(Token::BoolType { name, span }),
                _ => {
                    self.log_error(LexErrorKind::UnrecognizedKeyword { name });
                    Err(ErrorsEmitted(()))
                }
            }
        } else {
            Ok(Token::Identifier { name, span })
        }
    }

    /// Match an input string against an outer attribute keyword.
    fn tokenize_outer_attribute(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<Token, ErrorsEmitted> {
        match name.as_str() {
            "abstract" => Ok(Token::Abstract { name, span }),
            "calldata" => Ok(Token::Calldata { name, span }),
            "extern" => Ok(Token::Extern { name, span }),
            "payable" => Ok(Token::Payable { name, span }),
            "storage" => Ok(Token::Storage { name, span }),
            "topic" => Ok(Token::Topic { name, span }),
            "unsafe" => Ok(Token::Unsafe { name, span }),
            "view" => Ok(Token::View { name, span }),
            _ => {
                self.log_error(LexErrorKind::UnrecognizedAttribute { name });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Match an input string against an inner attribute keyword.
    fn tokenize_inner_attribute(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<Token, ErrorsEmitted> {
        match name.as_str() {
            "constructor" => Ok(Token::Constructor { name, span }),
            "contract" => Ok(Token::Contract { name, span }),
            "error" => Ok(Token::Error { name, span }),
            "event" => Ok(Token::Event { name, span }),
            "interface" => Ok(Token::Interface { name, span }),
            "library" => Ok(Token::Library { name, span }),
            "modifier" => Ok(Token::Modifier { name, span }),
            "script" => Ok(Token::Script { name, span }),
            "test" => Ok(Token::Test { name, span }),
            _ => {
                self.log_error(LexErrorKind::UnrecognizedAttribute { name });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Tokenize a delimiter (i.e., `(`, `)`, `[`, `]`, `{` and `}`).
    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        let delim = self
            .peek_current()
            .ok_or(self.log_error(LexErrorKind::CharNotFound {
                expected: "delimiter".to_string(),
            }));

        match delim {
            Ok('(') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LParen { delim: '(', span })
            }

            Ok('[') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBracket { delim: '[', span })
            }

            Ok('{') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBrace { delim: '{', span })
            }

            Ok(')') => self.tokenize_closing_delimiter(')'),

            Ok(']') => self.tokenize_closing_delimiter(']'),

            Ok('}') => self.tokenize_closing_delimiter('}'),

            _ => {
                self.log_error(LexErrorKind::UnexpectedChar {
                    expected: "delimiter".to_string(),
                    found: delim.map_err(|_| ErrorsEmitted(()))?,
                });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Tokenize a closing delimiter, throw an error where the inputted delimiter is missing,
    /// or does not match the expected delimiter.
    fn tokenize_closing_delimiter(&mut self, expected: char) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        let curr = if let Some(c) = self.peek_current() {
            c
        } else {
            self.log_error(LexErrorKind::MissingDelimiter { delim: expected });
            return Err(ErrorsEmitted(()));
        };

        // check if the current character is a delimiter
        if !is_delimiter(curr) {
            self.log_error(LexErrorKind::UnexpectedChar {
                expected: "delimiter".to_string(),
                found: curr,
            });
            return Err(ErrorsEmitted(()));
        }

        if curr == expected {
            self.advance(); // move past closing delimiter

            match curr {
                ')' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RParen { delim: curr, span })
                }

                ']' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RBracket { delim: curr, span })
                }

                '}' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RBrace { delim: curr, span })
                }

                _ => {
                    self.log_error(LexErrorKind::UnexpectedChar {
                        expected: "closing delimiter".to_string(),
                        found: curr,
                    });

                    return Err(ErrorsEmitted(()));
                }
            }
        } else {
            self.log_error(LexErrorKind::MismatchedDelimiter {
                expected,
                found: curr,
            });
            Err(ErrorsEmitted(()))
        }
    }

    /// Tokenize a string literal, handling escape sequences where applicable.
    fn tokenize_string(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut value = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote (`"`)

        while let Some(c) = self.peek_current() {
            match c {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        value.push(escaped_char);
                        self.advance();
                    } else {
                        self.log_error(LexErrorKind::CharNotFound {
                            expected: "escape sequence".to_string(),
                        });
                    }
                }
                '"' => {
                    self.advance(); // skip closing quote (`"`)

                    let span = Span::new(self.input, start_pos, self.pos);

                    return Ok(Token::StringLiteral {
                        value: value.as_bytes().to_vec(),
                        span,
                    });
                }
                _ => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        self.log_error(LexErrorKind::MissingQuote { quote: '\"' });
        Err(ErrorsEmitted(()))
    }

    /// Tokenize a static byte array literal, handling escape sequences where applicable.
    fn tokenize_bytes(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut value = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote (`"`)

        while let Some(c) = self.peek_current() {
            match c {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        value.push(escaped_char);
                        self.advance();
                    } else {
                        self.log_error(LexErrorKind::CharNotFound {
                            expected: "escape sequence".to_string(),
                        });

                        return Err(ErrorsEmitted(()));
                    }
                }
                '"' => {
                    self.advance(); // skip closing quote (`"`)

                    let span = Span::new(self.input, start_pos, self.pos);

                    if value.len() == 1 {
                        return Ok(Token::ByteLiteral {
                            value: value.as_bytes()[0],
                            span,
                        });
                    }

                    let bytes = ast::get_bytes(value.as_bytes());

                    return Ok(Token::BytesLiteral { value: bytes, span });
                }
                _ => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        self.log_error(LexErrorKind::MissingQuote { quote: '\"' });
        Err(ErrorsEmitted(()))
    }

    /// Tokenize a `char` literal, handling escape sequences where applicable.
    fn tokenize_char(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip opening quote (`'`)

        if let Some(value) = self.peek_current() {
            match value {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        self.advance();

                        let span = Span::new(self.input, start_pos, self.pos);

                        Ok(Token::CharLiteral {
                            value: escaped_char,
                            span,
                        })
                    } else {
                        self.log_error(LexErrorKind::CharNotFound {
                            expected: "escape sequence".to_string(),
                        });
                        Err(ErrorsEmitted(()))
                    }
                }

                _ if value == '\'' || value == ' ' => {
                    self.log_error(LexErrorKind::EmptyCharLiteral);
                    Err(ErrorsEmitted(()))
                }

                _ => {
                    self.advance(); // advance to the next (regular) character

                    if let Some('\'') = self.peek_current() {
                        self.advance(); // skip closing quote (`'`)

                        let span = Span::new(self.input, start_pos, self.pos);

                        Ok(Token::CharLiteral { value, span })
                    } else {
                        self.log_error(LexErrorKind::MissingQuote { quote: '\'' });
                        Err(ErrorsEmitted(()))
                    }
                }
            }
        } else {
            self.log_error(LexErrorKind::CharNotFound {
                expected: "character literal".to_string(),
            });
            Err(ErrorsEmitted(()))
        }
    }

    /// Tokenize a big unsigned integer literal (i.e., `U256` and `U512`).
    /// The default big uint type is `U256`.
    fn tokenize_big_uint(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        // check for hexadecimal prefix (`0x`)
        if self.peek_current() == Some('0')
            && self
                .peek_next()
                .is_some_and(|x| &x.to_lowercase().to_string() == "x")
        {
            // consume prefix
            self.advance();
            self.advance();
        }

        // collect hexadecimal digits (may have `_` separators)
        while let Some(c) = self.peek_current() {
            if c.is_digit(16) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        // remove the `_` separators before parsing (if they exist)
        let value = self.input[start_pos..self.pos]
            .split('_')
            .collect::<Vec<&str>>()
            .concat()
            .parse::<U256>();

        let span = Span::new(self.input, start_pos, self.pos);

        if let Ok(v) = value {
            Ok(Token::BigUIntLiteral {
                value: BigUIntKind::U256(v),
                span,
            })
        } else {
            self.log_error(LexErrorKind::ParseBigUIntError);
            Err(ErrorsEmitted(()))
        }
    }

    /// Tokenize a hash literal (i.e., `H160`, `H256` and `H512`).
    /// The default hash type is `H256`.
    fn tokenize_hash(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip `$`

        if self.peek_current() == Some('0') && self.peek_next() == Some('x') {
            self.advance(); // skip `0`
            self.advance(); // skip `x`
        } else if self.peek_current().is_some_and(|x| x.is_digit(16)) {
            // it's okay if there is no `0x`, as long as the input is a valid hexadecimal digit
            ()
        } else {
            self.log_error(LexErrorKind::ParseHashError);
            return Err(ErrorsEmitted(()));
        }

        let hash_start_pos = self.pos;

        while let Some(c) = self.peek_current() {
            if c.is_digit(16) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let value = H256::from_slice(
            &self.input[hash_start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .as_bytes(),
        );

        let span = Span::new(self.input, start_pos, self.pos);

        Ok(Token::HashLiteral {
            value: HashKind::H256(value),
            span,
        })
    }

    /// Tokenize a numeric value (i.e., `i64` or `u64`).
    /// Parse to `u64` unless a `-` is encountered, in which case parse to `i64`.
    fn tokenize_numeric(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut is_negative = false;

        // check for `-` before the number to decide which type of integer to parse to
        if self.peek_current() == Some('-') && self.peek_next().is_some_and(|c| c.is_digit(10)) {
            is_negative = true;
            self.advance(); // skip `-`
        }

        // go back and read from previous character (`-``) if negative,
        // else read from current position
        let start_pos = if is_negative { self.pos - 1 } else { self.pos };

        // collect integers (may have `_` separators)
        while let Some(c) = self.peek_current() {
            if c.is_digit(10) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if is_negative {
            // remove the `_` separators before parsing (if they exist)
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<i64>();

            let span = Span::new(self.input, start_pos, self.pos);

            if let Ok(v) = value {
                Ok(Token::IntLiteral {
                    value: IntKind::I64(v),
                    span,
                })
            } else {
                self.log_error(LexErrorKind::ParseIntError);
                Err(ErrorsEmitted(()))
            }
        } else {
            // remove the `_` separators before parsing (if they exist)
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<u64>();

            let span = Span::new(self.input, start_pos, self.pos);

            if let Ok(v) = value {
                Ok(Token::UIntLiteral {
                    value: UIntKind::U64(v),
                    span,
                })
            } else {
                self.log_error(LexErrorKind::ParseUIntError);
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Tokenize punctuation.
    fn tokenize_punctuation(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.peek_current() {
            if c.is_ascii_punctuation() && !is_delimiter(c) && !is_separator(c) && !is_quote(c) {
                self.advance();
            } else {
                break;
            }
        }

        let punc = self.input[start_pos..self.pos].trim().to_string();

        let span = Span::new(self.input, start_pos, self.pos);

        match punc.as_str() {
            "." => Ok(Token::Dot { punc: '.', span }),
            ".." => Ok(Token::DblDot { punc, span }),
            "..=" => Ok(Token::DotDotEquals { punc, span }),
            "!" => Ok(Token::Bang { punc: '!', span }),
            "!=" => Ok(Token::BangEquals { punc, span }),
            "#" => Ok(Token::HashSign { punc: '#', span }),
            "#!" => Ok(Token::HashBang { punc, span }),
            "$" => {
                self.log_error(LexErrorKind::ReservedChar);
                Err(ErrorsEmitted(()))
            }
            "%" => Ok(Token::Percent { punc: '%', span }),
            "%=" => Ok(Token::PercentEquals { punc, span }),
            "&" => Ok(Token::Ampersand { punc: '&', span }),
            "&&" => Ok(Token::DblAmpersand { punc, span }),
            "+" => Ok(Token::Plus { punc: '+', span }),
            "+=" => Ok(Token::PlusEquals { punc, span }),
            "-" => Ok(Token::Minus { punc: '-', span }),
            "-=" => Ok(Token::MinusEquals { punc, span }),
            "->" => Ok(Token::ThinArrow { punc, span }),
            "*" => Ok(Token::Asterisk { punc: '*', span }),
            "**" => Ok(Token::DblAsterisk { punc, span }),
            "*=" => Ok(Token::AsteriskEquals { punc, span }),
            "/" => Ok(Token::Slash { punc: '/', span }),
            "/=" => Ok(Token::SlashEquals { punc, span }),
            ":" => Ok(Token::Colon { punc: ':', span }),
            "::" => Ok(Token::DblColon { punc, span }),
            "::*" => Ok(Token::ColonColonAsterisk { punc, span }),
            "=" => Ok(Token::Equals { punc: '=', span }),
            "==" => Ok(Token::DblEquals { punc, span }),
            "=>" => Ok(Token::FatArrow { punc, span }),
            "<" => Ok(Token::LessThan { punc: '<', span }),
            "<<" => Ok(Token::DblLessThan { punc, span }),
            "<=" => Ok(Token::LessThanEquals { punc, span }),
            ">" => Ok(Token::GreaterThan { punc: '>', span }),
            ">>" => Ok(Token::DblGreaterThan { punc, span }),
            ">=" => Ok(Token::GreaterThanEquals { punc, span }),
            "@" => Ok(Token::AtSign { punc: '@', span }),
            "?" => Ok(Token::QuestionMark { punc: '?', span }),
            "\\" => Ok(Token::Backslash { punc: '\\', span }),
            "^" => Ok(Token::Caret { punc: '^', span }),
            "`" => Ok(Token::Backtick { punc: '`', span }),
            "|" => Ok(Token::Pipe { punc: '|', span }),
            "||" => Ok(Token::DblPipe { punc, span }),
            _ => {
                self.log_error(LexErrorKind::UnrecognizedChar { value: punc });
                Err(ErrorsEmitted(()))
            }
        }
    }

    /// Parse an escape sequence found in a string or character literal.
    fn parse_escape_sequence(&mut self) -> Result<Option<char>, ErrorsEmitted> {
        self.advance(); // skip backslash

        if let Some(c) = self.peek_current() {
            let escaped_char = match c {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '0' => '\0',
                '\'' => '\'',
                '\"' => '\"',
                _ => {
                    self.log_error(LexErrorKind::UnrecognizedEscapeSequence { sequence: c });
                    return Err(ErrorsEmitted(()));
                }
            };

            self.advance(); // skip escape sequence

            Ok(Some(escaped_char))
        } else {
            // incomplete escape sequence
            self.log_error(LexErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted(()))
        }
    }

    /// Advance the scanner (and iterator) by one character.
    fn advance(&mut self) {
        self.pos += 1; // update lexer's position
        self.peekable_chars.next(); // move to next character in the iterator (discard output)
    }

    /// Get the character at the current position.
    fn peek_current(&self) -> Option<char> {
        self.peekable_chars.clone().peek().cloned()
    }

    /// Get the next character without advancing the iterator.
    fn peek_next(&self) -> Option<char> {
        let mut cloned_iter = self.peekable_chars.clone();
        cloned_iter.next();
        cloned_iter.peek().cloned()
    }

    /// Skip the source string's whitespace, which is considered unnecessary.
    fn skip_whitespace(&mut self) {
        while let Some(c) = self.peek_current() {
            if c.is_whitespace() {
                // if the current character is whitespace, advance to the next
                self.advance();
            } else {
                // if the current character is not whitespace, break out of the loop
                break;
            }
        }
    }

    /// Log information about an error that occurred during tokenization.
    fn log_error(&mut self, error_kind: LexErrorKind) {
        let error = CompilerError::new(error_kind, self.input, self.pos);

        self.errors.push(error);
    }
}

/// List of reserved keywords to match against some input string.
fn is_keyword(value: &str) -> bool {
    [
        "abstract",
        "alias",
        "as",
        "break",
        "bytes",
        "calldata",
        "constructor",
        "const",
        "continue",
        "contract",
        "else",
        "enum",
        "error",
        "event",
        "extern",
        "false",
        "for",
        "func",
        "if",
        "impl",
        "import",
        "interface",
        "in",
        "let",
        "library",
        "loop",
        "match",
        "modifier",
        "mod",
        "mut",
        "package",
        "payable",
        "pub",
        "ref",
        "return",
        "script",
        "self",
        "static",
        "storage",
        "struct",
        "super",
        "test",
        "topic",
        "trait",
        "true",
        "unsafe",
        "view",
        "while",
        "i32",
        "i64",
        "i128",
        "u8",
        "u16",
        "u32",
        "u64",
        "u128",
        "u256",
        "u512",
        "byte",
        "b2",
        "b3",
        "b4",
        "b5",
        "b6",
        "b7",
        "b8",
        "b9",
        "b10",
        "b11",
        "b12",
        "b13",
        "b14",
        "b15",
        "b16",
        "b17",
        "b18",
        "b19",
        "b20",
        "b21",
        "b22",
        "b23",
        "b24",
        "b25",
        "b26",
        "b27",
        "b28",
        "b29",
        "b30",
        "b31",
        "b32",
        "h160",
        "h256",
        "h512",
        "String",
        "char",
        "bool",
    ]
    .contains(&value)
}

/// List of delimiters to match against some input `char`.
fn is_delimiter(value: char) -> bool {
    ['(', ')', '{', '}', '[', ']'].contains(&value)
}

/// List of separators to match against some input `char`.
fn is_separator(value: char) -> bool {
    [';', ',', '_'].contains(&value)
}

/// List of recognized quote characters (for `char` and string literals) to match against
/// some input `char`.
fn is_quote(value: char) -> bool {
    ['\'', '\"'].contains(&value)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn tokenize_assignment_stmt() {
        let input = r#"let x: u256 = 0x1234_ABCD; // inline comment
        // line comment
        /*
        block comment
        */
        foo /* don't print */ bar
        let baz: byte = b"x";
        "#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_attributes() {
        let input = r#"
        #[abstract]
        #![contract]
        pub mod foo {
            #[storage]
            const balances: [U256] = [0x1234, 0x5678, 0x90AB, 0xCDEF];
            
            #![interface]
            trait Bar

            #![constructor]
            pub func new() -> Contract
            
            #[extern]
            #[modifier]
            pub func baz();
        }"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: {:?}\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_char_lit() {
        let input = r#"let lit = 'a';
        let esc = '\\';
        let another_esc = '\'';"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: {:?}\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_function() {
        let input = r#"
        func foo(param1: u64, param2: char, param3: bool) -> ReturnType {
            let bar: b3 = b"bar";
            
            if (param3) {
                print("{}", param2);
            } else {
                print(bar);
            }

            for x in y {
                param1 += 2;
            }

            return ReturnType {
                baz: "hello world"
            }
        }
        "#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors(),
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_hash_lits() {
        let input = r#"let hash = $0xBEEF_CAFE_1234_5678_90AB_CDEF_CAFE_BEEF;"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_import_decl() {
        let input = r#"import package::module::Object as Foo;"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_struct_def() {
        let input = r#"
        /// These is a doc comment
        /// for the struct called `Foo`.
        pub struct Foo {
            bar: String,
            baz: u64,
        }"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors(),
        ));

        println!("{:#?}", stream);
    }
}
