//! ## Lexer
//!
//! Tokenizes identifiers (sequences of alphabetic characters), keywords, numbers,
//! hash literals, static byte arrays, character and string literals, delimiters, doc comments
//! and punctuation. It ignores ordinary comments and stops tokenizing when it reaches
//! the end of the input string.

use core::{iter::Peekable, str::Chars};

use crate::{
    ast::{BigUInt, Bool, Byte, Bytes, Char, Float, Int, Str, UInt},
    error::{CompilerError, ErrorsEmitted, LexErrorKind},
    span::Span,
    token::{DocCommentType, Token, TokenStream, TokenType},
    B16, B2, B32, B4, B8, H160, H256, H512, U256, U512,
};

/// Struct that stores an input string (source code) and contains methods to render tokens
/// from characters in that string (i.e., tokenization).
pub(crate) struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    peekable_chars: Peekable<Chars<'a>>,
    errors: Vec<CompilerError<LexErrorKind>>, // store lexer errors
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance.
    /// Initialize an empty `Vec` to store potential errors, and create an `Iterator`
    /// from the characters in the input string to traverse the contents and look ahead
    /// without advancing the lexer.
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            peekable_chars: input.chars().peekable(),
            errors: Vec::new(),
        }
    }

    /// Main tokenizing function.
    /// Returns a stream of tokens generated from some input string (source code).
    pub(crate) fn lex(&mut self) -> Result<TokenStream, ErrorsEmitted> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.peek_current() {
            let start_pos = self.pos;

            match c {
                _ if c.is_whitespace() => self.skip_whitespace(),

                _ if c == '/'
                    && (self.peek_next() == Some('/') || self.peek_next() == Some('*')) =>
                {
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
                            found: current_char.map_err(|_| ErrorsEmitted)?,
                        });

                        return Err(ErrorsEmitted);
                    }
                }

                '(' | '[' | '{' | ')' | ']' | '}' => {
                    tokens.push(self.tokenize_delimiter()?);
                }

                '"' => tokens.push(self.tokenize_str()?),

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

                '.' => match &self.peek_next() {
                    Some('.') => tokens.push(self.tokenize_punctuation()?),
                    _ => {
                        self.advance();
                        let span = Span::new(self.input, start_pos, self.pos);
                        tokens.push(Token::Dot { punc: '.', span })
                    }
                },

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

                '!' | '%' | '&' | '*' | '+' | '/' | '-' | ':' | '<' | '=' | '>' | '?' | '\\'
                | '^' | '|' => tokens.push(self.tokenize_punctuation()?),

                _ => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::UnrecognizedChar { punc: c, span });

                    self.log_error(LexErrorKind::UnrecognizedChar {
                        value: format!("{}", c),
                    });

                    self.advance();
                }
            }
        }

        if !self.errors().is_empty() {
            return Err(ErrorsEmitted);
        }

        Ok(TokenStream::new(tokens, self.input, 0, self.pos))
    }

    /// Tokenize a doc comment by advancing the lexer position until the end of line, ignoring
    /// ordinary line and block comments.
    fn tokenize_doc_comment(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip first `/`

        match &self.peek_current() {
            Some('/') if self.peek_next() == Some('/') || self.peek_current() == Some('!') => {
                let comment_type = if self.peek_current() == Some('/') {
                    DocCommentType::OuterDocComment
                } else {
                    DocCommentType::InnerDocComment
                };

                self.advance(); // skip third `/` or `!`
                self.skip_whitespace();

                let comment_start_pos = self.pos; // only store data after `///`

                // advance until the end of the line
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }

                let comment = self.input[comment_start_pos..self.pos].trim().to_string();

                let span = Span::new(self.input, start_pos, self.pos);

                self.advance();

                Ok(Token::DocComment {
                    comment,
                    span,
                    comment_type,
                })
            }
            Some('/') => {
                // consume ordinary newline or trailing comment (`//`)
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }

                // replace actual source code with `""`, as ordinary comments are discarded
                Ok(Token::LineComment {
                    span: Span::new("", start_pos, self.pos),
                })
            }
            Some('*') => {
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

                // replace actual source code with `""`, as ordinary comments are discarded
                Ok(Token::BlockComment {
                    span: Span::new("", start_pos, self.pos),
                })
            }
            _ => {
                // return the first `/` as a `Token::Slash` instead of aborting
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::Slash { punc: '/', span })
            }
        }
    }

    /// Tokenize an identifier, keyword or attribute by advancing the lexer position until
    /// a non-alphanumeric character (excluding underscores) is encountered, and then checking
    /// if the extracted value is a keyword or attribute. Keywords and attributes are reserved
    /// words in Feo, whereas identifiers are user-defined names for variables, functions, etc.
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

        if is_keyword(&name) | is_attribute(&name) {
            match name.as_str() {
                "alias" => Ok(Token::Alias { name, span }),
                "as" => Ok(Token::As { name, span }),
                "break" => Ok(Token::Break { name, span }),
                "const" => Ok(Token::Const { name, span }),
                "continue" => Ok(Token::Continue { name, span }),
                "else" => Ok(Token::Else { name, span }),
                "enum" => Ok(Token::Enum { name, span }),
                "Err" => Ok(Token::Err { name, span }),
                "false" => Ok(Token::BoolLiteral {
                    value: {
                        if let Ok(n) = name.parse::<bool>() {
                            Bool::from(n)
                        } else {
                            self.log_error(LexErrorKind::ParseBoolError);
                            return Err(ErrorsEmitted);
                        }
                    },
                    span,
                }),
                "for" => Ok(Token::For { name, span }),
                "func" => Ok(Token::Func { name, span }),
                "if" => Ok(Token::If { name, span }),
                "impl" => Ok(Token::Impl { name, span }),
                "import" => Ok(Token::Import { name, span }),
                "in" => Ok(Token::In { name, span }),
                "let" => Ok(Token::Let { name, span }),
                "loop" => Ok(Token::Loop { name, span }),
                "match" => Ok(Token::Match { name, span }),
                "module" => Ok(Token::Module { name, span }),
                "mut" => Ok(Token::Mut { name, span }),
                "Ok" => Ok(Token::Ok { name, span }),
                "None" => Ok(Token::None { name, span }),
                "package" => Ok(Token::Package { name, span }),
                "pub" => Ok(Token::Pub { name, span }),
                "ref" => Ok(Token::Ref { name, span }),
                "return" => Ok(Token::Return { name, span }),
                "self" => Ok(Token::SelfKeyword { name, span }),
                "Some" => Ok(Token::Some { name, span }),
                "static" => Ok(Token::Static { name, span }),
                "struct" => Ok(Token::Struct { name, span }),
                "super" => Ok(Token::Super { name, span }),
                "trait" => Ok(Token::Trait { name, span }),
                "true" => Ok(Token::BoolLiteral {
                    value: {
                        if let Ok(n) = name.parse::<bool>() {
                            Bool::from(n)
                        } else {
                            self.log_error(LexErrorKind::ParseBoolError);
                            return Err(ErrorsEmitted);
                        }
                    },

                    span,
                }),
                "while" => Ok(Token::While { name, span }),
                "i32" => Ok(Token::I32Type { name, span }),
                "i64" => Ok(Token::I64Type { name, span }),
                "u8" => Ok(Token::U8Type { name, span }),
                "u16" => Ok(Token::U16Type { name, span }),
                "u32" => Ok(Token::U32Type { name, span }),
                "u64" => Ok(Token::U64Type { name, span }),
                "u256" => Ok(Token::U256Type { name, span }),
                "u512" => Ok(Token::U512Type { name, span }),
                "f32" => Ok(Token::F32Type { name, span }),
                "f64" => Ok(Token::F64Type { name, span }),
                "byte" => Ok(Token::ByteType { name, span }),
                "b2" => Ok(Token::B2Type { name, span }),
                "b4" => Ok(Token::B4Type { name, span }),
                "b8" => Ok(Token::B8Type { name, span }),
                "b16" => Ok(Token::B16Type { name, span }),
                "b32" => Ok(Token::B32Type { name, span }),
                "h160" => Ok(Token::H160Type { name, span }),
                "h256" => Ok(Token::H256Type { name, span }),
                "h512" => Ok(Token::H512Type { name, span }),
                "String" => Ok(Token::StringType { name, span }),
                "str" => Ok(Token::StrType { name, span }),
                "char" => Ok(Token::CharType { name, span }),
                "bool" => Ok(Token::BoolType { name, span }),
                "Self" => Ok(Token::SelfType { name, span }),
                "Option" => Ok(Token::OptionType { name, span }),
                "Result" => Ok(Token::ResultType { name, span }),
                "Vec" => Ok(Token::VecType { name, span }),
                "Mapping" => Ok(Token::MappingType { name, span }),
                "calldata" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "abstract" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "constructor" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "contract" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "error" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "event" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "extern" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "interface" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }

                "library" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "modifier" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "payable" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "script" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "storage" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "test" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "topic" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "unsafe" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_inner_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                "view" => {
                    if let Some(']') = self.peek_current() {
                        let token = self.tokenize_outer_attribute(name, span);
                        self.advance();
                        token
                    } else {
                        self.log_error(LexErrorKind::MissingDelimiter { delim: ']' });
                        Err(ErrorsEmitted)
                    }
                }
                _ => {
                    self.log_error(LexErrorKind::UnrecognizedKeyword { name });
                    Err(ErrorsEmitted)
                }
            }
        } else {
            Ok(Token::Identifier { name, span })
        }
    }

    /// Match an input string against an outer attribute (prefix `#`) keyword.
    fn tokenize_outer_attribute(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<Token, ErrorsEmitted> {
        match name.as_str() {
            "calldata" => Ok(Token::Calldata { name, span }),
            "constructor" => Ok(Token::Constructor { name, span }),
            "error" => Ok(Token::Error { name, span }),
            "event" => Ok(Token::Event { name, span }),
            "extern" => Ok(Token::Extern { name, span }),
            "modifier" => Ok(Token::Modifier { name, span }),
            "payable" => Ok(Token::Payable { name, span }),
            "storage" => Ok(Token::Storage { name, span }),
            "test" => Ok(Token::Test { name, span }),
            "topic" => Ok(Token::Topic { name, span }),
            "view" => Ok(Token::View { name, span }),

            _ => {
                self.log_error(LexErrorKind::UnrecognizedOuterAttribute { name });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Match an input string against an inner attribute (prefix `#!`) keyword.
    fn tokenize_inner_attribute(
        &mut self,
        name: String,
        span: Span,
    ) -> Result<Token, ErrorsEmitted> {
        match name.as_str() {
            "abstract" => Ok(Token::Abstract { name, span }),
            "contract" => Ok(Token::Contract { name, span }),
            "interface" => Ok(Token::Interface { name, span }),
            "library" => Ok(Token::Library { name, span }),
            "script" => Ok(Token::Script { name, span }),
            "unsafe" => Ok(Token::Unsafe { name, span }),

            _ => {
                self.log_error(LexErrorKind::UnrecognizedInnerAttribute { name });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Tokenize a delimiter (i.e., `(`, `)`, `[`, `]`, `{` and `}`) and advance the lexer past it.
    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        let delim = &self.peek_current();

        match delim {
            Some('(') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LParen { delim: '(', span })
            }

            Some('[') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBracket { delim: '[', span })
            }

            Some('{') => {
                self.advance(); // move past opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBrace { delim: '{', span })
            }

            Some(')') => self.tokenize_closing_delimiter(')'),

            Some(']') => self.tokenize_closing_delimiter(']'),

            Some('}') => self.tokenize_closing_delimiter('}'),

            Some(d) => {
                self.log_error(LexErrorKind::UnexpectedChar {
                    expected: "delimiter".to_string(),
                    found: *d,
                });
                Err(ErrorsEmitted)
            }

            None => {
                self.log_error(LexErrorKind::CharNotFound {
                    expected: "delimiter".to_string(),
                });
                Err(ErrorsEmitted)
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
            return Err(ErrorsEmitted);
        };

        // check if the current character is a delimiter
        if !is_delimiter(curr) {
            self.log_error(LexErrorKind::UnexpectedChar {
                expected: "delimiter".to_string(),
                found: curr,
            });
            return Err(ErrorsEmitted);
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

                    return Err(ErrorsEmitted);
                }
            }
        } else {
            self.log_error(LexErrorKind::MismatchedDelimiter {
                expected,
                found: curr,
            });
            Err(ErrorsEmitted)
        }
    }

    /// Tokenize a string literal, handling escape sequences where applicable.
    fn tokenize_str(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut buf = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote (`"`)

        while let Some(c) = self.peek_current() {
            match c {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        buf.push(escaped_char);
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

                    return Ok(Token::StrLiteral {
                        value: Str::from(buf.as_str()),
                        span,
                    });
                }
                _ => {
                    buf.push(c);
                    self.advance();
                }
            }
        }

        self.log_error(LexErrorKind::MissingQuote { quote: '\"' });
        Err(ErrorsEmitted)
    }

    /// Tokenize a fixed-length byte string (`Bytes`), handling escape sequences where applicable.
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

                        return Err(ErrorsEmitted);
                    }
                }
                '"' => {
                    self.advance(); // skip closing quote (`"`)

                    let span = Span::new(self.input, start_pos, self.pos);

                    if value.len() == 1 {
                        return Ok(Token::ByteLiteral {
                            value: Byte::from(value.as_bytes()[0]),
                            span,
                        });
                    }

                    let bytes = get_bytes(value.as_bytes());

                    return Ok(Token::BytesLiteral { value: bytes, span });
                }
                _ => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        self.log_error(LexErrorKind::MissingQuote { quote: '\"' });
        Err(ErrorsEmitted)
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
                            value: Char::from(escaped_char),
                            span,
                        })
                    } else {
                        self.log_error(LexErrorKind::CharNotFound {
                            expected: "escape sequence".to_string(),
                        });
                        Err(ErrorsEmitted)
                    }
                }

                _ if value == '\'' || value == ' ' => {
                    self.log_error(LexErrorKind::EmptyCharLiteral);
                    Err(ErrorsEmitted)
                }

                _ => {
                    self.advance(); // advance to the next (regular) character

                    if let Some('\'') = self.peek_current() {
                        self.advance(); // skip closing quote (`'`)

                        let span = Span::new(self.input, start_pos, self.pos);

                        Ok(Token::CharLiteral {
                            value: Char::from(value),
                            span,
                        })
                    } else {
                        self.log_error(LexErrorKind::MissingQuote { quote: '\'' });
                        Err(ErrorsEmitted)
                    }
                }
            }
        } else {
            self.log_error(LexErrorKind::CharNotFound {
                expected: "character literal".to_string(),
            });
            Err(ErrorsEmitted)
        }
    }

    /// Tokenize a large unsigned integer literal (i.e., `u256` and `u512`).
    fn tokenize_big_uint(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut suffix_opt = None::<TokenType>;

        let start_pos = self.pos;

        if let Some(c) = self.peek_current() {
            // check for hexadecimal prefix (`0x`)
            if c == '0' && self.peek_next() == Some('x') {
                // consume prefix
                self.advance();
                self.advance();
            } else {
                self.log_error(LexErrorKind::UnexpectedHexadecimalPrefix { prefix: c });
                return Err(ErrorsEmitted);
            }
        } else {
            self.log_error(LexErrorKind::CharNotFound {
                expected: "`0x` prefix".to_string(),
            });
            return Err(ErrorsEmitted);
        }

        // collect hexadecimal digits (may have `_` separators)
        while let Some(c) = self.peek_current() {
            if c.is_digit(16) || c == '_' {
                self.advance();
            } else if let Ok(t) = self.tokenize_identifier_or_keyword() {
                match t {
                    Token::U256Type { .. } => suffix_opt = Some(TokenType::U256Type),
                    Token::U512Type { .. } => suffix_opt = Some(TokenType::U512Type),

                    _ => suffix_opt = None,
                }
                break;
            } else {
                break;
            }
        }

        // remove the `_` separators before parsing (if they exist)
        let value_string = self.input[start_pos..self.pos]
            .split('_')
            .collect::<Vec<&str>>()
            .concat();

        let span = Span::new(self.input, start_pos, self.pos);

        if let Some(s) = suffix_opt {
            match s {
                TokenType::U256Type => {
                    if let Ok(v) = value_string.trim_end_matches("u256").parse::<U256>() {
                        Ok(Token::BigUIntLiteral {
                            value: BigUInt::U256(v),
                            span,
                        })
                    } else {
                        self.log_error(LexErrorKind::ParseBigUIntError);
                        Err(ErrorsEmitted)
                    }
                }

                TokenType::U512Type => {
                    if let Ok(v) = value_string.trim_end_matches("u512").parse::<U512>() {
                        Ok(Token::BigUIntLiteral {
                            value: BigUInt::U512(v),
                            span,
                        })
                    } else {
                        self.log_error(LexErrorKind::ParseBigUIntError);
                        Err(ErrorsEmitted)
                    }
                }
                tt => {
                    self.log_error(LexErrorKind::UnexpectedBigUIntSuffix {
                        suffix: tt.to_string(),
                    });
                    Err(ErrorsEmitted)
                }
            }
        } else {
            if let Ok(v) = value_string.parse::<U512>() {
                Ok(Token::BigUIntLiteral {
                    value: BigUInt::U512(v),
                    span,
                })
            } else {
                self.log_error(LexErrorKind::ParseBigUIntError);
                Err(ErrorsEmitted)
            }
        }
    }

    /// Tokenize a hash literal (i.e., `h160`, `h256` and `h512`), starting with `$`.
    fn tokenize_hash(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip `$`

        if let Some(c) = self.peek_current() {
            // check for hexadecimal prefix (`0x`)
            if c == '0' && self.peek_next() == Some('x') {
                self.advance(); // skip `0`
                self.advance(); // skip `x`
            } else if self.peek_current().is_some_and(|x| x.is_digit(16)) {
                // it's okay if there is no `0x`, as long as the input is a valid hexadecimal digit
                ()
            } else {
                self.log_error(LexErrorKind::UnexpectedHexadecimalPrefix { prefix: c });
                return Err(ErrorsEmitted);
            }
        } else {
            self.log_error(LexErrorKind::CharNotFound {
                expected: "`0x` prefix".to_string(),
            });
            return Err(ErrorsEmitted);
        }

        let hash_start_pos = self.pos;

        while let Some(c) = self.peek_current() {
            if c.is_digit(16) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let hash = self.input[hash_start_pos..self.pos]
            .split('_')
            .collect::<Vec<&str>>()
            .concat();

        let span = Span::new(self.input, start_pos, self.pos);

        match hash.len() {
            20 => {
                let value = H160::from_slice(hash.as_bytes());

                Ok(Token::HashLiteral {
                    value: crate::ast::Hash::H160(value),
                    span,
                })
            }

            32 => {
                let value = H256::from_slice(hash.as_bytes());

                Ok(Token::HashLiteral {
                    value: crate::ast::Hash::H256(value),
                    span,
                })
            }

            64 => {
                let value = H512::from_slice(hash.as_bytes());

                Ok(Token::HashLiteral {
                    value: crate::ast::Hash::H512(value),
                    span,
                })
            }

            _ => {
                self.log_error(LexErrorKind::InvalidHashLength { len: hash.len() });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Tokenize a numeric value (i.e., `f64`, `i64` or `u64`).
    /// Parse to `u64` unless a `-` is encountered, in which case parse to `i64`.
    /// If a `.` is encountered, try to parse to `f64`.
    fn tokenize_numeric(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut is_float = false;
        let mut is_negative = false;
        let mut suffix_opt = None::<TokenType>;

        // check for `-` before the number to decide which type of integer to parse to later
        if let Some('-') = self.peek_current() {
            let next = self.peek_next();

            if next.is_some() {
                if next.unwrap().is_digit(10) {
                    is_negative = true;
                    self.advance(); // skip `-`
                } else {
                    self.log_error(LexErrorKind::UnexpectedChar {
                        expected: "digit".to_string(),
                        found: next.unwrap(),
                    });
                    return Err(ErrorsEmitted);
                }
            } else {
                self.log_error(LexErrorKind::CharNotFound {
                    expected: "digit".to_string(),
                });
                return Err(ErrorsEmitted);
            }
        }

        // go back and read from previous character (`-`) if negative,
        // else read from current position
        let start_pos = if is_negative { self.pos - 1 } else { self.pos };

        // collect integers (may have `_` separators)
        while let Some(c) = self.peek_current() {
            if c.is_digit(10) || c == '_' {
                self.advance();
            } else if c == '.' && !is_float && self.peek_next() != Some('.') {
                is_float = true;
                self.advance();
            } else if let Ok(t) = self.tokenize_identifier_or_keyword() {
                match t {
                    Token::U8Type { .. } => suffix_opt = Some(TokenType::U8Type),
                    Token::U16Type { .. } => suffix_opt = Some(TokenType::U16Type),
                    Token::U32Type { .. } => suffix_opt = Some(TokenType::U32Type),
                    Token::U64Type { .. } => suffix_opt = Some(TokenType::U64Type),
                    Token::I32Type { .. } => suffix_opt = Some(TokenType::I32Type),
                    Token::I64Type { .. } => suffix_opt = Some(TokenType::I64Type),
                    Token::F32Type { .. } => suffix_opt = Some(TokenType::F32Type),
                    Token::F64Type { .. } => suffix_opt = Some(TokenType::F64Type),
                    _ => suffix_opt = None,
                }
                break;
            } else {
                break;
            }
        }

        if let Some(s) = suffix_opt {
            return self.tokenize_numeric_suffix(start_pos, s);
        }

        if is_float {
            return self.tokenize_numeric_suffix(start_pos, TokenType::F64Type);
        }

        if is_negative {
            self.tokenize_numeric_suffix(start_pos, TokenType::I64Type)
        } else {
            self.tokenize_numeric_suffix(start_pos, TokenType::U64Type)
        }
    }

    /// Tokenize punctuation (i.e., operators and separators).
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
            "?" => Ok(Token::QuestionMark { punc: '?', span }),
            "\\" => Ok(Token::Backslash { punc: '\\', span }),
            "^" => Ok(Token::Caret { punc: '^', span }),
            "|" => Ok(Token::Pipe { punc: '|', span }),
            "||" => Ok(Token::DblPipe { punc, span }),
            _ => {
                self.log_error(LexErrorKind::UnrecognizedChar { value: punc });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Helper method to tokenize numeric literals with a suffix (e.g., `0u8`).
    fn tokenize_numeric_suffix(
        &mut self,
        start_pos: usize,
        suffix: TokenType,
    ) -> Result<Token, ErrorsEmitted> {
        // remove the `_` separators before parsing (if they exist)
        let value_string = self.input[start_pos..self.pos]
            .split('_')
            .collect::<Vec<&str>>()
            .concat();

        let span = Span::new(self.input, start_pos, self.pos);

        match suffix {
            TokenType::I32Type => {
                if let Ok(i) = value_string.trim_end_matches("i32").parse::<i32>() {
                    Ok(Token::IntLiteral {
                        value: Int::I32(i),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::I64Type => {
                if let Ok(i) = value_string.trim_end_matches("i64").parse::<i64>() {
                    Ok(Token::IntLiteral {
                        value: Int::I64(i),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::U8Type => {
                if let Ok(ui) = value_string.trim_end_matches("u8").parse::<u8>() {
                    Ok(Token::UIntLiteral {
                        value: UInt::U8(ui),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseUIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::U16Type => {
                if let Ok(ui) = value_string.trim_end_matches("u16").parse::<u16>() {
                    Ok(Token::UIntLiteral {
                        value: UInt::U16(ui),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseUIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::U32Type => {
                if let Ok(ui) = value_string.trim_end_matches("u32").parse::<u32>() {
                    Ok(Token::UIntLiteral {
                        value: UInt::U32(ui),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseUIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::U64Type => {
                if let Ok(ui) = value_string.trim_end_matches("u64").parse::<u64>() {
                    Ok(Token::UIntLiteral {
                        value: UInt::U64(ui),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseUIntError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::F32Type => {
                if let Ok(f) = value_string.trim_end_matches("f32").parse::<f32>() {
                    Ok(Token::FloatLiteral {
                        value: Float::F32(ordered_float::OrderedFloat(f)),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseFloatError);
                    Err(ErrorsEmitted)
                }
            }

            TokenType::F64Type => {
                if let Ok(f) = value_string.trim_end_matches("f64").parse::<f64>() {
                    Ok(Token::FloatLiteral {
                        value: Float::F64(ordered_float::OrderedFloat(f)),
                        span,
                    })
                } else {
                    self.log_error(LexErrorKind::ParseFloatError);
                    Err(ErrorsEmitted)
                }
            }

            tt => {
                self.log_error(LexErrorKind::UnexpectedNumericSuffix {
                    suffix: tt.to_string(),
                });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Handle escape sequences found in a string or character literal.
    /// Escape sequences are special sequences of characters that represent certain characters,
    /// such as newline (`\n`), tab (`\t`), or quotes (`\'`) within strings and character literals.
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
                    return Err(ErrorsEmitted);
                }
            };

            self.advance(); // skip escape sequence

            Ok(Some(escaped_char))
        } else {
            // incomplete escape sequence
            self.log_error(LexErrorKind::UnexpectedEndOfInput);
            Err(ErrorsEmitted)
        }
    }

    /// Advance the lexer (and iterator) by one character.
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
                // if the current character is whitespace, advance to the next one
                self.advance();
            } else {
                // if the current character is not whitespace, break out of the loop
                break;
            }
        }
    }

    /// Log information about an error that occurred during tokenization by pushing the error
    /// to the `errors` vector and providing information about error kind and position.
    fn log_error(&mut self, error_kind: LexErrorKind) {
        let error = CompilerError::new(error_kind, self.pos, self.input);

        self.errors.push(error);
    }

    /// Retrieve a list of any errors that occurred during tokenization.
    pub(crate) fn errors(&self) -> &[CompilerError<LexErrorKind>] {
        &self.errors
    }
}

/// Helper function to turn a byte slice (`&[u8]`) into a `Bytes`.
fn get_bytes(value: &[u8]) -> Bytes {
    let bytes = match value.len() {
        0 => panic!("empty slice"),
        1 => panic!("byte string literals must have more than one character"),
        2 => Bytes::B2(B2::from_slice(value)),
        3 => Bytes::B4(B4::from(&pad_zeroes::<3, 4>(value))),
        4 => Bytes::B4(B4::from_slice(value)),
        5 => Bytes::B8(B8::from(&pad_zeroes::<5, 8>(value))),
        6 => Bytes::B8(B8::from(&pad_zeroes::<6, 8>(value))),
        7 => Bytes::B8(B8::from(&pad_zeroes::<7, 8>(value))),
        8 => Bytes::B8(B8::from_slice(value)),
        9 => Bytes::B16(B16::from(&pad_zeroes::<9, 16>(value))),
        10 => Bytes::B16(B16::from(&pad_zeroes::<10, 16>(value))),
        11 => Bytes::B16(B16::from(&pad_zeroes::<11, 16>(value))),
        12 => Bytes::B16(B16::from(&pad_zeroes::<12, 16>(value))),
        13 => Bytes::B16(B16::from(&pad_zeroes::<13, 16>(value))),
        14 => Bytes::B16(B16::from(&pad_zeroes::<14, 16>(value))),
        15 => Bytes::B16(B16::from(&pad_zeroes::<15, 16>(value))),
        16 => Bytes::B16(B16::from_slice(value)),
        17 => Bytes::B32(B32::from(&pad_zeroes::<17, 32>(value))),
        18 => Bytes::B32(B32::from(&pad_zeroes::<18, 32>(value))),
        19 => Bytes::B32(B32::from(&pad_zeroes::<19, 32>(value))),
        20 => Bytes::B32(B32::from(&pad_zeroes::<20, 32>(value))),
        21 => Bytes::B32(B32::from(&pad_zeroes::<21, 32>(value))),
        22 => Bytes::B32(B32::from(&pad_zeroes::<22, 32>(value))),
        23 => Bytes::B32(B32::from(&pad_zeroes::<23, 32>(value))),
        24 => Bytes::B32(B32::from(&pad_zeroes::<24, 32>(value))),
        25 => Bytes::B32(B32::from(&pad_zeroes::<25, 32>(value))),
        26 => Bytes::B32(B32::from(&pad_zeroes::<26, 32>(value))),
        27 => Bytes::B32(B32::from(&pad_zeroes::<27, 32>(value))),
        28 => Bytes::B32(B32::from(&pad_zeroes::<28, 32>(value))),
        29 => Bytes::B32(B32::from(&pad_zeroes::<29, 32>(value))),
        30 => Bytes::B32(B32::from(&pad_zeroes::<30, 32>(value))),
        31 => Bytes::B32(B32::from(&pad_zeroes::<31, 32>(value))),
        32 => Bytes::B32(B32::from_slice(value)),
        _ => panic!("slice too big"),
    };

    bytes
}

/// Pads an input byte slice with zeroes to turn it into a fixed-length array.
/// Useful when converting a byte slice into a fixed-length byte string (`Bytes`).
#[track_caller]
fn pad_zeroes<const A: usize, const B: usize>(slice: &[u8]) -> [u8; B] {
    assert!(B >= A, "input size is greater than target size");

    let arr: [u8; A] = slice
        .try_into()
        .expect("unable to convert slice into fixed-length byte array");
    let mut target = [0; B];

    target[..A].copy_from_slice(&arr);
    target
}

/// List of reserved keywords to match against some input `&str`.
fn is_keyword(value: &str) -> bool {
    [
        "alias", "as", "break", "bytes", "const", "continue", "else", "enum", "Err", "false",
        "for", "func", "if", "impl", "import", "in", "let", "loop", "match", "module", "mut",
        "None", "Ok", "package", "pub", "ref", "return", "self", "Some", "static", "struct",
        "super", "trait", "true", "while", "i32", "i64", "u8", "u16", "u32", "u64", "u256", "u512",
        "f32", "f64", "byte", "b2", "b4", "b8", "b16", "b32", "h160", "h256", "h512", "String",
        "str", "char", "bool", "Self", "Option", "Result", "Vec", "Mapping",
    ]
    .contains(&value)
}

/// List of reserved attribute keywords to match against some input `&str`.
fn is_attribute(value: &str) -> bool {
    [
        "abstract",
        "calldata",
        "constructor",
        "contract",
        "error",
        "event",
        "extern",
        "interface",
        "library",
        "modifier",
        "payable",
        "script",
        "storage",
        "test",
        "topic",
        "unsafe",
        "view",
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
    fn tokenize_assignment_stmt() -> Result<(), ()> {
        let input = r#"let x: u256 = 0x1234_ABCDu256; // inline comment
        // line comment
        /*
        block comment
        */
        foo /* don't print */ bar
        let baz: byte = b"x";
        "#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[test]
    fn tokenize_attributes() -> Result<(), ()> {
        let input = r#"
        pub module foo {
            #![contract]

            #[storage]
            const balances: [u256; 4] = [0x1234, 0x5678, 0x90AB, 0xCDEF];
            
            #![interface]
            trait Bar

            #[constructor]
            pub func new() -> Contract
            
            #[extern]
            #[modifier]
            pub func baz();
        }"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[test]
    fn tokenize_char_lit() {
        let input = r#"let lit = 'a';
        let esc = '\\';
        let another_esc = '\'';"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => println!("{:#?}", t.tokens()),
            Err(_) => println!("{:#?}", lexer.errors),
        }
    }

    #[test]
    fn tokenize_function() -> Result<(), ()> {
        let input = r#"
        func foo(param1: u64, param2: char, param3: bool) -> Result<ReturnType, Err> {
            let bar: b4 = b"bar";
            
            if (param3) {
                print("{}", param2);
            } else {
                print(bar);
            }

            for x in y {
                param1 += 2;
            }

            return Ok(ReturnType {
                baz: "hello world"
            });
        }
        "#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[test]
    fn tokenize_hash_lits() -> Result<(), ()> {
        let input = r#"let hash = $0xBEEF_CAFE_1234_5678_90AB_CDEF_CAFE_BEEF;"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[test]
    fn tokenize_import_decl() -> Result<(), ()> {
        let input = r#"import package::some_module::SomeObject as Foo;"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[test]
    fn tokenize_struct_def() -> Result<(), ()> {
        let input = r#"
        /// These is a doc comment
        /// for the struct called `Foo`.
        pub struct Foo {
            bar: str,
            baz: u64,
        }"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }

    #[should_panic]
    #[test]
    fn tokenize_unrecognized_chars() {
        let input = r#"~ § ±"#;

        let mut lexer = Lexer::new(input);

        lexer.lex().expect(&format!("unrecognized input: {input}"));
    }

    #[test]
    fn tokenize_comprehensive() -> Result<(), ()> {
        let input = r#"
        ////////////////////////////////////////////////////////////////////////////////
        // `src/lib.feo`
        ////////////////////////////////////////////////////////////////////////////////
        //! package contents
    
        pub module some_library {}
        pub import package::Contract;

        ////////////////////////////////////////////////////////////////////////////////
        // `src/lib/some_library.feo`
        ////////////////////////////////////////////////////////////////////////////////
        #![library]
        
        #[extern]
        pub trait SomeTrait {
            func bar() -> str; 
        }

        func hello_world() {
            print!("hello world");
        }

        ////////////////////////////////////////////////////////////////////////////////
        // `src/main.feo`
        ////////////////////////////////////////////////////////////////////////////////
        #![script]

        import package::some_library;
        import package::some_contract::{Contract, OWNER};

        func main() {
            greater_than(1, 2);

            let world: b8 = b"world";

            print!("hello {}", Str::from(world));

            some_library::hello_world();

            let some_contract = Contract::new(OWNER, u256::ZERO);
            let foo = some_contract.foo();

            print!("{}", foo);
        }

        func greater_than(arg1: u256, arg2: u256) {
            if arg1 > arg2 {
                print!("{} is greater than {}", arg1, arg2);
            } else if arg1 == arg2 {
                print!("{} is equal to {}", arg1, arg2);
            } else {
                print!("{} is less than {}", arg1, arg2);
            }
        }

        ////////////////////////////////////////////////////////////////////////////////
        // `src/some_contract.feo`
        ////////////////////////////////////////////////////////////////////////////////
        #![contract]

        import package::some_library::SomeTrait;

        struct Foo {
            field1: str,
            field2: h160,
            field3: u256,
            field4: Vec<u256>,
            field5: b4,
            field6: bool
        }

        #[storage]
        #[extern]
        pub const OWNER: h160 = $0x12345123451234512345;

        const STR: str = "foo";
        const BYTES: b4 = Bytes::from(STR);

        trait Bar {
            #![interface]
            func foo(&mut self) -> Foo;
        }

        impl Contract {
            #[constructor]
            pub func new(owner: h160, balance: u256) -> Contract {
                Contract {
                    owner: owner,
                    balance: balance
      
                }
            }
        }

        impl Bar for Contract {
            func foo(&mut self) -> Foo {
                let array: [u8; 4] = [1, 2, 3, 4];
                let mut vec: Vec<u256> = Vec::new();

                let _unused_float = -12.34f64;

                for num in array {
                    vec.push(num as u256);
                }

                vec.push(0x12345);

                Foo {
                    field1: "foo",
                    field2: OWNER,
                    field3: 0x0123_4567_89AB_CDEF,
                    field4: vec,
                    field5: b"bar",
                    field6: true
                }
            }
        }"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex();

        match stream {
            Ok(t) => Ok(println!("{:#?}", t.tokens())),
            Err(_) => Err(println!("{:#?}", lexer.errors)),
        }
    }
}
