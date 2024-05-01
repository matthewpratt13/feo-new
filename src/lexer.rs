use std::{iter::Peekable, str::Chars};

use crate::{
    ast::{self, BigUInt, Byte, Hash, Int, Str, UInt},
    error::{CompilerError, ErrorsEmitted, LexErrorKind},
    span::Span,
    token::{DocCommentType, Token, TokenStream},
    H160, H256, H512, U512,
};

/// Struct that stores an input string (source code) and contains methods to render tokens
/// from characters in that string (i.e., tokenization).
pub struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    peekable_chars: Peekable<Chars<'a>>,
    errors: Vec<CompilerError<LexErrorKind>>,
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance.
    /// Initialize an empty `Vec` to store potential errors, and create an `Iterator`
    /// from the characters in the input string to traverse the contents  and look ahead
    /// without advancing the lexer.
    pub(crate) fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            peekable_chars: input.chars().peekable(),
            errors: Vec::new(),
        }
    }

    /// Retrieve the lexer's `CompilerError`.
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

                '.' => match self.peek_next() {
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
            } else {
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

            // replace actual source code with `""`, as ordinary comments are discarded
            Ok(Token::BlockComment {
                span: Span::new("", start_pos, self.pos),
            })
        } else {
            // return the first `/` as a `Token::Slash` instead of aborting
            let span = Span::new(self.input, start_pos, self.pos);
            Ok(Token::Slash { punc: '/', span })
        }
    }

    /// Tokenize an identifier, or reserved keyword or attribute.
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
                            n
                        } else {
                            self.log_error(LexErrorKind::LexBoolError);
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
                            n
                        } else {
                            self.log_error(LexErrorKind::LexBoolError);
                            return Err(ErrorsEmitted);
                        }
                    },

                    span,
                }),
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

    /// Tokenize a delimiter (i.e., `(`, `)`, `[`, `]`, `{` and `}`).
    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorsEmitted> {
        let start_pos = self.pos;

        let delim = self.peek_current();

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
                    found: d,
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
    fn tokenize_string(&mut self) -> Result<Token, ErrorsEmitted> {
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
                        value: Str(buf.as_bytes().to_vec()),
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

    /// Tokenize a static byte array literal (`Bytes`), handling escape sequences where applicable.
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
                            value: Byte(value.as_bytes()[0]),
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
                            value: escaped_char,
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

                        Ok(Token::CharLiteral { value, span })
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
            .parse::<U512>();

        let span = Span::new(self.input, start_pos, self.pos);

        if let Ok(v) = value {
            Ok(Token::BigUIntLiteral {
                value: BigUInt::U512(v),
                span,
            })
        } else {
            self.log_error(LexErrorKind::LexBigUIntError);
            Err(ErrorsEmitted)
        }
    }

    /// Tokenize a hash literal (i.e., `h160`, `h256` and `h512`), starting with `$`.
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
            self.log_error(LexErrorKind::LexHashError);
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
                    value: Hash::H160(value),
                    span,
                })
            }

            32 => {
                let value = H256::from_slice(hash.as_bytes());

                Ok(Token::HashLiteral {
                    value: Hash::H256(value),
                    span,
                })
            }

            64 => {
                let value = H512::from_slice(hash.as_bytes());

                Ok(Token::HashLiteral {
                    value: Hash::H512(value),
                    span,
                })
            }

            _ => {
                self.log_error(LexErrorKind::InvalidHashLength { len: hash.len() });
                Err(ErrorsEmitted)
            }
        }
    }

    /// Tokenize a numeric value (i.e., `i128` or `u128`).
    /// Parse to `u128` unless a `-` is encountered, in which case parse to `i128`.
    fn tokenize_numeric(&mut self) -> Result<Token, ErrorsEmitted> {
        let mut is_negative = false;

        // check for `-` before the number to decide which type of integer to parse to
        if self.peek_current() == Some('-') && self.peek_next().is_some_and(|c| c.is_digit(10)) {
            is_negative = true;
            self.advance(); // skip `-`
        }

        // go back and read from previous character (`-`) if negative,
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
                .parse::<i128>();

            let span = Span::new(self.input, start_pos, self.pos);

            if let Ok(v) = value {
                Ok(Token::IntLiteral {
                    value: Int::I128(v),
                    span,
                })
            } else {
                self.log_error(LexErrorKind::LexIntError);
                Err(ErrorsEmitted)
            }
        } else {
            // remove the `_` separators before parsing (if they exist)
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<u128>();

            let span = Span::new(self.input, start_pos, self.pos);

            if let Ok(v) = value {
                Ok(Token::UIntLiteral {
                    value: UInt::U128(v),
                    span,
                })
            } else {
                self.log_error(LexErrorKind::LexUIntError);
                Err(ErrorsEmitted)
            }
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

    /// Advance the lexer / scanner (and iterator) by one character.
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

    /// Log information about an error that occurred during tokenization.
    fn log_error(&mut self, error_kind: LexErrorKind) {
        let error = CompilerError::new(error_kind, self.input, self.pos);

        self.errors.push(error);
    }
}

/// List of reserved keywords to match against some input string.
fn is_keyword(value: &str) -> bool {
    [
        "alias", "as", "break", "bytes", "const", "continue", "else", "enum", "Err", "false",
        "for", "func", "if", "impl", "import", "in", "let", "loop", "match", "module", "mut",
        "None", "Ok", "package", "pub", "ref", "return", "self", "Some", "static", "struct",
        "super", "trait", "true", "while", "i32", "i64", "i128", "u8", "u16", "u32", "u64", "u128",
        "u256", "u512", "byte", "b2", "b4", "b8", "b16", "b32", "h160", "h256", "h512", "String",
        "str", "char", "bool", "Option", "Result", "Vec", "Mapping",
    ]
    .contains(&value)
}

/// List of reserved attribute keywords to match against some input string.
fn is_attribute(value: &str) -> bool {
    [
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
        let input = r#"import package::some_module::SomeObject as Foo;"#;

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
            bar: str,
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

    #[test]
    #[should_panic]
    fn tokenize_unrecognized_chars() {
        let input = r#"~ § ±"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors(),
        ));

        println!("{:#?}", stream);
    }

    #[test]
    fn tokenize_comprehensive() {
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

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. current char: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors(),
        ));

        println!("{:#?}", stream);
    }
}
