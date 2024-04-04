#![allow(dead_code)]

use crate::{
    ast::{IntKind, UIntKind},
    error::{CompilerError, ErrorEmitted, LexerErrorKind},
    span::Span,
    token::{Token, TokenStream},
    U256,
};

/// Lexer struct that holds an input string and a `Vec<CompilerError>`, and contains methods 
/// to render tokens (tokenize) from characters from that string.
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    errors: Vec<CompilerError<LexerErrorKind>>, // store lexer errors from tokenization
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance.
    /// Initialize an empty `Vec` to store potential errors.
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            errors: Vec::new(),
        }
    }

    /// Main lexing function.
    /// Returns a stream of tokens generated from some input string (source code).
    fn lex(&mut self) -> Result<TokenStream, ErrorEmitted> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.peek_current() {
            let start_pos = self.pos;

            match c {
                _ if c.is_whitespace() => self.skip_whitespace(),

                _ if c == '/'
                    && (self.peek_next() == Some('/') || self.peek_next() == Some('*')) =>
                {
                    tokens.push(self.tokenize_doc_comment().map_err(|_| ErrorEmitted(()))?)
                }

                _ if c.is_ascii_alphabetic() || c == '_' => {
                    tokens.push(self.tokenize_identifier_or_keyword()?)
                }

                '(' | '[' | '{' | ')' | ']' | '}' => {
                    tokens.push(self.tokenize_delimiter()?);
                }
                '"' => tokens.push(self.tokenize_string()?),

                '\'' => tokens.push(self.tokenize_char()?),

                _ if c == '0'
                    && self
                        .peek_next()
                        .is_some_and(|x| &x.to_lowercase().to_string() == "x") =>
                {
                    tokens.push(self.tokenize_u256()?)
                }

                _ if c.is_digit(10)
                    || (c == '-' && self.peek_next().is_some_and(|c| c.is_digit(10))) =>
                {
                    tokens.push(self.tokenize_numeric()?)
                }

                ',' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Comma { punc: ',', span })
                }

                ';' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Semicolon { punc: ';', span })
                }

                '!' | '#' | '%' | '&' | '*' | '+' | '/' | '-' | '.' | ':' | '<' | '=' | '?'
                | '\\' | '^' | '`' => tokens.push(self.tokenize_punctuation()?),

                _ if !self.peek_next().is_some() => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::EOF { span })
                }

                _ => return Err(self.log_error(LexerErrorKind::UnrecognizedCharacter { punc: c })),
            }
        }

        let stream = TokenStream::new(&tokens, self.input, 0, self.pos);
        Ok(stream)
    }

    /// Tokenize a doc comment, ignoring ordinary line and block comments.
    fn tokenize_doc_comment(&mut self) -> Result<Token, ()> {
        let start_pos = self.pos;

        self.advance(); // skip first `/`

        if let Some('/') = self.peek_current() {
            self.advance(); // skip second `/`

            if self.peek_current() == Some('/') || self.peek_current() == Some('!') {
                self.advance(); // skip third `/` or `!`

                self.skip_whitespace();

                // advance until the end of the line
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    } else {
                        self.advance();
                    }
                }

                // trim any trailing whitespace
                let comment = self.input[start_pos..self.pos].trim().to_string();

                let span = Span::new(self.input, start_pos, self.pos);

                self.advance();

                Ok(Token::DocComment { comment, span })
            } else {
                // consume ordinary line comment
                loop {
                    self.advance();

                    if self.peek_current() == Some('\n') || self.pos == self.input.len() - 1 {
                        break Err(());
                    }
                }
            }
        } else if let Some('*') = self.peek_current() {
            // consume block comment
            loop {
                self.advance();

                if self.peek_current() == Some('*') && self.peek_next() == Some('/') {
                    break Err(self.advance_by(2));
                }

                if self.pos == self.input.len() - 1 {
                    break Err(());
                }
            }
        } else {
            let span = Span::new(self.input, start_pos, self.pos);

            Ok(Token::Slash { punc: '/', span })
        }
    }

    /// Tokenize an identifier and match it against reserved keywords.
    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.input.chars().nth(self.pos) {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let name = &self.input[start_pos..self.pos];
        let span = Span::new(self.input, start_pos, self.pos);

        if is_keyword(name) {
            self.advance(); // advance to next character

            match name {
                "abstract" => Ok(Token::Abstract {
                    name: name.to_string(),
                    span,
                }),
                "alias" => Ok(Token::Alias {
                    name: name.to_string(),
                    span,
                }),
                "as" => Ok(Token::As {
                    name: name.to_string(),
                    span,
                }),
                "break" => Ok(Token::Break {
                    name: name.to_string(),
                    span,
                }),
                "const" => Ok(Token::Const {
                    name: name.to_string(),
                    span,
                }),
                "continue" => Ok(Token::Continue {
                    name: name.to_string(),
                    span,
                }),
                "contract" => Ok(Token::Contract {
                    name: name.to_string(),
                    span,
                }),
                "else" => Ok(Token::Else {
                    name: name.to_string(),
                    span,
                }),
                "enum" => Ok(Token::Enum {
                    name: name.to_string(),
                    span,
                }),
                "extern" => Ok(Token::Extern {
                    name: name.to_string(),
                    span,
                }),
                "false" => Ok(Token::BoolLiteral {
                    value: name
                        .parse::<bool>()
                        .map_err(|_| self.log_error(LexerErrorKind::ParseBoolError))?,
                    span,
                }),
                "for" => Ok(Token::For {
                    name: name.to_string(),
                    span,
                }),
                "func" => Ok(Token::Func {
                    name: name.to_string(),
                    span,
                }),
                "if" => Ok(Token::If {
                    name: name.to_string(),
                    span,
                }),
                "impl" => Ok(Token::Impl {
                    name: name.to_string(),
                    span,
                }),
                "import" => Ok(Token::Import {
                    name: name.to_string(),
                    span,
                }),
                "in" => Ok(Token::In {
                    name: name.to_string(),
                    span,
                }),
                "let" => Ok(Token::Let {
                    name: name.to_string(),
                    span,
                }),
                "library" => Ok(Token::Library {
                    name: name.to_string(),
                    span,
                }),
                "loop" => Ok(Token::Loop {
                    name: name.to_string(),
                    span,
                }),
                "match" => Ok(Token::Match {
                    name: name.to_string(),
                    span,
                }),
                "module" => Ok(Token::Module {
                    name: name.to_string(),
                    span,
                }),
                "mut" => Ok(Token::Mut {
                    name: name.to_string(),
                    span,
                }),
                "package" => Ok(Token::Package {
                    name: name.to_string(),
                    span,
                }),
                "payable" => Ok(Token::Payable {
                    name: name.to_string(),
                    span,
                }),
                "pub" => Ok(Token::Pub {
                    name: name.to_string(),
                    span,
                }),
                "ref" => Ok(Token::Ref {
                    name: name.to_string(),
                    span,
                }),
                "return" => Ok(Token::Return {
                    name: name.to_string(),
                    span,
                }),
                "self" => Ok(Token::SelfKeyword {
                    name: name.to_string(),
                    span,
                }),
                "static" => Ok(Token::Static {
                    name: name.to_string(),
                    span,
                }),
                "storage" => Ok(Token::Storage {
                    name: name.to_string(),
                    span,
                }),
                "struct" => Ok(Token::Struct {
                    name: name.to_string(),
                    span,
                }),
                "super" => Ok(Token::Super {
                    name: name.to_string(),
                    span,
                }),
                "test" => Ok(Token::Test {
                    name: name.to_string(),
                    span,
                }),
                "topic" => Ok(Token::Topic {
                    name: name.to_string(),
                    span,
                }),
                "trait" => Ok(Token::Trait {
                    name: name.to_string(),
                    span,
                }),
                "true" => Ok(Token::BoolLiteral {
                    value: name
                        .parse::<bool>()
                        .map_err(|_| self.log_error(LexerErrorKind::ParseBoolError))?,
                    span,
                }),
                "unsafe" => Ok(Token::Unsafe {
                    name: name.to_string(),
                    span,
                }),
                "while" => Ok(Token::While {
                    name: name.to_string(),
                    span,
                }),
                _ => Err(self.log_error(LexerErrorKind::InvalidKeyword {
                    name: name.to_string(),
                })),
            }
        } else {
            self.advance(); // advance to next character

            Ok(Token::Identifier {
                name: name.to_string(),
                span,
            })
        }
    }

    /// Tokenize an opening delimiter.
    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let delim = self.input.chars().nth(self.pos).ok_or(self.log_error(
            LexerErrorKind::CharacterNotFound {
                expected: "delimiter".to_string(),
            },
        ))?;

        match delim {
            '(' => {
                self.advance(); // skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LParen { delim, span })
            }

            '{' => {
                self.advance(); //  skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBrace { delim, span })
            }

            '[' => {
                self.advance(); // skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBracket { delim, span })
            }

            ')' => self.tokenize_closing_delimiter(')'),

            ']' => self.tokenize_closing_delimiter(']'),

            '}' => self.tokenize_closing_delimiter('}'),

            _ => Err(self.log_error(LexerErrorKind::UnexpectedCharacter {
                expected: "delimiter".to_string(),
                found: delim,
            })),
        }
    }

    /// Tokenize a closing delimiter, throw errors where the inputted delimiter is missing,
    /// or does not match the expected delimiter.
    fn tokenize_closing_delimiter(&mut self, expected: char) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let delim = self
            .input
            .chars()
            .nth(self.pos)
            .ok_or(self.log_error(LexerErrorKind::MissingDelimiter { delim: expected }))?;

        if delim == expected {
            self.advance(); // move past closing delimiter

            match delim {
                ')' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RParen { delim, span })
                }

                '}' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RBrace { delim, span })
                }

                ']' => {
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RBracket { delim, span })
                }

                _ => {
                    return Err(self.log_error(LexerErrorKind::UnexpectedCharacter {
                        expected: "delimiter".to_string(),
                        found: delim,
                    }))
                }
            }
        } else {
            return Err(self.log_error(LexerErrorKind::MismatchedDelimiter {
                expected,
                found: delim,
            }));
        }
    }

    /// Tokenize a string literal, handling escape sequences where applicable.
    fn tokenize_string(&mut self) -> Result<Token, ErrorEmitted> {
        let mut value = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote (`"`)

        while let Some(c) = self.input.chars().nth(self.pos) {
            match c {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        value.push(escaped_char);
                        self.advance();
                    } else {
                        return Err(self.log_error(LexerErrorKind::CharacterNotFound {
                            expected: "escape sequence".to_string(),
                        }));
                    }
                }
                '"' => {
                    self.advance(); // skip closing quote (`"`)

                    let span = Span::new(self.input, start_pos, self.pos);

                    return Ok(Token::StringLiteral { value, span });
                }
                _ => {
                    value.push(c);
                    self.advance();
                }
            }
        }

        Err(self.log_error(LexerErrorKind::MissingQuote { quote: '\"' }))
    }

    /// Tokenize a `char` literal, handling escape sequences where applicable.
    fn tokenize_char(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip opening quote (`'`)

        match self.input.chars().nth(self.pos) {
            Some(value) => match value {
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
                        return Err(self.log_error(LexerErrorKind::CharacterNotFound {
                            expected: "escape sequence".to_string(),
                        }));
                    }
                }

                _ if value == ' ' => {
                    return Err(self.log_error(LexerErrorKind::CharacterNotFound {
                        expected: "character literal".to_string(),
                    }));
                }

                _ => {
                    self.advance();

                    if let Some('\'') = self.input.chars().nth(self.pos) {
                        self.advance(); // skip closing quote (`'`)

                        let span = Span::new(self.input, start_pos, self.pos);

                        Ok(Token::CharLiteral { value, span })
                    } else {
                        return Err(self.log_error(LexerErrorKind::MissingQuote { quote: '\'' }));
                    }
                }
            },

            None => {
                let span = Span::new(self.input, start_pos, self.pos);
                return Ok(Token::EOF { span });
            }
        }
    }

    /// Tokenize a `U256` hexadecimal digit.
    fn tokenize_u256(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        // check for hexadecimal prefix (`0x`)
        if self.peek_current() == Some('0')
            && self
                .peek_next()
                .is_some_and(|x| &x.to_lowercase().to_string() == "x")
        {
            self.advance_by(2); // consume prefix
        }

        // collect hexadecimal digits (may have `_` separators)
        while let Some(c) = self.input.chars().nth(self.pos) {
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
            .parse::<U256>()
            .map_err(|_| self.log_error(LexerErrorKind::ParseHexError))?;

        let span = Span::new(self.input, start_pos, self.pos);

        self.advance();

        Ok(Token::U256Literal { value, span })
    }

    /// Tokenize a numeric value (i.e., `i64` or `u64`).
    /// Parse to `u64` unless a `-` is encountered, in which case parse to `i64`.
    fn tokenize_numeric(&mut self) -> Result<Token, ErrorEmitted> {
        let mut is_negative = false;

        let start_pos = self.pos;

        // check for `-` before the number to decide which type of integer to parse to
        if self.peek_current() == Some('-') {
            if self.peek_next().is_some_and(|c| c.is_digit(10)) {
                is_negative = true;
                self.advance(); // skip '-'
            } else {
                let span = Span::new(self.input, start_pos, self.pos);
                self.advance();
                return Ok(Token::Minus { punc: '-', span });
            }
        }

        // go back and read from previous char ('-') if negative, else read from current position
        let start_pos = if is_negative { self.pos - 1 } else { self.pos };

        while let Some(c) = self.input.chars().nth(self.pos) {
            if c.is_digit(10) || c == '_' {
                self.advance();
            }
        }

        if is_negative {
            // remove the `_` separators before parsing
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<i64>()
                .map_err(|_| self.log_error(LexerErrorKind::ParseIntError))?;

            let span = Span::new(self.input, start_pos, self.pos);

            self.advance();

            Ok(Token::IntLiteral {
                value: IntKind::I64(value),
                span,
            })
        } else {
            // remove the `_` separators before parsing (if they exist)
            let value = self.input[start_pos..self.pos]
                .parse::<u64>()
                .map_err(|_| self.log_error(LexerErrorKind::ParseUIntError))?;

            let span = Span::new(self.input, start_pos, self.pos);

            self.advance();

            Ok(Token::UIntLiteral {
                value: UIntKind::U64(value),
                span,
            })
        }
    }

    /// Tokenize punctuation.
    fn tokenize_punctuation(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.input.chars().nth(self.pos) {
            if !is_delimiter(c) && !is_separator(c) | !is_quote(c) {
                self.advance()
            } else {
                break;
            }
        }

        let punc_string = self.input[start_pos..self.pos].to_string();

        let punc = self.input[start_pos..self.pos]
            .parse::<char>()
            .map_err(|_| self.log_error(LexerErrorKind::ParsePuncError))?;

        let span = Span::new(self.input, start_pos, self.pos);

        self.advance();

        match punc_string.as_str() {
            "_" => Ok(Token::Underscore {
                name: punc_string,
                span,
            }),
            "." => Ok(Token::FullStop { punc, span }),
            ".." => Ok(Token::DblDot {
                punc: punc_string,
                span,
            }),
            "..=" => Ok(Token::DotDotEquals {
                punc: punc_string,
                span,
            }),
            "!" => Ok(Token::Bang { punc, span }),
            "!=" => Ok(Token::BangEquals {
                punc: punc_string,
                span,
            }),
            "#" => Ok(Token::HashSign { punc, span }),
            "#!" => Ok(Token::HashBang {
                punc: punc_string,
                span,
            }),
            "%" => Ok(Token::Percent { punc, span }),
            "%=" => Ok(Token::PercentEquals {
                punc: punc_string,
                span,
            }),
            "&" => Ok(Token::Ampersand { punc, span }),
            "&&" => Ok(Token::DblAmpersand {
                punc: punc_string,
                span,
            }),
            "+" => Ok(Token::Plus { punc, span }),
            "+=" => Ok(Token::PlusEquals {
                punc: punc_string,
                span,
            }),
            "-" => Ok(Token::Minus { punc, span }),
            "-=" => Ok(Token::MinusEquals {
                punc: punc_string,
                span,
            }),
            "->" => Ok(Token::ThinArrow {
                punc: punc_string,
                span,
            }),
            "*" => Ok(Token::Asterisk { punc, span }),
            "**" => Ok(Token::DblAsterisk {
                punc: punc_string,
                span,
            }),
            "*=" => Ok(Token::AsteriskEquals {
                punc: punc_string,
                span,
            }),

            "/" => Ok(Token::Slash { punc, span }),
            "/=" => Ok(Token::SlashEquals {
                punc: punc_string,
                span,
            }),
            ":" => Ok(Token::Colon { punc, span }),
            "::" => Ok(Token::DblColon {
                punc: punc_string,
                span,
            }),
            "::*" => Ok(Token::ColonColonAsterisk {
                punc: punc_string,
                span,
            }),
            "=" => Ok(Token::Equals { punc, span }),
            "==" => Ok(Token::DblEquals {
                punc: punc_string,
                span,
            }),
            "=>" => Ok(Token::FatArrow {
                punc: punc_string,
                span,
            }),
            "<" => Ok(Token::LessThan { punc, span }),
            "<<" => Ok(Token::DblLessThan {
                punc: punc_string,
                span,
            }),
            "<=" => Ok(Token::LessThanEquals {
                punc: punc_string,
                span,
            }),
            ">" => Ok(Token::GreaterThan { punc, span }),
            ">>" => Ok(Token::DblGreaterThan {
                punc: punc_string,
                span,
            }),
            ">=" => Ok(Token::GreaterThanEquals {
                punc: punc_string,
                span,
            }),
            "?" => Ok(Token::QuestionMark { punc, span }),
            "\\" => Ok(Token::Backslash { punc, span }),
            "^" => Ok(Token::Caret { punc, span }),
            "`" => Ok(Token::Backtick { punc, span }),
            "|" => Ok(Token::Pipe { punc, span }),
            "||" => Ok(Token::DblPipe {
                punc: punc_string,
                span,
            }),
            _ => Err(self.log_error(LexerErrorKind::InvalidPunc { punc: punc_string })),
        }
    }

    /// Parse an escape sequence found in a string or character literal.
    fn parse_escape_sequence(&mut self) -> Result<Option<char>, ErrorEmitted> {
        self.advance(); // skip backslash

        if self.pos < self.input.len() {
            let escaped_char = match self.input.chars().nth(self.pos) {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('0') => '\0',
                Some('\'') => '\'',
                Some('"') => '"',
                Some(c) => {
                    return Err(
                        self.log_error(LexerErrorKind::InvalidEscapeSequence { sequence: c })
                    );
                }
                None => {
                    return Err(self.log_error(LexerErrorKind::CharacterNotFound {
                        expected: "character".to_string(),
                    }))
                }
            };

            self.advance(); // skip escape sequence

            Ok(Some(escaped_char))
        } else {
            Ok(None) // incomplete escape sequence
        }
    }

    /// Advance the lexer by one character.
    fn advance(&mut self) {
        self.pos += 1;
    }

    /// Advance the lexer by `num_chars` characters.
    fn advance_by(&mut self, num_chars: usize) {
        self.pos += num_chars;
    }

    /// Get the character at the lexer's current position.
    fn peek_current(&self) -> Option<char> {
        if self.pos < self.input.len() - 1 && !self.input.is_empty() {
            self.input[self.pos..self.pos + 1].parse::<char>().ok()
        } else {
            None
        }
    }

    /// Get the next character without advancing the lexer.
    fn peek_next(&self) -> Option<char> {
        if self.pos < self.input.len() - 2 && self.input.len() > 1 {
            self.input[self.pos + 1..self.pos + 2].parse::<char>().ok()
        } else {
            None
        }
    }

    /// Skip the source string's whitespace, which is considered unnecessary during tokenization.
    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && self.input.chars().nth(self.pos).unwrap().is_whitespace()
        {
            self.advance();
        }
    }

    /// Log and store information about an error that occurred during lexing.
    /// Return `ErrorEmitted` just to confirm that the action happened.
    fn log_error(&mut self, error_kind: LexerErrorKind) -> ErrorEmitted {
        let error = CompilerError::new(&self.input, self.pos, error_kind);

        self.errors.push(error);
        ErrorEmitted(())
    }
}

/// Check if some substring is a reserved keyword (as opposed to an ordinary identifier).
fn is_keyword(value: &str) -> bool {
    vec![
        "abstract", "alias", "as", "break", "const", "continue", "contract", "else", "enum",
        "extern", "false", "for", "func", "if", "impl", "import", "in", "let", "library", "loop",
        "match", "module", "mut", "package", "payable", "pub", "ref", "return", "self", "static",
        "storage", "struct", "super", "test", "topic", "trait", "true", "unsafe", "while",
    ]
    .contains(&value)
}

/// Check if a character is a delimiter.
fn is_delimiter(value: char) -> bool {
    vec!['(', ')', '{', '}', '[', ']'].contains(&value)
}

/// Check if a character is a `;` or `,`.
fn is_separator(value: char) -> bool {
    vec![';', ','].contains(&value)
}

/// Check if a character is a `'` or `"`.
fn is_quote(value: char) -> bool {
    vec!['\'', '\"'].contains(&value)
}
