#![allow(dead_code)]

use crate::{
    error::{CompilerError, ErrorEmitted, LexerErrorKind},
    number::{IntKind, UIntKind},
    span::Span,
    token::Token,
    U256,
};

/// Lexer struct, containing methods to render tokens (tokenize) from characters in a source string.
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    errors: Vec<CompilerError<LexerErrorKind>>,
}

impl<'a> Lexer<'a> {
    /// Constructor method
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            errors: Vec::new(),
        }
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

    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.input.chars().nth(self.pos) {
            if c.is_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let name = &self.input[start_pos..self.pos];
        let span = Span::new(self.input, start_pos, self.pos);

        if self.is_keyword(name) {
            self.advance(); // advance to next character

            match name {
                "let" => Ok(Token::Let {
                    name: name.to_string(),
                    span,
                }),

                // TODO: fill out the other keyword tokens
                _ => todo!(),
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
        let mut _is_int = false;

        let start_pos = self.pos;

        // check for `-` before the number to decide which type of integer to parse to
        if self.peek_current() == Some('-') {
            self.advance(); // skip `-`
        }

        if self.peek_current().is_some_and(|c| c.is_digit(10)) {
            _is_int = true
        } else {
            let span = Span::new(self.input, start_pos, self.pos);

            return Ok(Token::Minus { punc: '-', span });
        }

        while let Some(c) = self.peek_current() {
            if c.is_digit(10) || c == '_' {
                self.advance();
            }
        }

        if _is_int {
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
                .map_err(|_| self.log_error(LexerErrorKind::ParseIntError))?;

            let span = Span::new(self.input, start_pos, self.pos);

            self.advance();

            Ok(Token::UIntLiteral {
                value: UIntKind::U64(value),
                span,
            })
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

    /// Get the current token.
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

    /// Check if some substring is a reserved keyword (as opposed to an ordinary identifier).
    fn is_keyword(&self, value: &str) -> bool {
        let keywords = vec!["let", "func", "if", "else", "while", "for"]; // TODO: add keywords
        keywords.contains(&value)
    }

    /// Skip the source string's whitespace, which is considered unnecessary during tokenization.
    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && self.input.chars().nth(self.pos).unwrap().is_whitespace()
        {
            self.advance();
        }
    }

    /// Log a given lexer error and return an `ErrorEmitted` to show an error has occurred.
    /// The `error_kind` describes the error succinctly.
    fn log_error(&mut self, error_kind: LexerErrorKind) -> ErrorEmitted {
        let error = CompilerError::new(&self.input, self.pos, error_kind);

        self.errors.push(error);
        ErrorEmitted(())
    }
}
