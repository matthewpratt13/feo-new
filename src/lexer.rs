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

    fn tokenize_comment(&mut self) -> Result<Token, ErrorEmitted> {
        todo!()
    }

    fn tokenize_block_comment(&mut self) -> Result<Token, ErrorEmitted> {
        todo!()
    }

    fn tokenize_doc_line_comment(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        self.advance_by(3); // skip doc comment prefix (`///`)

        while self.pos < self.input.len() {
            if self.input.chars().nth(self.pos).unwrap() == '\n' {
                break;
            }
            
            self.advance();
        }

        let comment = self.input[start_pos..self.pos].trim().to_string();
        let span = Span::new(self.input, start_pos, self.pos);

        Ok(Token::DocComment { comment, span })
    }

    fn tokenize_doc_block_comment(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        self.advance_by(3); // skip doc block comment prefix (`/**`)

        while self.pos + 1 < self.input.len() && &self.input[self.pos..self.pos + 2] != "*/" {
            self.advance();
        }

        let comment = self.input[start_pos..self.pos].trim().to_string();
        let span = Span::new(self.input, start_pos, self.pos);

        self.advance_by(2); // skip comment terminator (`*/`)

        Ok(Token::DocComment { comment, span })
    }

    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        while self.pos < self.input.len() {
            let current_char = self.input.chars().nth(self.pos).unwrap();
            if current_char.is_alphanumeric() || current_char == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let name = &self.input[start_pos..self.pos];
        let span = Span::new(self.input, start_pos, self.pos);

        if self.is_keyword(name) {
            match name {
                "let" => Ok(Token::Let {
                    name: name.to_string(),
                    span,
                }),

                // TODO: fill out the other keyword tokens
                _ => todo!(),
            }
        } else {
            Ok(Token::Identifier {
                name: name.to_string(),
                span,
            })
        }
    }

    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let delim = self.input.chars().nth(self.pos).unwrap();

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

    fn tokenize_closing_delimiter(&mut self, expected: char) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let delim = self.input.chars().nth(self.pos).unwrap();

        if delim == expected {
            self.advance(); // skip closing delimiter

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

    fn tokenize_string(&mut self) -> Result<Token, ErrorEmitted> {
        let mut value = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote (`"`)

        while self.pos < self.input.len() {
            let current_char = self.input.chars().nth(self.pos).unwrap();

            match current_char {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        value.push(escaped_char);
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
                    value.push(current_char);
                    self.advance();
                }
            }
        }

        Err(self.log_error(LexerErrorKind::MissingQuote { quote: '\"' }))
    }

    fn tokenize_char(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip opening quote (`'`)

        match self.input.chars().nth(self.pos) {
            Some(value) => match value {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        let span = Span::new(self.input, start_pos, self.pos);

                        self.advance();

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
                        let span = Span::new(self.input, start_pos, self.pos);

                        self.advance(); // skip closing quote (`'`)

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

        Ok(Token::U256Literal { value, span })
    }

    fn tokenize_numeric(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let mut is_int = false;

        // check for `-` before the number to decide which type of integer to parse to
        if self.input.chars().nth(self.pos) == Some('-') {
            self.advance();
            is_int = true;
        }

        while let Some(c) = self.input.chars().nth(self.pos) {
            if c.is_digit(10) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if is_int {
            // remove the `_` separators before parsing
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<i64>()
                .map_err(|_| self.log_error(LexerErrorKind::ParseIntError))?;
            let span = Span::new(self.input, start_pos, self.pos);

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

        // TODO: check for `u64` (default)
    }

    /// Parse an escape sequence found in a string or character literal.
    fn parse_escape_sequence(&mut self) -> Result<Option<char>, ErrorEmitted> {
        self.advance(); // skip backslash

        if self.pos < self.input.len() {
            let escaped_char = match self.input.chars().nth(self.pos).unwrap() {
                'n' => '\n',
                'r' => '\r',
                't' => '\t',
                '\\' => '\\',
                '0' => '\0',
                '\'' => '\'',
                '"' => '"',
                _ => {
                    return Err(self.log_error(LexerErrorKind::InvalidEscapeSequence {
                        sequence: self.input.chars().nth(self.pos).unwrap(),
                    }));
                } // invalid escape sequence
            };

            self.advance();

            Ok(Some(escaped_char))
        } else {
            Ok(None) // incomplete escape sequence
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn advance_by(&mut self, num_chars: usize) {
        self.pos += num_chars;
    }

    fn peek_current(&self) -> Option<char> {
        if self.pos < self.input.len() - 1 && !self.input.is_empty() {
            self.input[self.pos..self.pos + 1].parse::<char>().ok()
        } else {
            None
        }
    }

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
