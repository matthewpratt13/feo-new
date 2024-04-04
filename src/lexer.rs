#![allow(dead_code)]

use crate::{
    error::{LexerError, LexerErrorKind},
    number::IntKind,
    span::Span,
    token::Token,
    U256,
};

/// Lexer struct, containing methods to render tokens (tokenize) from characters in a source string.
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    errors: Vec<LexerError>,
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

    fn tokenize_comment(&mut self) -> Result<Token, LexerError> {
        todo!()
    }

    fn tokenize_block_comment(&mut self) -> Result<Token, LexerError> {
        todo!()
    }

    fn tokenize_doc_line_comment(&mut self) -> Result<Token, LexerError> {
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

    fn tokenize_doc_block_comment(&mut self) -> Result<Token, LexerError> {
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

    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, LexerError> {
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

    fn tokenize_delimiter(&mut self) -> Result<Token, LexerError> {
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

            _ => Err(LexerError {
                error_kind: LexerErrorKind::MismatchedDelimiter(delim),
                pos: self.pos,
            }),
        }
    }

    fn tokenize_closing_delimiter(&mut self, expected: char) -> Result<Token, LexerError> {
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
                    return Err(LexerError {
                        error_kind: LexerErrorKind::UnexpectedCharacter(delim),
                        pos: self.pos,
                    })
                }
            }
        } else {
            return Err(LexerError {
                error_kind: LexerErrorKind::MismatchedDelimiter(delim),
                pos: self.pos,
            });
        }
    }

    fn tokenize_string(&mut self) -> Result<Token, LexerErrorKind> {
        let mut value = String::new();

        let start_pos = self.pos;

        self.advance(); // skip opening quote

        while self.pos < self.input.len() {
            let current_char = self.input.chars().nth(self.pos).unwrap();
            match current_char {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence() {
                        value.push(escaped_char);
                    } else {
                        return Err(LexerErrorKind::InvalidEscapeSequenceInStringLiteral);
                    }
                }
                '"' => {
                    self.advance(); // skip closing quote

                    let span = Span::new(self.input, start_pos, self.pos);

                    return Ok(Token::StringLiteral { value, span });
                }
                _ => {
                    value.push(current_char);
                    self.advance();
                }
            }
        }

        Err(LexerErrorKind::UnclosedStringLiteral)
    }

    fn tokenize_char(&mut self) -> Token {
        todo!()
    }

    fn tokenize_u256(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.pos;

        while self.pos < self.input.len()
            && self.input[self.pos..].chars().next().unwrap().is_numeric()
        {
            self.advance();
        }

        let number = self.input[start_pos..self.pos].parse::<U256>();
        let span = Span::new(self.input, start_pos, self.pos);

        match number {
            Ok(value) => Ok(Token::U256Literal { value, span }),
            Err(_) => todo!(),
        }
    }

    fn tokenize_numeric(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.pos;

        while self.pos < self.input.len() {
            let current_char = self.input.chars().nth(self.pos).unwrap();
            if current_char.is_numeric() {
                self.advance();
            } else {
                break;
            }
        }

        // TODO: check for `u64` (default)

        let value = self.input[start_pos..self.pos].parse::<i64>().unwrap();
        let span = Span::new(self.input, start_pos, self.pos);

        Ok(Token::IntLiteral {
            value: IntKind::I64(value),
            span,
        })
    }

    /// Parse an escape sequence found in a string or character literal.
    fn parse_escape_sequence(&mut self) -> Option<char> {
        self.advance(); // skip backslash

        if self.pos < self.input.len() {
            let escaped_char = match self.input.chars().nth(self.pos).unwrap() {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                // TODO: handle other escape sequences
                _ => return None, // invalid escape sequence
            };

            self.advance();
            Some(escaped_char)
        } else {
            None // incomplete escape sequence
        }
    }

    fn advance(&mut self) {
        self.pos += 1;
    }

    fn advance_by(&mut self, num_chars: usize) {
        self.pos += num_chars;
    }

    /// Check if some string is a reserved keyword (as opposed to an ordinary identifier).
    fn is_keyword(&self, value: &str) -> bool {
        let keywords = vec!["let", "func", "if", "else", "while", "for"]; // TODO: add keywords
        keywords.contains(&value)
    }

    /// Skip the source string's whitespace, which is considered unnecessary in tokenization.
    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && self.input.chars().nth(self.pos).unwrap().is_whitespace()
        {
            self.advance();
        }
    }
}
