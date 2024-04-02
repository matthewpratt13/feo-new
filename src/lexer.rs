#![allow(dead_code)]

use crate::{
    error::{LexerError, LexerErrorKind},
    number::IntKind,
    span::Span,
    token::Token,
    U256,
};

struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    errors: Vec<LexerError>,
}

impl<'a> Lexer<'a> {
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            errors: Vec::new(),
        }
    }

    fn tokenize_comment(&mut self) -> Token {
        todo!()
    }

    fn tokenize_block_comment(&mut self) -> Token {
        todo!()
    }

    fn tokenize_doc_line_comment(&mut self) -> Token {
        let start_pos = self.pos;

        self.pos += 3; // Skip the "///"

        while self.pos < self.input.len() {
            if self.input.chars().nth(self.pos).unwrap() == '\n' {
                break;
            }
            self.pos += 1;
        }

        let comment = self.input[start_pos..self.pos].trim().to_string();
        let span = Span::new(self.input, start_pos, self.pos);

        Token::DocComment { comment, span }
    }

    fn tokenize_doc_block_comment(&mut self) -> Token {
        let start_pos = self.pos;

        self.pos += 3; // Skip the "/**" delimiter

        while self.pos + 1 < self.input.len() && &self.input[self.pos..self.pos + 2] != "*/" {
            self.pos += 1;
        }

        let comment = self.input[start_pos..self.pos].trim().to_string();
        let span = Span::new(self.input, start_pos, self.pos);

        self.pos += 2; // Skip the "*/" delimiter

        Token::DocComment { comment, span }
    }

    // fn tokenize_identifier_or_keyword(&mut self) -> Token {
    //     let start_pos = self.pos;

    //     while self.pos < self.input.len() {
    //         let current_char = self.input.chars().nth(self.pos).unwrap();
    //         if current_char.is_alphanumeric() || current_char == '_' {
    //             self.pos += 1;
    //         } else {
    //             break;
    //         }
    //     }

    //     let value = &self.input[start_pos..self.pos];

    //     let span = Span::new(self.input, start_pos, self.pos);

    //     if self.is_keyword(value) {
    //         // match value to keywords, return e.g. Token::Let{ value: value.to_string, span }
    //         Token::Keyword {
    //             value: value.to_string(),
    //             span,
    //         }
    //     } else {
    //         Token::Identifier {
    //             name: value.to_string(),
    //             span,
    //         }
    //     }
    // }

    fn tokenize_delimiter(&mut self) -> Result<Token, LexerError> {
        let start_pos = self.pos;
        let delim = self.input.chars().nth(self.pos).unwrap();

        match delim {
            '(' => {
                self.pos += 1; // Move past the opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LParen { delim, span })
            }

            '{' => {
                self.pos += 1; // Move past the opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBrace { delim, span })
            }

            '[' => {
                self.pos += 1; // Move past the opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBracket { delim, span })
            }

            ')' => self.tokenize_closing_delimiter(')'),
            ']' => self.tokenize_closing_delimiter(']'),
            '}' => self.tokenize_closing_delimiter('}'),
            _ => unreachable!(), // Placeholder for unmatched delimiters
        }
    }

    fn tokenize_closing_delimiter(&mut self, expected: char) -> Result<Token, LexerError> {
        let start_pos = self.pos;

        let delim = self.input.chars().nth(self.pos).unwrap();

        if delim == expected {
            self.pos += 1; // Move past the closing delimiter

            match delim {
                ')' => {
                    self.pos += 1; // Move past the opening delimiter
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RParen { delim, span })
                }

                '}' => {
                    self.pos += 1; // Move past the opening delimiter
                    let span = Span::new(self.input, start_pos, self.pos);
                    Ok(Token::RBrace { delim, span })
                }

                ']' => {
                    self.pos += 1; // Move past the opening delimiter
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

    fn tokenize_numeric(&mut self) -> Token {
        let start_pos = self.pos;

        while self.pos < self.input.len() {
            let current_char = self.input.chars().nth(self.pos).unwrap();
            if current_char.is_numeric() {
                self.pos += 1;
            } else {
                break;
            }
        }

        // check for u64 and f64

        let value = self.input[start_pos..self.pos].parse::<i64>().unwrap();
        let span = Span::new(self.input, start_pos, self.pos);

        Token::IntLiteral {
            value: IntKind::I64(value),
            span,
        }
    }

    fn tokenize_u256(&mut self) -> Token {
        let start_pos = self.pos;

        while self.pos < self.input.len()
            && self.input[self.pos..].chars().next().unwrap().is_numeric()
        {
            self.pos += 1;
        }

        let number = self.input[start_pos..self.pos].parse::<U256>();
        let span = Span::new(self.input, start_pos, self.pos);

        match number {
            Ok(value) => Token::U256Literal { value, span },
            Err(_) => todo!(),
        }
    }

    // fn tokenize_u256(&mut self) -> Token {
    //     let start_pos = self.pos;
    //     while self.pos < self.input.len() && self.input[self.pos..].chars().next().unwrap().is_numeric() {
    //         self.pos += 1;
    //     }
    //     let number = self.input[start_pos..self.pos.parse::<u128>();
    //     match number {
    //         Ok(num) => Token::U256Literal(num),
    //         Err(_) => Token::Error,
    //     }
    // }

    // fn tokenize_string(&mut self) -> Token {
    //     // Tokenize string literals
    //     let mut value = String::new();
    //     self.position += 1; // Skip the opening quote
    //     while self.position < self.input.len() {
    //         let current_char = self.input.chars().nth(self.position).unwrap();
    //         match current_char {
    //             '\\' => {
    //                 // Handle escape sequences
    //                 if let Some(escaped_char) = self.parse_escape_sequence() {
    //                     value.push(escaped_char);
    //                 } else {
    //                     // Invalid escape sequence
    //                     return Token::Error(format!("Invalid escape sequence in string literal"));
    //                 }
    //             }
    //             '"' => {
    //                 self.position += 1; // Skip the closing quote
    //                 return Token::StringLiteral(value);
    //             }
    //             _ => {
    //                 value.push(current_char);
    //                 self.position += 1;
    //             }
    //         }
    //     }
    //     // Unclosed string literal
    //     Token::Error(format!("Unclosed string literal"))
    // }

    fn tokenize_char(&mut self) -> Token {
        todo!()
    }

    fn parse_escape_sequence(&mut self) -> Option<char> {
        self.pos += 1; // Skip the backslash
        if self.pos < self.input.len() {
            let escaped_char = match self.input.chars().nth(self.pos).unwrap() {
                'n' => '\n',
                't' => '\t',
                '\\' => '\\',
                '"' => '"',
                // Handle other escape sequences...
                _ => return None, // Invalid escape sequence
            };
            self.pos += 1;
            Some(escaped_char)
        } else {
            None // Incomplete escape sequence
        }
    }

    fn is_keyword(&self, value: &str) -> bool {
        let keywords = vec!["let", "fn", "if", "else", "while", "for"]; // Example keywords
        keywords.contains(&value)
    }

    fn skip_whitespace(&mut self) {
        while self.pos < self.input.len()
            && self.input.chars().nth(self.pos).unwrap().is_whitespace()
        {
            self.pos += 1;
        }
    }
}
