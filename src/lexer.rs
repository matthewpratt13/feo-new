#![allow(dead_code)]

use std::{iter::Peekable, str::Chars};

use crate::{
    ast::{IntKind, UIntKind},
    error::{CompilerError, ErrorEmitted, LexErrorKind},
    span::Span,
    token::{Token, TokenStream},
    U256,
};

/// Lexer struct that holds an input string and contains methods to render tokens (tokenize)
/// from characters in that string.
struct Lexer<'a> {
    input: &'a str,
    pos: usize,
    peekable_chars: Peekable<Chars<'a>>,
    errors: Vec<CompilerError<LexErrorKind>>,
}

impl<'a> Lexer<'a> {
    /// Create a new `Lexer` instance.
    /// Initialize an empty `Vec` to store potential errors ,and create an `Iterator`
    /// from the characters in the source string to traverse the input string
    /// and look ahead in the source code without moving forward.
    fn new(input: &'a str) -> Self {
        Lexer {
            input,
            pos: 0,
            peekable_chars: input.chars().peekable(),
            errors: Vec::new(),
        }
    }

    /// Get a list of the lexer's `CompilerError`.
    fn errors(&self) -> Vec<CompilerError<LexErrorKind>> {
        self.errors.clone()
    }

    /// Main lexer function.
    /// Returns a stream of tokens generated from some input string (source code).
    fn lex(&mut self) -> Result<TokenStream, ErrorEmitted> {
        let mut tokens: Vec<Token> = Vec::new();

        while let Some(c) = self.peek_current() {
            let start_pos = self.pos;

            match c {
                _ if c.is_whitespace() => self.skip_whitespace(),

                _ if c == '/' && self.peek_next() == Some('/') || self.peek_next() == Some('*') => {
                    tokens.push(self.tokenize_doc_comment().unwrap_or(Token::EOF));
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
                    self.advance();
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Comma { punc: ',', span })
                }

                ';' => {
                    self.advance();
                    let span = Span::new(self.input, start_pos, self.pos);
                    tokens.push(Token::Semicolon { punc: ';', span })
                }

                '!' | '#' | '%' | '&' | '*' | '+' | '/' | '-' | '.' | ':' | '<' | '=' | '>'
                | '?' | '\\' | '^' | '`' | '|' => tokens.push(self.tokenize_punctuation()?),

                _ if !self.peek_next().is_some() => tokens.push(Token::EOF),

                _ => {
                    return Err(self.log_error(LexErrorKind::UnrecognizedChar {
                        value: c.to_string(),
                    }))
                }
            }
        }

        let stream = TokenStream::new(&tokens, self.input, 0, self.pos);
        Ok(stream)
    }

    /// Tokenize a doc comment, ignoring ordinary line and block comments.
    fn tokenize_doc_comment(&mut self) -> Option<Token> {
        let start_pos = self.pos;

        self.advance(); // skip first `/`

        if let Some('/') = self.peek_current() {
            self.advance(); // skip second `/`

            if self.peek_current() == Some('/') || self.peek_current() == Some('!') {
                self.advance(); // skip third `/` or `!`

                self.skip_whitespace();

                let comment_start_pos = self.pos;

                // advance until the end of the line
                while let Some(c) = self.peek_current() {
                    if c == '\n' {
                        break;
                    }
                    self.advance();
                }

                // trim any trailing whitespace
                let comment = self.input[comment_start_pos..self.pos].trim().to_string();

                self.advance();

                let span = Span::new(self.input, start_pos, self.pos);

                Some(Token::DocComment { comment, span })
            } else {
                // consume ordinary line comment
                loop {
                    if self.peek_next().is_some() {
                        self.advance()
                    } else {
                        break None;
                    }

                    if self.peek_current() == Some('\n') {
                        self.advance();
                        break None;
                    }
                }
            }
        } else if let Some('*') = self.peek_current() {
            self.advance(); // skip `*`

            // consume block comment
            loop {
                if self.peek_next().is_some() {
                    self.advance();
                } else {
                    break None;
                }

                if self.peek_current() == Some('*') && self.peek_next() == Some('/') {
                    self.advance();
                    self.advance();

                    break None;
                }
            }
        } else {
            let span = Span::new(self.input, start_pos, self.pos);
            Some(Token::Slash { punc: '/', span })
        }
    }

    /// Tokenize an identifier and match it against reserved keywords.
    fn tokenize_identifier_or_keyword(&mut self) -> Result<Token, ErrorEmitted> {
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

        if is_keyword(&name) {
            match name.as_str() {
                "abstract" => Ok(Token::Abstract { name, span }),
                "alias" => Ok(Token::Alias { name, span }),
                "as" => Ok(Token::As { name, span }),
                "break" => Ok(Token::Break { name, span }),
                "const" => Ok(Token::Const { name, span }),
                "continue" => Ok(Token::Continue { name, span }),
                "contract" => Ok(Token::Contract { name, span }),
                "else" => Ok(Token::Else { name, span }),
                "enum" => Ok(Token::Enum { name, span }),
                "extern" => Ok(Token::Extern { name, span }),
                "false" => Ok(Token::BoolLiteral {
                    value: name
                        .parse::<bool>()
                        .map_err(|_| self.log_error(LexErrorKind::ParseBoolError))?,
                    span,
                }),
                "for" => Ok(Token::For { name, span }),
                "func" => Ok(Token::Func { name, span }),
                "if" => Ok(Token::If { name, span }),
                "impl" => Ok(Token::Impl { name, span }),
                "import" => Ok(Token::Import { name, span }),
                "in" => Ok(Token::In { name, span }),
                "let" => Ok(Token::Let { name, span }),
                "library" => Ok(Token::Library { name, span }),
                "loop" => Ok(Token::Loop { name, span }),
                "match" => Ok(Token::Match { name, span }),
                "module" => Ok(Token::Module { name, span }),
                "mut" => Ok(Token::Mut { name, span }),
                "package" => Ok(Token::Package { name, span }),
                "payable" => Ok(Token::Payable { name, span }),
                "pub" => Ok(Token::Pub { name, span }),
                "ref" => Ok(Token::Ref { name, span }),
                "return" => Ok(Token::Return { name, span }),
                "self" => Ok(Token::SelfKeyword { name, span }),
                "static" => Ok(Token::Static { name, span }),
                "storage" => Ok(Token::Storage { name, span }),
                "struct" => Ok(Token::Struct { name, span }),
                "super" => Ok(Token::Super { name, span }),
                "test" => Ok(Token::Test { name, span }),
                "topic" => Ok(Token::Topic { name, span }),
                "trait" => Ok(Token::Trait { name, span }),
                "true" => Ok(Token::BoolLiteral {
                    value: name
                        .parse::<bool>()
                        .map_err(|_| self.log_error(LexErrorKind::ParseBoolError))?,
                    span,
                }),
                "unsafe" => Ok(Token::Unsafe { name, span }),
                "while" => Ok(Token::While { name, span }),
                "i32" => Ok(Token::I32Type { name, span }),
                "i64" => Ok(Token::I64Type { name, span }),
                "u8" => Ok(Token::U8Type { name, span }),
                "u16" => Ok(Token::U16Type { name, span }),
                "u32" => Ok(Token::U32Type { name, span }),
                "u64" => Ok(Token::U64Type { name, span }),
                "u256" => Ok(Token::U256Type { name, span }),
                "String" => Ok(Token::StringType { name, span }),
                "char" => Ok(Token::CharType { name, span }),
                "bool" => Ok(Token::BoolType { name, span }),
                _ => Err(self.log_error(LexErrorKind::UnrecognizedKeyword { name })),
            }
        } else {
            Ok(Token::Identifier { name, span })
        }
    }

    /// Tokenize an opening delimiter.
    fn tokenize_delimiter(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        let delim = self
            .peek_current()
            .ok_or(self.log_error(LexErrorKind::CharNotFound {
                expected: "delimiter".to_string(),
            }))?;

        match delim {
            '(' => {
                self.advance(); // skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LParen { delim, span })
            }

            '[' => {
                self.advance(); //  skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBracket { delim, span })
            }

            '{' => {
                self.advance(); // skip opening delimiter
                let span = Span::new(self.input, start_pos, self.pos);
                Ok(Token::LBrace { delim, span })
            }

            ')' => self.tokenize_closing_delimiter(')'),

            ']' => self.tokenize_closing_delimiter(']'),

            '}' => self.tokenize_closing_delimiter('}'),

            _ => Err(self.log_error(LexErrorKind::UnexpectedChar {
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
            .peek_current()
            .ok_or(self.log_error(LexErrorKind::MissingDelimiter { delim: expected }))?;

        if !is_delimiter(delim) {
            return Err(self.log_error(LexErrorKind::UnexpectedChar {
                expected: "delimiter".to_string(),
                found: delim,
            }));
        }

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
                    return Err(self.log_error(LexErrorKind::UnexpectedChar {
                        expected: "closing delimiter".to_string(),
                        found: delim,
                    }))
                }
            }
        } else {
            return Err(self.log_error(LexErrorKind::MismatchedDelimiter {
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

        while let Some(c) = self.peek_current() {
            match c {
                '\\' => {
                    // handle escape sequences
                    if let Some(escaped_char) = self.parse_escape_sequence()? {
                        value.push(escaped_char);
                        self.advance();
                    } else {
                        return Err(self.log_error(LexErrorKind::CharNotFound {
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

        Err(self.log_error(LexErrorKind::MissingQuote { quote: '\"' }))
    }

    /// Tokenize a `char` literal, handling escape sequences where applicable.
    fn tokenize_char(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        self.advance(); // skip opening quote (`'`)

        match self.peek_current() {
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
                        return Err(self.log_error(LexErrorKind::CharNotFound {
                            expected: "escape sequence".to_string(),
                        }));
                    }
                }

                _ if value == ' ' => {
                    return Err(self.log_error(LexErrorKind::EmptyCharLiteral));
                }

                _ => {
                    self.advance();

                    if let Some('\'') = self.peek_current() {
                        self.advance(); // skip closing quote (`'`)

                        let span = Span::new(self.input, start_pos, self.pos);

                        Ok(Token::CharLiteral { value, span })
                    } else {
                        return Err(self.log_error(LexErrorKind::MissingQuote { quote: '\'' }));
                    }
                }
            },

            None => Err(self.log_error(LexErrorKind::CharNotFound {
                expected: "character literal".to_string(),
            })),
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
            .parse::<U256>()
            .map_err(|_| self.log_error(LexErrorKind::ParseHexError))?;

        let span = Span::new(self.input, start_pos, self.pos);

        Ok(Token::U256Literal { value, span })
    }

    /// Tokenize a numeric value (i.e., `i64` or `u64`).
    /// Parse to `u64` unless a `-` is encountered, in which case parse to `i64`.
    fn tokenize_numeric(&mut self) -> Result<Token, ErrorEmitted> {
        let mut is_negative = false;

        // check for `-` before the number to decide which type of integer to parse to
        if self.peek_current() == Some('-') && self.peek_next().is_some_and(|c| c.is_digit(10)) {
            is_negative = true;
            self.advance(); // skip '-'
        }

        // go back and read from previous char ('-') if negative, else read from current position
        let start_pos = if is_negative { self.pos - 1 } else { self.pos };

        while let Some(c) = self.peek_current() {
            if c.is_digit(10) || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        if is_negative {
            // remove the `_` separators before parsing
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<i64>()
                .map_err(|_| self.log_error(LexErrorKind::ParseIntError))?;

            let span = Span::new(self.input, start_pos, self.pos);

            Ok(Token::IntLiteral {
                value: IntKind::I64(value),
                span,
            })
        } else {
            // remove the `_` separators before parsing (if they exist)
            let value = self.input[start_pos..self.pos]
                .split('_')
                .collect::<Vec<&str>>()
                .concat()
                .parse::<u64>()
                .map_err(|_| self.log_error(LexErrorKind::ParseUIntError))?;

            let span = Span::new(self.input, start_pos, self.pos);

            Ok(Token::UIntLiteral {
                value: UIntKind::U64(value),
                span,
            })
        }
    }

    /// Tokenize punctuation.
    fn tokenize_punctuation(&mut self) -> Result<Token, ErrorEmitted> {
        let start_pos = self.pos;

        while let Some(c) = self.peek_current() {
            if c.is_ascii_punctuation() && !is_delimiter(c) && !is_separator(c) && !is_quote(c) {
                self.advance();
            } else {
                break;
            }
        }

        let punc_string = self.input[start_pos..self.pos].trim().to_string();

        let punc = punc_string.parse::<char>().map_err(|_| {
            self.log_error(LexErrorKind::UnrecognizedChar {
                value: punc_string.clone(),
            })
        })?;

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
            _ => Err(self.log_error(LexErrorKind::UnrecognizedChar { value: punc_string })),
        }
    }

    /// Parse an escape sequence found in a string or character literal.
    fn parse_escape_sequence(&mut self) -> Result<Option<char>, ErrorEmitted> {
        self.advance(); // skip backslash

        if self.pos < self.input.len() {
            let escaped_char = match self.peek_current() {
                Some('n') => '\n',
                Some('r') => '\r',
                Some('t') => '\t',
                Some('\\') => '\\',
                Some('0') => '\0',
                Some('\'') => '\'',
                Some('"') => '"',
                Some(c) => {
                    return Err(self.log_error(LexErrorKind::InvalidEscapeSequence { sequence: c }));
                }
                None => {
                    return Err(self.log_error(LexErrorKind::CharNotFound {
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

    /// Advance the lexer (and iterator) by one character.
    fn advance(&mut self) {
        self.pos += 1; // update lexer's position
        self.peekable_chars.next(); // move to next character in the iterator (discard output)
    }

    /// Get the character at the current position.
    fn peek_current(&mut self) -> Option<char> {
        self.peekable_chars.peek().cloned()
    }

    /// Get the next character without advancing the iterator.
    fn peek_next(&self) -> Option<char> {
        let mut cloned_iter = self.peekable_chars.clone();
        cloned_iter.next();
        cloned_iter.peek().cloned()
    }

    /// Get the character at position `n` without advancing the iterator
    fn peek_nth(&self, n: usize) -> Option<char> {
        self.peekable_chars.clone().nth(n)
    }

    /// Skip the source string's whitespace, which is considered unnecessary during tokenization.
    fn skip_whitespace(&mut self) {
        // while self.pos < self.input.len()
        //     && self.peek_nth(self.pos).unwrap().is_whitespace()
        // {
        //     self.advance();
        // }

        while let Some(c) = self.peek_current() {
            if c.is_whitespace() {
                // if the current char is whitespace, advance to the next char
                self.advance();
            } else {
                // if the current char is not whitespace, break out of the loop
                break;
            }
        }
    }

    /// Log and store information about an error that occurred during lexing.
    /// Return `ErrorEmitted` just to confirm that the action happened.
    fn log_error(&mut self, error_kind: LexErrorKind) -> ErrorEmitted {
        let error = CompilerError::new(error_kind, self.input, self.pos);

        self.errors.push(error);
        ErrorEmitted(())
    }
}

/// Check if some substring is a reserved keyword (as opposed to an ordinary identifier).
fn is_keyword(value: &str) -> bool {
    [
        "abstract", "alias", "as", "break", "const", "continue", "contract", "else", "enum",
        "extern", "false", "for", "func", "if", "impl", "import", "in", "let", "library", "loop",
        "match", "module", "mut", "package", "payable", "pub", "ref", "return", "self", "static",
        "storage", "struct", "super", "test", "topic", "trait", "true", "unsafe", "while", "i32",
        "i64", "u8", "u16", "u32", "u64", "u256", "String", "char", "bool",
    ]
    .contains(&value)
}

/// Check if a character is a delimiter.
fn is_delimiter(value: char) -> bool {
    ['(', ')', '{', '}', '[', ']'].contains(&value)
}

/// Check if a character is a `;` or `,`.
fn is_separator(value: char) -> bool {
    [';', ','].contains(&value)
}

/// Check if a character is a `'` or `"`.
fn is_quote(value: char) -> bool {
    ['\'', '\"'].contains(&value)
}

#[cfg(test)]
mod tests {

    use super::*;

    #[test]
    fn tokenize_assignment_stmt() {
        let input = r#"let x: u256 = 0x1234_ABCD;"#;

        let mut lexer = Lexer::new(input);

        let stream = lexer.lex().expect(&format!(
            "unable to tokenize input. token at current position: `{:?}`\n{:#?}",
            lexer.peek_current(),
            lexer.errors()
        ));

        println!("{:#?}", stream);
    }
}
