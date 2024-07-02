use std::collections::HashMap;

use log::info;

use crate::scan::scan_error::ScanError;
use crate::token::{Token, TokenType};

/// Scan the source code and return a vector of tokens.
pub fn scan(source: String) -> Result<Vec<Token>, Vec<ScanError>> {
    // This is a placeholder for the scanner implementation.
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens()
}

/// A scanner for the Lox language.
struct Scanner {
    // The source code.
    source: String,
    // The tokens scanned so far.
    tokens: Vec<Token>,
    // Any errors encountered during scanning.
    errors: Vec<ScanError>,
    /// The first character in the lexeme being scanned.
    start: usize,
    // The character currently being considered.
    current: usize,
    // The current line number.
    line: u32,
    // A static mapping of keywords to their token types.
    keyword_tokens: HashMap<&'static str, TokenType>,
}

// Public API
impl Scanner {
    pub fn new(source: String) -> Self {
        Scanner {
            source,
            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            keyword_tokens: Self::keyword_tokens(),
        }
    }

    /// Scan the source code and return a vector of tokens.
    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<ScanError>> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_next_token();
        }
        // Add the EOF token.
        let eof_token = Token::new(TokenType::Eof, "".to_string(), self.line);
        self.tokens.push(eof_token);

        if self.errors.len() > 0 {
            // If we got any errors, return those.
            return Err(self.errors.clone());
        } else {
            // Otherwise, return the tokens.
            let token_string = self
                .tokens
                .iter()
                .map(|t| t.to_string())
                .collect::<Vec<String>>()
                .join(" ");
            info!("Tokens: {}", token_string);
            return Ok(self.tokens.clone());
        }
    }
}

// Private API
impl Scanner {
    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn scan_next_token(&mut self) {
        let c = match self.advance() {
            Some(c) => c,
            None => return,
        };

        let token_type = match c {
            '(' => Some(TokenType::LeftParen),
            ')' => Some(TokenType::RightParen),
            '{' => Some(TokenType::LeftBrace),
            '}' => Some(TokenType::RightBrace),
            ',' => Some(TokenType::Comma),
            '.' => Some(TokenType::Dot),
            '-' => Some(TokenType::Minus),
            '+' => Some(TokenType::Plus),
            ';' => Some(TokenType::Semicolon),
            '*' => Some(TokenType::Star),
            '!' => match self.advance_on_match('=') {
                true => Some(TokenType::BangEqual),
                false => Some(TokenType::Bang),
            },
            '=' => match self.advance_on_match('=') {
                true => Some(TokenType::EqualEqual),
                false => Some(TokenType::Equal),
            },
            '<' => match self.advance_on_match('=') {
                true => Some(TokenType::LessEqual),
                false => Some(TokenType::Less),
            },
            '>' => match self.advance_on_match('=') {
                true => Some(TokenType::GreaterEqual),
                false => Some(TokenType::Greater),
            },
            '/' => match self.advance_on_match('/') {
                true => {
                    while let Some(c) = self.peek() {
                        if c == '\n' {
                            break;
                        }
                        self.advance();
                    }
                    None
                }
                false => Some(TokenType::Slash),
            },
            ' ' | '\r' | '\t' => None,
            '\n' => {
                self.line += 1;
                None
            }
            '"' => self.scan_string(),
            c if c.is_ascii_digit() => self.scan_number(),
            c if c.is_ascii_alphabetic() || c == '_' => Some(self.scan_identifier_or_keyword()),
            _ => {
                self.errors.push(ScanError::UnexpectedCharacter {
                    char: c,
                    line: self.line,
                });
                None
            }
        };
        let lexeme = self.source[self.start..self.current].to_string();
        if let Some(token_type) = token_type {
            let token = Token::new(token_type, lexeme, self.line);
            self.tokens.push(token);
        }
    }

    // Get the next character in the source string and advance the current index.
    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }

    // This method is named "match" in the book.
    fn advance_on_match(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        match self.source.chars().nth(self.current) {
            Some(c) if c == expected => {
                self.current += 1;
                true
            }
            // The fallthrough case handles both being at the end of the source string
            // (self.current >= self.source.len()) or the current character not matching
            // the expected.
            _ => false,
        }
    }

    fn peek(&self) -> Option<char> {
        self.source.chars().nth(self.current)
    }

    fn peek_next(&self) -> Option<char> {
        self.source.chars().nth(self.current + 1)
    }

    fn scan_string(&mut self) -> Option<TokenType> {
        while let Some(c) = self.peek() {
            match c {
                '"' => break,
                '\n' => self.line += 1,
                _ => (),
            }
            self.advance();
        }
        if self.is_at_end() {
            self.errors
                .push(ScanError::UnterminatedString { line: self.line });
            return None;
        }

        // Consume the closing ".
        self.advance();

        // Trim the quotes off the string we've scanned.
        let raw_value = self.source[self.start..self.current].to_string();
        let value = raw_value[1..raw_value.len() - 1].to_string();
        Some(TokenType::String(value))
    }

    fn scan_number(&mut self) -> Option<TokenType> {
        // Consume as many digits as we find in a row.
        while let Some(c) = self.peek() {
            if !c.is_ascii_digit() {
                break;
            }
            self.advance();
        }

        // Look for a fractional part.
        if let (Some('.'), Some(d)) = (self.peek(), self.peek_next()) {
            if d.is_ascii_digit() {
                // Consume the ".".
                self.advance();

                // Consume the post-decimal digits.
                while let Some(c) = self.peek() {
                    if !c.is_ascii_digit() {
                        break;
                    }
                    self.advance();
                }
            }
        }

        let value = self.source[self.start..self.current]
            .to_string()
            .parse::<f64>()
            .unwrap(); // unwrapping here is okay because we've already validated the string is a number.
        Some(TokenType::Number(value))
    }

    fn scan_identifier_or_keyword(&mut self) -> TokenType {
        while let Some(c) = self.peek() {
            if c.is_ascii_alphanumeric() || c == '_' {
                self.advance();
            } else {
                break;
            }
        }

        let lexeme = self.source[self.start..self.current].to_string();
        match self.keyword_tokens.get(lexeme.as_str()) {
            Some(token_type) => token_type.clone(),
            None => TokenType::Identifier,
        }
    }

    fn keyword_tokens() -> HashMap<&'static str, TokenType> {
        let mut keywords = HashMap::new();
        keywords.insert("and", TokenType::And);
        keywords.insert("class", TokenType::Class);
        keywords.insert("else", TokenType::Else);
        keywords.insert("false", TokenType::False);
        keywords.insert("for", TokenType::For);
        keywords.insert("fun", TokenType::Fun);
        keywords.insert("if", TokenType::If);
        keywords.insert("nil", TokenType::Nil);
        keywords.insert("or", TokenType::Or);
        keywords.insert("print", TokenType::Print);
        keywords.insert("return", TokenType::Return);
        keywords.insert("super", TokenType::Super);
        keywords.insert("this", TokenType::This);
        keywords.insert("true", TokenType::True);
        keywords.insert("var", TokenType::Var);
        keywords.insert("while", TokenType::While);
        keywords
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_symbols() {
        let source = "(){},.-+;*".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 11);
        assert_eq!(
            tokens[0],
            Token::new(TokenType::LeftParen, "(".to_string(), 1)
        );
        assert_eq!(
            tokens.iter().last().unwrap(),
            &Token::new(TokenType::Eof, "".to_string(), 1)
        );
    }

    #[test]
    fn test_invalid_chars() {
        let source = "[]".to_string();
        let errors = scan(source).unwrap_err();
        assert_eq!(errors.len(), 2);
        assert_eq!(
            errors[0],
            ScanError::UnexpectedCharacter { char: '[', line: 1 }
        );
        assert_eq!(
            errors[1],
            ScanError::UnexpectedCharacter { char: ']', line: 1 }
        );
    }

    #[test]
    fn test_scan_multiline() {
        let source = "*(){}\n*".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 7);
        assert_eq!(tokens[0], Token::new(TokenType::Star, "*".to_string(), 1));
        assert_eq!(
            tokens[1],
            Token::new(TokenType::LeftParen, "(".to_string(), 1)
        );
        assert_eq!(tokens[5], Token::new(TokenType::Star, "*".to_string(), 2));
        assert_eq!(tokens[6], Token::new(TokenType::Eof, "".to_string(), 2));
    }

    #[test]
    fn test_multicharacter_tokens() {
        let source = "= != == <= >=".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 6);
        let token_types = tokens
            .iter()
            .map(|t| t.tp.clone())
            .collect::<Vec<TokenType>>();
        assert_eq!(
            token_types,
            vec![
                TokenType::Equal,
                TokenType::BangEqual,
                TokenType::EqualEqual,
                TokenType::LessEqual,
                TokenType::GreaterEqual,
                TokenType::Eof,
            ]
        )
    }

    #[test]
    fn test_comments() {
        let source = "* + / // This is a comment***\n()".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0], Token::new(TokenType::Star, "*".to_string(), 1));
        assert_eq!(tokens[1], Token::new(TokenType::Plus, "+".to_string(), 1));
        assert_eq!(tokens[2], Token::new(TokenType::Slash, "/".to_string(), 1));
        assert_eq!(
            tokens[3],
            Token::new(TokenType::LeftParen, "(".to_string(), 2)
        );
        assert_eq!(
            tokens[4],
            Token::new(TokenType::RightParen, ")".to_string(), 2)
        );
        assert_eq!(tokens[5], Token::new(TokenType::Eof, "".to_string(), 2));
    }

    #[test]
    fn test_strings_simple() {
        let source = "\"abcdef\"".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens[0],
            Token::new(
                TokenType::String("abcdef".to_string()),
                "\"abcdef\"".to_string(),
                1
            )
        );
    }

    #[test]
    fn test_strings() {
        let source = "(\"abcdef\" \n + \n \"ghijkl\")".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 6);
        let token_types = tokens
            .iter()
            .map(|t| t.tp.clone())
            .collect::<Vec<TokenType>>();
        assert_eq!(
            token_types,
            vec![
                TokenType::LeftParen,
                TokenType::String("abcdef".to_string()),
                TokenType::Plus,
                TokenType::String("ghijkl".to_string()),
                TokenType::RightParen,
                TokenType::Eof
            ]
        );
        assert_eq!(tokens[3].lexeme, "\"ghijkl\"");
        assert_eq!(tokens[3].line, 3);
    }

    #[test]
    fn test_multline_string() {
        let source = "\"abc\ndef\"".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 2);
        assert_eq!(
            tokens[0],
            Token::new(
                TokenType::String("abc\ndef".to_string()),
                "\"abc\ndef\"".to_string(),
                2
            )
        );
    }

    #[test]
    fn test_numbers() {
        let source = "123\n4.3\n0.00".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 4);
        assert_eq!(
            tokens[0],
            Token::new(TokenType::Number(123.0), "123".to_string(), 1)
        );
        assert_eq!(
            tokens[1],
            Token::new(TokenType::Number(4.3), "4.3".to_string(), 2)
        );
        assert_eq!(
            tokens[2],
            Token::new(TokenType::Number(0.0), "0.00".to_string(), 3)
        );
    }

    #[test]
    fn test_invalid_numbers() {
        let source = ".123 4.5.5".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 6);
        assert_eq!(tokens[0], Token::new(TokenType::Dot, ".".to_string(), 1));
        assert_eq!(
            tokens[1],
            Token::new(TokenType::Number(123.0), "123".to_string(), 1)
        );
        assert_eq!(
            tokens[2],
            Token::new(TokenType::Number(4.5), "4.5".to_string(), 1)
        );
        assert_eq!(tokens[3], Token::new(TokenType::Dot, ".".to_string(), 1));
        assert_eq!(
            tokens[4],
            Token::new(TokenType::Number(5.0), "5".to_string(), 1)
        );
    }

    #[test]
    fn test_ids_and_keywords() {
        let source = "abc fun class classy land".to_string();
        let tokens = scan(source).unwrap();
        assert_eq!(tokens.len(), 6);
        let token_types = tokens
            .iter()
            .map(|t| t.tp.clone())
            .collect::<Vec<TokenType>>();
        assert_eq!(
            token_types,
            vec![
                TokenType::Identifier,
                TokenType::Fun,
                TokenType::Class,
                TokenType::Identifier,
                TokenType::Identifier,
                TokenType::Eof
            ]
        );
    }
}
