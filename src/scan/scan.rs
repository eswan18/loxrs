use crate::scan::scan_error::ScanError;
use crate::scan::token::{Token, TokenType};

pub fn scan(source: String) -> Result<Vec<Token>, Vec<ScanError>> {
    // This is a placeholder for the scanner implementation.
    println!("Scanning...");
    let mut scanner = Scanner::new(source);
    scanner.scan_tokens()
}

struct Scanner {
    source: String,
    tokens: Vec<Token>,
    errors: Vec<ScanError>,
    // The first character in the lexeme being scanned.
    start: usize,
    // The character currently being considered.
    current: usize,
    // The current line number.
    line: u32,
}

// Public API
impl Scanner {
    pub fn new(source: String) -> Scanner {
        Scanner {
            source,
            tokens: Vec::new(),
            errors: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
        }
    }

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
            _ => {
                self.errors.push(ScanError::UnexpectedCharacter(c));
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
        assert_eq!(errors[0], ScanError::UnexpectedCharacter('['));
        assert_eq!(errors[1], ScanError::UnexpectedCharacter(']'));
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
            .map(|t| t.token_type.clone())
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
}
