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
        let eof = Token::new(TokenType::Eof, "".to_string(), self.line);
        self.tokens.push(eof);

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
            '(' => TokenType::LeftParen,
            ')' => TokenType::RightParen,
            '{' => TokenType::LeftBrace,
            '}' => TokenType::RightBrace,
            ',' => TokenType::Comma,
            '.' => TokenType::Dot,
            '-' => TokenType::Minus,
            '+' => TokenType::Plus,
            ';' => TokenType::Semicolon,
            '*' => TokenType::Star,
            _ => {
                self.errors.push(ScanError::UnexpectedCharacter(c));
                return;
            }
        };
        let lexeme = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(token_type, lexeme, self.line));
    }

    // Get the next character in the source string and advance the current index.
    fn advance(&mut self) -> Option<char> {
        self.current += 1;
        self.source.chars().nth(self.current - 1)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_scan_symbols() {
        let source = "(){},.-+;*".to_string();
        let mut scanner = Scanner::new(source);
        let tokens = scanner.scan_tokens().unwrap();
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
        let mut scanner = Scanner::new(source);
        let errors = scanner.scan_tokens().unwrap_err();
        assert_eq!(errors.len(), 2);
        assert_eq!(errors[0], ScanError::UnexpectedCharacter('['));
        assert_eq!(errors[1], ScanError::UnexpectedCharacter(']'));
    }
}
