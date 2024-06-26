use crate::expr::{
    BinaryOperator, BinaryOperatorType, Expr, LiteralValue, UnaryOperator, UnaryOperatorType,
};
use crate::parse::ParseError;
use crate::token::{Token, TokenType};

type ParseResult = Result<Expr, ParseError>;

pub fn parse(tokens: Vec<Token>) -> ParseResult {
    let mut parser = Parser::new(tokens);
    parser.parse_expression()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    // Parse methods for specific expressions.

    /// Parse any expression.
    fn parse_expression(&mut self) -> ParseResult {
        self.parse_equality()
    }

    /// Parse equality checking expressions.
    fn parse_equality(&mut self) -> ParseResult {
        let mut expr = self.parse_comparison()?;

        let equality_operators = [TokenType::BangEqual, TokenType::EqualEqual];
        while self.check_current_token_type(&equality_operators) {
            // Unwrapping here is safe because we just checked that the token is one of these.
            let token = self.peek().unwrap();
            let op_type = match token.tp {
                TokenType::BangEqual => BinaryOperatorType::BangEqual,
                TokenType::EqualEqual => BinaryOperatorType::EqualEqual,
                _ => unreachable!("We just checked that this is one of the equality operators"),
            };
            let operator = BinaryOperator {
                tp: op_type,
                line: token.line,
            };
            self.advance();
            let right = self.parse_comparison()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse binary comparison expressions.
    fn parse_comparison(&mut self) -> ParseResult {
        let mut expr = self.parse_term()?;

        let comparison_operators = [
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ];
        while self.check_current_token_type(&comparison_operators) {
            // Unwrapping here is safe because we just checked that the token is one of these.
            let token = self.peek().unwrap();
            let op_type = match token.tp {
                TokenType::Greater => BinaryOperatorType::Greater,
                TokenType::GreaterEqual => BinaryOperatorType::GreaterEqual,
                TokenType::Less => BinaryOperatorType::Less,
                TokenType::LessEqual => BinaryOperatorType::LessEqual,
                _ => unreachable!("We just checked that this is one of the comparison operators"),
            };
            let operator = BinaryOperator {
                tp: op_type,
                line: token.line,
            };
            self.advance();
            let right = self.parse_term()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse addition and subtraction expressions.
    fn parse_term(&mut self) -> ParseResult {
        let mut expr = self.parse_factor()?;

        let term_operators = [TokenType::Minus, TokenType::Plus];
        while self.check_current_token_type(&term_operators) {
            // Unwrapping here is safe because we just checked that the token is one of these.
            let token = self.peek().unwrap();
            let operator = match token.tp {
                TokenType::Minus => BinaryOperatorType::Minus,
                TokenType::Plus => BinaryOperatorType::Plus,
                _ => unreachable!("We just checked that this is one of the term operators"),
            };
            let operator = BinaryOperator {
                tp: operator,
                line: token.line,
            };
            self.advance();
            let right = self.parse_factor()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse multiplication and division expressions.
    fn parse_factor(&mut self) -> ParseResult {
        let mut expr = self.parse_unary()?;

        let factor_operators = [TokenType::Slash, TokenType::Star];
        while self.check_current_token_type(&factor_operators) {
            // Unwrapping here is safe because we just checked that the token is one of these.
            let token = self.peek().unwrap();
            let operator = match token.tp {
                TokenType::Slash => BinaryOperatorType::Slash,
                TokenType::Star => BinaryOperatorType::Star,
                _ => unreachable!("We just checked that this is one of the factor operators"),
            };
            let operator = BinaryOperator {
                tp: operator,
                line: token.line,
            };
            self.advance();
            let right = self.parse_unary()?;
            expr = Expr::Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    /// Parse unary expressions.
    fn parse_unary(&mut self) -> ParseResult {
        let unary_operators = [TokenType::Bang, TokenType::Minus];
        if self.check_current_token_type(&unary_operators) {
            // Unwrapping here is safe because we just checked that the token is one of these.
            let token = self.peek().unwrap();
            let operator = match token.tp {
                TokenType::Bang => UnaryOperatorType::Bang,
                TokenType::Minus => UnaryOperatorType::Minus,
                _ => unreachable!("We just checked that this is one of the unary operators"),
            };
            let operator = UnaryOperator {
                tp: operator,
                line: token.line,
            };
            self.advance();
            let right = self.parse_unary()?;
            return Ok(Expr::Unary {
                operator,
                right: Box::new(right),
            });
        }
        self.parse_primary()
    }

    /// Parse primary expressions.
    fn parse_primary(&mut self) -> ParseResult {
        let token = self.peek().unwrap();
        let literal_expr = match &token.tp {
            TokenType::False => Some(Expr::Literal {
                value: LiteralValue::Boolean(false),
            }),
            TokenType::True => Some(Expr::Literal {
                value: LiteralValue::Boolean(true),
            }),
            TokenType::Nil => Some(Expr::Literal {
                value: LiteralValue::Nil,
            }),
            TokenType::Number(value) => Some(Expr::Literal {
                value: LiteralValue::Number(value.clone()),
            }),
            TokenType::String(value) => Some(Expr::Literal {
                value: LiteralValue::String(value.clone()),
            }),
            _ => None,
        };
        if let Some(expr) = literal_expr {
            // Consume the input token.
            self.advance();
            return Ok(expr);
        }

        // If we didn't match anything yet, look for a grouping expression.
        if self.advance_on_match(&[TokenType::LeftParen]) {
            let expr = self.parse_expression()?;
            match self.peek() {
                Some(token) if token.tp == TokenType::RightParen => {
                    self.advance();
                }
                Some(token) => {
                    return Err(ParseError::MissingRightParen { line: token.line });
                }
                None => {
                    let last_line = self.tokens.last().unwrap().line;
                    return Err(ParseError::MissingRightParen { line: last_line });
                }
            };
            return Ok(Expr::Grouping {
                expression: Box::new(expr),
            });
        }
        let last_line = self.tokens.last().unwrap().line;
        let line = match self.peek() {
            Some(token) => token.line,
            None => last_line,
        };
        Err(ParseError::ExtraInput { line })
    }

    /// Move the current position to the next beginning of a statement.
    fn synchronize(&mut self) -> () {
        loop {
            let t = match self.peek() {
                Some(token) => token,
                None => return,
            };
            if t.tp == TokenType::Semicolon {
                self.advance();
                return;
            }
            match t.tp {
                TokenType::Class => return,
                TokenType::Fun => return,
                TokenType::Var => return,
                TokenType::For => return,
                TokenType::If => return,
                TokenType::While => return,
                TokenType::Print => return,
                TokenType::Return => return,
                _ => {}
            }
            self.advance();
        }
    }

    // Helper methods for parsing.

    fn advance_on_match(&mut self, token_types: &[TokenType]) -> bool {
        if self.check_current_token_type(token_types) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn check_current_token_type(&self, token_types: &[TokenType]) -> bool {
        let current_token = match self.peek() {
            Some(token) => token,
            None => return false,
        };
        for token_type in token_types {
            if current_token.tp == *token_type {
                return true;
            }
        }
        false
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.tokens.len()
    }

    fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.current)
    }

    fn advance(&mut self) -> &Token {
        let token = self.tokens.get(self.current).unwrap();
        if !self.is_at_end() {
            self.current += 1;
        }
        token
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::scan::scan;

    #[test]
    fn simple_input() {
        let tokens = vec![
            Token {
                tp: TokenType::Number(1.0),
                lexeme: "1".to_string(),
                line: 1,
            },
            Token {
                tp: TokenType::Plus,
                lexeme: "+".to_string(),
                line: 1,
            },
            Token {
                tp: TokenType::Number(2.0),
                lexeme: "2".to_string(),
                line: 1,
            },
        ];
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(+ 1 2)");
    }

    #[test]
    fn order_of_arithmetic() {
        let input = "4 + (3 + 45) * 6;";
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(+ 4 (* ((+ 3 45)) 6))");
    }

    #[test]
    fn ungrouped_arithmetic() {
        let input = "4 * 3 + 6 / 9;";
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(+ (* 4 3) (/ 6 9))");
    }

    #[test]
    fn comparisons() {
        let input = "9 - 4 < 4 * 3 / 9";
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(< (- 9 4) (/ (* 4 3) 9))");
    }

    #[test]
    fn equality() {
        let input = "4 != 5 == 6";
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(== (!= 4 5) 6)");
    }

    #[test]
    fn unary() {
        let input = "!true == -5";
        let tokens = scan(input.to_string()).unwrap();
        let ast = parse(tokens).unwrap();
        let ast_str = format!("{}", ast);
        assert_eq!(ast_str, "(== (! true) (- 5))");
    }

    #[test]
    fn error_missing_right_paren() {
        let input = "(4 + 5";
        let tokens = scan(input.to_string()).unwrap();
        let error = parse(tokens).unwrap_err();
        assert_eq!(error, ParseError::MissingRightParen { line: 1 });

        let input = "\n\n(((3 + 4) *5) * 6";
        let tokens = scan(input.to_string()).unwrap();
        let error = parse(tokens).unwrap_err();
        assert_eq!(error, ParseError::MissingRightParen { line: 3 });
    }

    #[test]
    fn error_extra_input() {
        let input = "4 + 5 +";
        let tokens = scan(input.to_string()).unwrap();
        let error = parse(tokens).unwrap_err();
        assert_eq!(error, ParseError::ExtraInput { line: 1 });
    }
}
