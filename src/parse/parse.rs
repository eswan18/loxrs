use crate::ast::{
    Ast, BinaryOperator, BinaryOperatorType, Expr, LiteralValue, Stmt, UnaryOperator,
    UnaryOperatorType,
};
use crate::parse::ParseError;
use crate::token::{Token, TokenType};

pub fn parse(tokens: Vec<Token>) -> Result<Ast, Vec<ParseError>> {
    let mut parser = Parser::new(tokens);
    parser.parse()
}

struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    fn parse(&mut self) -> Result<Vec<Stmt>, Vec<ParseError>> {
        let mut stmts: Vec<Stmt> = Vec::new();
        let mut errors: Vec<ParseError> = Vec::new();
        while !self.is_at_end() {
            match self.parse_declaration() {
                Ok(stmt) => stmts.push(stmt),
                Err(err) => {
                    errors.push(err);
                    self.synchronize();
                }
            }
        }
        if errors.len() > 0 {
            Err(errors)
        } else {
            Ok(stmts)
        }
    }

    // Parsing statements

    fn parse_declaration(&mut self) -> Result<Stmt, ParseError> {
        match self.advance_on_match(&[TokenType::Var]) {
            Some(_) => self.parse_var_declaration(),
            None => self.parse_statement(),
        }
    }

    /// Parse a variable declaration.
    fn parse_var_declaration(&mut self) -> Result<Stmt, ParseError> {
        let name = match self.advance_on_match(&[TokenType::Identifier]) {
            Some(token) => token.lexeme.clone(),
            None => {
                let line = self.peek().unwrap().line;
                return Err(ParseError::ExpectedIdentifier { line });
            }
        };
        // Check if there is an initial value (which is optional).
        let mut initializer: Option<Expr> = None;
        if let Some(_) = self.advance_on_match(&[TokenType::Equal]) {
            let expr = self.parse_expression()?;
            initializer = Some(expr);
        }
        let stmt = Stmt::Var { name, initializer };
        // Consume the semicolon.
        match self.advance_on_match(&[TokenType::Semicolon]) {
            Some(_) => Ok(stmt),
            None => {
                let line = self.peek().unwrap().line;
                return Err(ParseError::ExpectedSemicolon { line });
            }
        }
    }

    /// Parse any non-declaration statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check_current_token_type(&[TokenType::Print]) {
            self.advance(); // Consume the Print token.
            self.parse_print_statement()
        } else {
            self.parse_expression_statement()
        }
    }

    /// Parse a print statement.
    fn parse_print_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expression()?;
        let line = self.peek().unwrap().line;
        match self.advance_on_match(&[TokenType::Semicolon]) {
            Some(_) => Ok(Stmt::Print(expr)),
            None => Err(ParseError::ExpectedSemicolon { line }),
        }
    }

    /// Parse an expression statement.
    fn parse_expression_statement(&mut self) -> Result<Stmt, ParseError> {
        let expr = self.parse_expression()?;
        let line = self.peek().unwrap().line;
        match self.advance_on_match(&[TokenType::Semicolon]) {
            Some(_) => Ok(Stmt::Expression(expr)),
            None => Err(ParseError::ExpectedSemicolon { line }),
        }
    }

    // Parsing expressions

    /// Parse any expression.
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_equality()?;

        // If we hit an equals sign, assignment must be intended.
        if let Some(t) = self.advance_on_match(&[TokenType::Equal]) {
            let line = t.line;
            let value = self.parse_assignment()?;
            return match expr {
                // The assignment is only valid if the left hand side is a variable.
                Expr::Variable { name } => Ok(Expr::Assignment {
                    name,
                    value: Box::new(value),
                }),
                _ => Err(ParseError::InvalidAssignmentTarget { line, expr }),
            };
        }

        Ok(expr)
    }

    /// Parse equality checking expressions.
    fn parse_equality(&mut self) -> Result<Expr, ParseError> {
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
    fn parse_comparison(&mut self) -> Result<Expr, ParseError> {
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
    fn parse_term(&mut self) -> Result<Expr, ParseError> {
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
    fn parse_factor(&mut self) -> Result<Expr, ParseError> {
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
    fn parse_unary(&mut self) -> Result<Expr, ParseError> {
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
    fn parse_primary(&mut self) -> Result<Expr, ParseError> {
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

        if let Some(id_token) = self.advance_on_match(&[TokenType::Identifier]) {
            return Ok(Expr::Variable {
                name: id_token.lexeme.clone(),
            });
        }

        // If we didn't match anything yet, look for a grouping expression.
        if let Some(_) = self.advance_on_match(&[TokenType::LeftParen]) {
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
        while !self.is_at_end() {
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

    fn advance_on_match(&mut self, token_types: &[TokenType]) -> Option<&Token> {
        if self.check_current_token_type(token_types) {
            return Some(self.advance());
        } else {
            return None;
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
        self.tokens[self.current].tp == TokenType::Eof
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

    ////////////////
    // Expressions
    ////////////////

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
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(+ 1 2)");
    }

    #[test]
    fn order_of_arithmetic() {
        let input = "4 + (3 + 45) * 6;";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(+ 4 (* ((+ 3 45)) 6))");
    }

    #[test]
    fn ungrouped_arithmetic() {
        let input = "4 * 3 + 6 / 9;";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(+ (* 4 3) (/ 6 9))");
    }

    #[test]
    fn comparisons() {
        let input = "9 - 4 < 4 * 3 / 9";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(< (- 9 4) (/ (* 4 3) 9))");
    }

    #[test]
    fn equality() {
        let input = "4 != 5 == 6";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(== (!= 4 5) 6)");
    }

    #[test]
    fn unary() {
        let input = "!true == -5";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(== (! true) (- 5))");
    }

    #[test]
    fn assignment() {
        let input = "x = 3 * 4";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(x = (* 3 4))");
    }

    #[test]
    fn error_missing_right_paren() {
        let input = "(4 + 5";
        let tokens = scan(input.to_string()).unwrap();
        let error = Parser::new(tokens).parse_expression().unwrap_err();
        assert_eq!(error, ParseError::MissingRightParen { line: 1 });

        let input = "\n\n(((3 + 4) *5) * 6";
        let tokens = scan(input.to_string()).unwrap();
        let error = Parser::new(tokens).parse_expression().unwrap_err();
        assert_eq!(error, ParseError::MissingRightParen { line: 3 });
    }

    #[test]
    fn error_extra_input() {
        let input = "4 + 5 +";
        let tokens = scan(input.to_string()).unwrap();
        let error = Parser::new(tokens).parse_expression().unwrap_err();
        assert_eq!(error, ParseError::ExtraInput { line: 1 });
    }

    #[test]
    fn error_invalid_assignment_target() {
        let inputs = [
            (
                "3 = 3",
                Expr::Literal {
                    value: LiteralValue::Number(3.0),
                },
            ),
            (
                "3 * x = 3",
                Expr::Binary {
                    left: Box::new(Expr::Literal {
                        value: LiteralValue::Number(3.0),
                    }),
                    operator: BinaryOperator {
                        tp: BinaryOperatorType::Star,
                        line: 1,
                    },
                    right: Box::new(Expr::Variable {
                        name: "x".to_string(),
                    }),
                },
            ),
            (
                "true = 3",
                Expr::Literal {
                    value: LiteralValue::Boolean(true),
                },
            ),
            (
                "\"abc\" = 3",
                Expr::Literal {
                    value: LiteralValue::String("abc".to_string()),
                },
            ),
        ];
        for (input, target) in inputs {
            let tokens = scan(input.to_string()).unwrap();
            let error = Parser::new(tokens).parse_expression().unwrap_err();
            assert_eq!(
                error,
                ParseError::InvalidAssignmentTarget {
                    expr: target,
                    line: 1
                }
            )
        }
    }

    ////////////////
    // Statements
    ////////////////

    #[test]
    fn expr_statement() {
        let input = "4 + 5;";
        let tokens = scan(input.to_string()).unwrap();
        let smts = parse(tokens).unwrap();
        assert_eq!(smts.len(), 1);
        let stmt_str = format!("{}", smts[0]);
        assert_eq!(stmt_str, "(+ 4 5);");
    }

    #[test]
    fn multiline_expr_stmt() {
        let input = "4 + 5;\n\n3 * true;";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 2);
        let stmt0_str = format!("{}", stmts[0]);
        let stmt1_str = format!("{}", stmts[1]);
        assert_eq!(stmt0_str, "(+ 4 5);");
        assert_eq!(stmt1_str, "(* 3 true);");
    }

    #[test]
    fn print_stmt() {
        let input = "print 4 + 5;";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        let stmt_str = format!("{}", stmts[0]);
        assert_eq!(stmt_str, "Print((+ 4 5));");
    }

    #[test]
    fn var_decl_stmt() {
        let input = "var x = 4 + 5;";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        let stmt_str = format!("{}", stmts[0]);
        assert_eq!(stmt_str, "var x = (+ 4 5);");
    }

    #[test]
    fn missing_semicolon() {
        let inputs = ["var x = 4 + 5", "print 4 + 5", "4 + 5"];
        for input in inputs {
            let tokens = scan(input.to_string()).unwrap();
            let error = parse(tokens).unwrap_err();
            assert_eq!(error.len(), 1);
            assert_eq!(error[0], ParseError::ExpectedSemicolon { line: 1 });
        }
    }
}
