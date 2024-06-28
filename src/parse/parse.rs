use crate::ast::{
    Ast, BinaryOperator, BinaryOperatorType, Expr, LiteralValue, LogicalOperator,
    LogicalOperatorType, Stmt, UnaryOperator, UnaryOperatorType,
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
        if self.check_current_token_type(&[TokenType::Var]) {
            self.parse_var_declaration()
        } else {
            self.parse_statement()
        }
    }

    /// Parse a variable declaration.
    fn parse_var_declaration(&mut self) -> Result<Stmt, ParseError> {
        // Consume the 'var' token.
        let var_token = self.advance();
        let line = var_token.line;
        let name = match self.advance_on_match(&[TokenType::Identifier]) {
            Some(token) => token.lexeme.clone(),
            None => {
                return Err(ParseError::ExpectedIdentifier { line });
            }
        };
        // Check if there is an initial value (which is optional).
        let mut initializer: Option<Expr> = None;
        if self.advance_on_match(&[TokenType::Equal]).is_some() {
            let expr = self.parse_expression()?;
            initializer = Some(expr);
        }
        let stmt = Stmt::Var { name, initializer };
        // Consume the semicolon.
        match self.advance_on_match(&[TokenType::Semicolon]) {
            Some(_) => Ok(stmt),
            None => {
                return Err(ParseError::ExpectedSemicolon { line });
            }
        }
    }

    /// Parse a while statement.
    fn parse_while_statement(&mut self) -> Result<Stmt, ParseError> {
        // Consume the while token.
        let while_token = self.advance();
        let line = while_token.line;
        // Consume the left paren.
        if self.advance_on_match(&[TokenType::LeftParen]).is_none() {
            return Err(ParseError::ExpectedLeftParen { line });
        };
        let condition = self.parse_expression()?;
        // Consume the right paren.
        if self.advance_on_match(&[TokenType::RightParen]).is_none() {
            return Err(ParseError::ExpectedRightParen { line });
        };
        let body = Box::new(self.parse_statement()?);
        Ok(Stmt::While { condition, body })
    }

    /// Parse any non-declaration statement.
    fn parse_statement(&mut self) -> Result<Stmt, ParseError> {
        if self.check_current_token_type(&[TokenType::For]) {
            self.parse_for_statement()
        } else if self.check_current_token_type(&[TokenType::If]) {
            self.parse_if_statement()
        } else if self.check_current_token_type(&[TokenType::Print]) {
            self.parse_print_statement()
        } else if self.check_current_token_type(&[TokenType::While]) {
            self.parse_while_statement()
        } else if self.check_current_token_type(&[TokenType::LeftBrace]) {
            self.parse_block()
        } else {
            self.parse_expression_statement()
        }
    }

    /// Parse a for statement.
    fn parse_for_statement(&mut self) -> Result<Stmt, ParseError> {
        // Consume the `for` token.
        let for_token = self.advance();
        let line = for_token.line;
        // Consume the left paren.
        if self.advance_on_match(&[TokenType::LeftParen]).is_none() {
            return Err(ParseError::ExpectedLeftParen { line });
        }
        // Check for an initializer expression: it can be a var statement, an expression statement, or empty (just a semicolon).
        let mut initializer: Option<Box<Stmt>> = None;
        if self.advance_on_match(&[TokenType::Semicolon]).is_none() {
            let init_stmt = if self.check_current_token_type(&[TokenType::Var]) {
                self.parse_var_declaration()?
            } else {
                self.parse_expression_statement()?
            };
            initializer = Some(Box::new(init_stmt));
        }
        // Check for a condition: an expression or empty.
        let mut condition: Option<Expr> = None;
        if !self.check_current_token_type(&[TokenType::Semicolon]) {
            condition = Some(self.parse_expression()?)
        }
        // Consume the semicolon after the condition.
        if self.advance_on_match(&[TokenType::Semicolon]).is_none() {
            return Err(ParseError::ExpectedSemicolon { line });
        }
        // Check for an increment expression: also optional.
        let mut increment: Option<Expr> = None;
        if !self.check_current_token_type(&[TokenType::RightParen]) {
            increment = Some(self.parse_expression()?);
        }
        // Consume the right paren after the conditoin
        if self.advance_on_match(&[TokenType::RightParen]).is_none() {
            return Err(ParseError::MissingRightParen { line });
        }
        // Parse the body.
        let mut body = self.parse_statement()?;

        if let Some(inc_expr) = increment {
            body = Stmt::Block(vec![body, Stmt::Expression(inc_expr)]);
        };
        let condition = match condition {
            Some(cond) => cond,
            None => Expr::Literal {
                value: LiteralValue::Boolean(true),
            },
        };
        let for_stmt = Stmt::While {
            condition,
            body: Box::new(body),
        };
        let for_stmt = match initializer {
            Some(init_stmt) => Stmt::Block(vec![*init_stmt, for_stmt]),
            None => for_stmt,
        };

        Ok(for_stmt)
    }

    fn parse_if_statement(&mut self) -> Result<Stmt, ParseError> {
        // Consume the 'if' token.
        let if_token = self.advance().clone();
        // Start by parsing the parens and enclosed condition expression.
        if self.advance_on_match(&[TokenType::LeftParen]).is_none() {
            return Err(ParseError::ExpectedLeftParen {
                line: if_token.line,
            });
        }
        let condition = self.parse_expression()?;
        if self.advance_on_match(&[TokenType::RightParen]).is_none() {
            return Err(ParseError::ExpectedRightParen {
                line: if_token.line,
            });
        }
        // Parse the then-branch (probably a block).
        let then_branch = Box::new(self.parse_statement()?);
        // Parse the else and its statement, if found.
        let else_branch = match self.advance_on_match(&[TokenType::Else]) {
            Some(_else_token) => {
                let else_branch = self.parse_statement()?;
                Some(Box::new(else_branch))
            }
            None => None,
        };
        Ok(Stmt::If {
            condition,
            then_branch,
            else_branch,
        })
    }

    /// Parse a print statement.
    fn parse_print_statement(&mut self) -> Result<Stmt, ParseError> {
        // Consume the print token.
        let print_token = self.advance();
        let line = print_token.line;
        let expr = self.parse_expression()?;
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

    /// Parse a block of statements.
    fn parse_block(&mut self) -> Result<Stmt, ParseError> {
        self.advance(); // Consume the '{'
        let mut stmts: Vec<Stmt> = Vec::new();
        while !self.check_current_token_type(&[TokenType::RightBrace]) && !self.is_at_end() {
            stmts.push(self.parse_declaration()?);
        }
        let block_stmt = Stmt::Block(stmts);
        match self.advance() {
            Token { tp, .. } if *tp == TokenType::RightBrace => Ok(block_stmt),
            Token { line, .. } => Err(ParseError::ExpectedRightBrace { line: *line }),
        }
    }

    // Parsing expressions

    /// Parse any expression.
    fn parse_expression(&mut self) -> Result<Expr, ParseError> {
        self.parse_assignment()
    }

    fn parse_assignment(&mut self) -> Result<Expr, ParseError> {
        let expr = self.parse_or()?;

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

    fn parse_or(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_and()?;
        while let Some(t) = self.advance_on_match(&[TokenType::Or]) {
            let line = t.line.clone();
            let right = self.parse_and()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: LogicalOperator {
                    tp: LogicalOperatorType::Or,
                    line,
                },
                right: Box::new(right),
            };
        }
        Ok(expr)
    }

    fn parse_and(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_equality()?;
        while let Some(t) = self.advance_on_match(&[TokenType::And]) {
            let line = t.line.clone();
            let right = self.parse_equality()?;
            expr = Expr::Logical {
                left: Box::new(expr),
                operator: LogicalOperator {
                    tp: LogicalOperatorType::And,
                    line,
                },
                right: Box::new(right),
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
        self.parse_call()
    }

    fn parse_call(&mut self) -> Result<Expr, ParseError> {
        let mut expr = self.parse_primary()?;

        loop {
            if let Some(token) = self.advance_on_match(&[TokenType::LeftParen]) {
                let line = token.line;
                let args = self.parse_call_args()?;
                // Consume the right paren.
                if self.advance_on_match(&[TokenType::RightParen]).is_none() {
                    return Err(ParseError::MissingRightParen { line });
                }
                expr = Expr::Call {
                    callee: Box::new(expr),
                    arguments: args,
                };
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn parse_call_args(&mut self) -> Result<Vec<Expr>, ParseError> {
        let mut args: Vec<Expr> = Vec::new();
        if self.check_current_token_type(&[TokenType::RightParen]) {
            return Ok(args);
        }
        loop {
            let arg = self.parse_expression()?;
            args.push(arg);
            if self.advance_on_match(&[TokenType::Comma]).is_none() {
                break;
            }
        }
        if args.len() > 255 {
            return Err(ParseError::TooManyArguments {
                line: self.peek().unwrap().line,
            });
        }
        Ok(args)
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
        if self.advance_on_match(&[TokenType::LeftParen]).is_some() {
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

    #[test]
    fn block_stmt() {
        let input = "{ var x = 3; 3 + 4; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        let inner_stmts = match &stmts[0] {
            Stmt::Block(stmts) => stmts,
            _ => panic!("the only top-level statement in the code should be a block"),
        };
        assert_eq!(inner_stmts.len(), 2);
        assert!(matches!(inner_stmts[0], Stmt::Var { .. }));
        assert!(matches!(inner_stmts[1], Stmt::Expression(_)));
    }

    #[test]
    fn if_stmt() {
        let input = "if (x == 4) { var y = 5; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert_eq!(format!("{}", condition), "(== x 4)");
                assert!(else_branch.is_none());
                match then_branch.as_ref() {
                    Stmt::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                        assert!(matches!(stmts[0], Stmt::Var { .. }));
                    }
                    _ => panic!("the then branch should be a block"),
                }
            }
            _ => panic!("the only top-level statement in the code should be a block"),
        }
    }

    #[test]
    fn if_else_stmt() {
        let input = "if (x == 4) { var y = 5; } else { print 3; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                assert_eq!(format!("{}", condition), "(== x 4)");
                match then_branch.as_ref() {
                    Stmt::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                        assert!(matches!(stmts[0], Stmt::Var { .. }));
                    }
                    _ => panic!("the then branch should be a block"),
                }
                match else_branch.as_ref() {
                    Some(branch_box) => match branch_box.as_ref() {
                        Stmt::Block(stmts) => {
                            assert_eq!(stmts.len(), 1);
                            assert!(matches!(stmts[0], Stmt::Print(_)));
                        }
                        _ => panic!("the else branch should be a block"),
                    },
                    _ => panic!("the else branch should be a block"),
                }
            }
            _ => panic!("the only top-level statement in the code should be a block"),
        }
    }

    #[test]
    // Make sure elses are bound to the nearest `if` that precedes them when it's ambiguous.
    fn dangling_else_associativity() {
        let input = "if (x == 4) if (y == 5) print 3; else print 4;";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::If { else_branch, .. } => {
                assert!(matches!(else_branch, None), "The outer else should be None");
            }
            _ => panic!("the only top-level statement in the code should be an if statement"),
        }
    }

    #[test]
    fn test_logical_simple() {
        let input = "3 == 4 and 4 > 6";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(and (== 3 4) (> 4 6))");

        let input = "3 == 4 or 4 > 6";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "(or (== 3 4) (> 4 6))");
    }

    #[test]
    fn test_logical_associativity() {
        let input = "3 == 4 and 4 > 6 or 5 < 6 and true or false";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(
            expr_str,
            "(or (or (and (== 3 4) (> 4 6)) (and (< 5 6) true)) false)"
        )
    }

    #[test]
    fn test_while() {
        let input = "while (x < 5) { print x; x = x + 1; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::While { condition, body } => {
                assert_eq!(format!("{}", condition), "(< x 5)");
                match body.as_ref() {
                    Stmt::Block(stmts) => {
                        assert_eq!(stmts.len(), 2);
                        assert!(matches!(stmts[0], Stmt::Print(_)));
                        assert!(matches!(stmts[1], Stmt::Expression(_)));
                    }
                    _ => panic!("the body should be a block"),
                }
            }
            _ => panic!("the only top-level statement in the code should be a while statement"),
        }
    }

    #[test]
    fn test_for_with_all_parts() {
        let input = "for (var i = 0; i < 5; i = i + 1) { print i; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        // For loop top-level statements are blocks if there's an initializer, otherwise they're just while loops.
        let inner_block = match &stmts[0] {
            Stmt::Block(stmts) => stmts,
            _ => panic!("the only top-level statement in the code should be a block"),
        };
        // The block should have two statements: the var declaration and the while loop.
        assert_eq!(inner_block.len(), 2);
        assert!(matches!(inner_block[0], Stmt::Var { .. }));
        assert!(matches!(inner_block[1], Stmt::While { .. }));
    }

    #[test]
    fn test_minimal_for() {
        let input = "for (;;) { print 3; }";
        let tokens = scan(input.to_string()).unwrap();
        let stmts = parse(tokens).unwrap();
        assert_eq!(stmts.len(), 1);
        match &stmts[0] {
            Stmt::While { condition, body } => {
                assert_eq!(format!("{}", condition), "true");
                match body.as_ref() {
                    Stmt::Block(stmts) => {
                        assert_eq!(stmts.len(), 1);
                        assert!(matches!(stmts[0], Stmt::Print(_)));
                    }
                    _ => panic!("the body should be a block"),
                }
            }
            _ => panic!("the only top-level statement in the code should be a while statement"),
        }
    }

    #[test]
    fn call() {
        let input = "abc(3, 4, 5)";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "abc(3, 4, 5)");

        // An empty call.
        let input = "abc()";
        let tokens = scan(input.to_string()).unwrap();
        let expr = Parser::new(tokens).parse_expression().unwrap();
        let expr_str = format!("{}", expr);
        assert_eq!(expr_str, "abc()");
    }

    #[test]
    fn error_too_many_args() {
        let args = (0..=256).map(|_| "1").collect::<Vec<&str>>().join(", ");
        let func_call_input = format!("abc({});", args);
        let tokens = scan(func_call_input).unwrap();
        let error = Parser::new(tokens).parse_expression().unwrap_err();
        assert_eq!(error, ParseError::TooManyArguments { line: 1 });
    }
}
