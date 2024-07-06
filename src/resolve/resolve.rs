use crate::ast::{Expr, Stmt, VariableReference};
use crate::resolve::ResolveError;
use log::{debug, info};
use std::collections::HashMap;

#[derive(Debug, Clone)]
pub enum FunctionType {
    None,
    Function,
}

#[derive(Debug, Clone)]
pub struct LocalResolutionMap {
    // A mapping of variable to their stack offset.
    pub depths: HashMap<VariableReference, usize>,
}

impl LocalResolutionMap {
    fn new() -> Self {
        LocalResolutionMap {
            depths: HashMap::new(),
        }
    }
}

/// Resolve all the non-global variables in the given statements.
/// Returns a mapping of variables to their stack offset.
pub fn resolve(stmts: &Vec<Stmt>) -> Result<LocalResolutionMap, ResolveError> {
    let mut resolver = Resolver {
        scopes: vec![HashMap::new()],
        resolutions: LocalResolutionMap::new(),
        current_function: FunctionType::None,
    };
    resolver.resolve_stmts(stmts)?;
    info!("Resolved all variables: {:?}", resolver.resolutions);
    Ok(resolver.resolutions)
}

pub struct Resolver {
    // A mapping of existing variable names in each scope, to a boolean representing whether the variable has been initialized to a value yet.
    scopes: Vec<HashMap<String, bool>>,
    // The local variable stack.
    resolutions: LocalResolutionMap,
    // Whether we're in a function.
    current_function: FunctionType,
}

impl Resolver {
    fn resolve_stmts(&mut self, stmts: &Vec<Stmt>) -> Result<(), ResolveError> {
        for stmt in stmts {
            self.resolve_stmt(stmt)?;
        }
        Ok(())
    }

    fn resolve_stmt(&mut self, stmt: &Stmt) -> Result<(), ResolveError> {
        match stmt {
            Stmt::Block(stmts) => {
                self.begin_scope();
                self.resolve_stmts(stmts)?;
                self.end_scope()?;
            }
            Stmt::Class { name, methods } => {
                self.declare(name)?;
                self.define(name);
            }
            Stmt::Var { name, initializer } => {
                self.declare(name)?;
                if let Some(expr) = initializer {
                    self.resolve_expr(expr)?;
                }
                self.define(name);
            }
            Stmt::Function { name, params, body } => {
                self.declare(name)?;
                self.define(name);
                self.resolve_function(FunctionType::Function, params, body)?;
            }
            Stmt::Expression(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::If {
                condition,
                then_branch,
                else_branch,
            } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(then_branch)?;
                if let Some(else_branch) = else_branch {
                    self.resolve_stmt(else_branch)?;
                }
            }
            Stmt::Print(expr) => {
                self.resolve_expr(expr)?;
            }
            Stmt::Return(value) => {
                if let FunctionType::None = self.current_function {
                    return Err(ResolveError::InvalidReturn);
                }
                if let Some(value) = value {
                    self.resolve_expr(value)?;
                }
            }
            Stmt::While { condition, body } => {
                self.resolve_expr(condition)?;
                self.resolve_stmt(body)?;
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        function_type: FunctionType,
        params: &Vec<String>,
        body: &Vec<Stmt>,
    ) -> Result<(), ResolveError> {
        // Update the current function type.
        let enclosing_function = self.current_function.clone();
        self.current_function = function_type;
        // Resolve the function body in a new scope.
        self.begin_scope();
        for param in params {
            self.declare(&param)?;
            self.define(&param);
        }
        self.resolve_stmts(body)?;
        self.end_scope()?;
        // Restore the previous function type.
        self.current_function = enclosing_function;
        Ok(())
    }

    fn declare(&mut self, name: &str) -> Result<(), ResolveError> {
        // If we have no scopes active, then this must be a global variable and we'll deal with it elsewhere.
        if let Some(current_scope) = self.scopes.last_mut() {
            // Set value to false to indicate that the variable hasn't been initialized.
            if current_scope.insert(name.to_string(), false).is_some() {
                // Error if the variable was already declared.
                return Err(ResolveError::Redeclaration(name.to_string()));
            }
        }
        Ok(())
    }

    fn define(&mut self, name: &str) {
        // If we have no scopes active, then this must be a global variable and we'll deal with it elsewhere.
        if let Some(current_scope) = self.scopes.last_mut() {
            // Set value to true to indicate that the variable has been initialized.
            current_scope.insert(name.to_string(), true);
        }
    }

    fn begin_scope(&mut self) -> () {
        self.scopes.push(HashMap::new());
    }

    fn end_scope(&mut self) -> Result<(), ResolveError> {
        if self.scopes.pop().is_none() {
            return Err(ResolveError::InvalidScopeOperation(
                "Attempted to end a scope when no scopes were active".to_string(),
            ));
        }
        Ok(())
    }

    fn resolve_expr(&mut self, expr: &Expr) -> Result<(), ResolveError> {
        match expr {
            Expr::Variable(reference) => {
                if self
                    .scopes
                    .last()
                    .is_some_and(|scope| scope.get(&reference.name) == Some(&false))
                {
                    return Err(ResolveError::ReadBeforeInitialize(reference.clone()));
                }
                self.resolve_reference(reference.clone());
            }
            Expr::Assignment { reference, value } => {
                self.resolve_expr(value)?;
                self.resolve_reference(reference.clone());
            }
            Expr::Binary { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Call { callee, args, .. } => {
                self.resolve_expr(callee)?;
                for arg in args {
                    self.resolve_expr(arg)?;
                }
            }
            Expr::Grouping { expression } => {
                self.resolve_expr(expression)?;
            }
            Expr::Literal { .. } => {}
            Expr::Logical { left, right, .. } => {
                self.resolve_expr(left)?;
                self.resolve_expr(right)?;
            }
            Expr::Unary { right, .. } => {
                self.resolve_expr(right)?;
            }
        }
        Ok(())
    }

    fn resolve_reference(&mut self, reference: VariableReference) {
        // Search for this variable starting at the innermost scope and working our way upwards.
        for (distance, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&reference.name) {
                let reference = VariableReference {
                    name: reference.name,
                    id: reference.id,
                };
                debug!(
                    "Resolved variable reference: {:?} referring to declaration {} levels away",
                    reference, distance
                );
                self.resolutions.depths.insert(reference, distance);
                return;
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::parse;
    use crate::scan::scan;

    fn parse_string(input: &str) -> Vec<Stmt> {
        let tokens = scan(input.to_string()).unwrap();
        parse(tokens).unwrap()
    }

    #[test]
    fn counts_levels_up_correctly() {
        let input = "var y = 0; { print y; { { print y; } } } ";
        let stmts = parse_string(input);
        let resolutions = resolve(&stmts).unwrap();
        assert_eq!(resolutions.depths.len(), 2);
        // The first y is 1 level below its declaration and the second is 3 levels below.
        let depths = resolutions.depths.values().collect::<Vec<&usize>>();
        assert!(depths.contains(&&1));
        assert!(depths.contains(&&3));
    }

    #[test]
    fn errros_on_circular_initializer() {
        let input = "var x = 4; { var x = x + 3; }";
        let stmts = parse_string(input);
        let err = resolve(&stmts).unwrap_err();
        match err {
            ResolveError::ReadBeforeInitialize(VariableReference { name, .. }) => {
                assert_eq!(name, "x")
            }
            _ => panic!("Expected ReadVarBeforeInitialize error"),
        }
    }

    #[test]
    fn errors_on_redeclaration_of_locals() {
        let input = "fun bad() {
            var a = 1;
            var a = 2;
        }";
        let stmts = parse_string(input);
        let err = resolve(&stmts).unwrap_err();
        match err {
            ResolveError::Redeclaration(_) => {}
            _ => panic!("Expected Redeclaration error"),
        }
    }

    #[test]
    fn errors_on_return_outside_of_function() {
        let input = "return 1;";
        let stmts = parse_string(input);
        let err = resolve(&stmts).unwrap_err();
        match err {
            ResolveError::InvalidReturn => {}
            _ => panic!("Expected InvalidReturn error"),
        }
    }
}
