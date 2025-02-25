use super::typed::*;

pub trait ForEachExpression {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, f: F) -> F;
}

impl ForEachExpression for Expression {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, mut f: F) -> F {
        match self {
            Expression::Binary { lhs, rhs, .. } => {
                f = lhs.for_each_expression(f);
                f = rhs.for_each_expression(f);
            },
            Expression::Block { statements, last_expr, .. } => {
                for stmt in statements {
                    f = stmt.for_each_expression(f);
                }
                if let Some(last_expr) = last_expr {
                    f = last_expr.for_each_expression(f);
                }
            },
            Expression::Call { callee, arguments, .. } => {
                f = callee.for_each_expression(f);
                for arg in arguments {
                    f = arg.for_each_expression(f);
                }
            },
            Expression::Literal(_) => {},
            Expression::Unary { operand, .. } => {
                f = operand.for_each_expression(f);
            },
            Expression::Variable { .. } => {},
        }

        f(self);
        f
    }
}

impl ForEachExpression for ExprRef {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, f: F) -> F {
        self.get_mut().for_each_expression(f)
    }
}

impl ForEachExpression for Statement {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, f: F) -> F {
        match self {
            Statement::Assert { expr, .. } => expr.for_each_expression(f),
            Statement::Decl(decl) => decl.for_each_expression(f),
            Statement::Expr { expr, .. } => expr.for_each_expression(f),
            Statement::Print { expr, .. } => expr.for_each_expression(f),
        }
    }
}

impl ForEachExpression for Declaration {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, f: F) -> F {
        match self {
            Declaration::Forward { .. } => f,
            Declaration::Function { body, .. } => {
                body.for_each_expression(f)
            },
            Declaration::Parameter { .. } => f,
            Declaration::TypeParameter { .. } => f,
            Declaration::Variable { initializer, .. } => {
                initializer.for_each_expression(f)
            },
        }
    }
}

impl ForEachExpression for DeclRef {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, f: F) -> F {
        self.borrow_mut().for_each_expression(f)
    }
}

impl ForEachExpression for Module {
    fn for_each_expression<F: FnMut(&mut Expression)>(&mut self, mut f: F) -> F {
        for stmt in &mut self.statements {
            f = stmt.for_each_expression(f)
        }
        f
    }
}
