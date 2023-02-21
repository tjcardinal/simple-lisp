use std::rc::Rc;

use crate::eval::{Environment, EvalError};

#[derive(Clone)]
pub enum Expression {
    Boolean(bool),
    Number(f64),
    Symbol(String),
    List(Vec<Expression>),
    Function(fn(&mut Environment, &[Expression]) -> Result<Expression, EvalError>),
    Lambda(Rc<Expression>, Rc<Expression>),
}

impl std::fmt::Debug for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Boolean(x) => write!(f, "{:?}", x),
            Self::Number(x) => write!(f, "{:?}", x),
            Self::Symbol(x) => write!(f, "{:?}", x),
            Self::List(x) => write!(f, "{:?}", x),
            Self::Function(_) => write!(f, "Function"),
            Self::Lambda(x, y) => write!(f, "({:?}) ({:?})", x, y),
        }
    }
}

impl std::cmp::PartialEq for Expression {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Boolean(x), Self::Boolean(y)) => x == y,
            (Self::Number(x), Self::Number(y)) => x == y,
            (Self::Symbol(x), Self::Symbol(y)) => x == y,
            (Self::List(x), Self::List(y)) => x == y,
            (Self::Lambda(x1, x2), Self::Lambda(y1, y2)) => x1 == y1 && x2 == y2,
            (_, _) => false,
        }
    }
}
