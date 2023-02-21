use std::{borrow::Borrow, rc::Rc};

use crate::expression::Expression;

pub type Environment = std::collections::HashMap<String, Expression>;

#[derive(Debug, Eq, PartialEq)]
pub enum EvalError {
    NotANumber,
    UnknownSymbol,
    UnexpectedFunction,
    InvalidForm,
}
fn eval_to_float_list(env: &mut Environment, args: &[Expression]) -> Result<Vec<f64>, EvalError> {
    args.iter()
        .map(|x| eval(env, x))
        .map(|x| match x {
            Ok(Expression::Number(x)) => Ok(x),
            _ => Err(EvalError::NotANumber),
        })
        .collect()
}

fn add_mult_helper(
    env: &mut Environment,
    args: &[Expression],
    init: f64,
    f: fn(f64, f64) -> f64,
) -> Result<Expression, EvalError> {
    Ok(Expression::Number(
        eval_to_float_list(env, args)?.into_iter().fold(init, f),
    ))
}

fn sub_div_helper(
    env: &mut Environment,
    args: &[Expression],
    init: f64,
    f: fn(f64, &f64) -> f64,
) -> Result<Expression, EvalError> {
    let values = eval_to_float_list(env, args)?;
    match values.len() {
        0 | 1 => Ok(Expression::Number(values.iter().fold(init, f))),
        _ => {
            let (first, rest) = values.split_first().unwrap();
            Ok(Expression::Number(rest.iter().fold(*first, f)))
        }
    }
}

fn comp_helper(
    env: &mut Environment,
    args: &[Expression],
    f: fn(&[f64]) -> bool,
) -> Result<Expression, EvalError> {
    Ok(Expression::Boolean(
        eval_to_float_list(env, args)?.windows(2).all(f),
    ))
}

pub fn default_env() -> Environment {
    let mut env = std::collections::HashMap::new();

    env.insert(
        "+".to_owned(),
        Expression::Function(|env, args| add_mult_helper(env, args, 0.0, |x, y| x + y)),
    );
    env.insert(
        "*".to_owned(),
        Expression::Function(|env, args| add_mult_helper(env, args, 1.0, |x, y| x * y)),
    );
    env.insert(
        "-".to_owned(),
        Expression::Function(|env, args| sub_div_helper(env, args, 0.0, |x, y| x - y)),
    );
    env.insert(
        "/".to_owned(),
        Expression::Function(|env, args| sub_div_helper(env, args, 1.0, |x, y| x / y)),
    );
    env.insert(
        "<".to_owned(),
        Expression::Function(|env, args| comp_helper(env, args, |x| x[0] < x[1])),
    );
    env.insert(
        "<=".to_owned(),
        Expression::Function(|env, args| comp_helper(env, args, |x| x[0] <= x[1])),
    );
    env.insert(
        ">".to_owned(),
        Expression::Function(|env, args| comp_helper(env, args, |x| x[0] > x[1])),
    );
    env.insert(
        ">=".to_owned(),
        Expression::Function(|env, args| comp_helper(env, args, |x| x[0] >= x[1])),
    );
    env.insert(
        "=".to_owned(),
        Expression::Function(|env, args| comp_helper(env, args, |x| x[0] == x[1])),
    );
    env.insert(
        "if".to_owned(),
        Expression::Function(|env, args| {
            match &args
                .iter()
                .map(|x| eval(env, x))
                .collect::<Result<Vec<_>, EvalError>>()?[..]
            {
                [Expression::Boolean(b), x, y] => {
                    if *b {
                        Ok(x.clone())
                    } else {
                        Ok(y.clone())
                    }
                }
                _ => Err(EvalError::InvalidForm),
            }
        }),
    );
    env.insert(
        "def".to_owned(),
        Expression::Function(|env, args| match args {
            [Expression::Symbol(s), x] => {
                let x = eval(env, x)?;
                env.insert(s.clone(), x.clone());
                Ok(x)
            }
            _ => Err(EvalError::InvalidForm),
        }),
    );
    env.insert(
        "quote".to_owned(),
        Expression::Function(|_, args| match args {
            [x] => Ok(x.clone()),
            _ => Err(EvalError::InvalidForm),
        }),
    );
    env.insert(
        "lambda".to_owned(),
        Expression::Function(|_, args| match args {
            [x, y] => Ok(Expression::Lambda(Rc::new(x.clone()), Rc::new(y.clone()))),
            _ => Err(EvalError::InvalidForm),
        }),
    );
    env
}

pub fn eval(env: &mut Environment, expr: &Expression) -> Result<Expression, EvalError> {
    match expr {
        Expression::Boolean(_) => Ok(expr.clone()),
        Expression::Number(_) => Ok(expr.clone()),
        Expression::Symbol(x) => env
            .get(x)
            .ok_or(EvalError::UnknownSymbol)
            .map(|x| x.clone()),
        Expression::List(xs) => {
            let (first, rest) = xs.split_first().ok_or(EvalError::InvalidForm)?;
            let first_eval = eval(env, first)?;
            match first_eval {
                Expression::Function(f) => f(env, rest),
                Expression::Lambda(args, f) => match args.borrow() {
                    Expression::List(args) if args.len() == rest.len() => {
                        let mut temp_env = env.clone();
                        for (arg, value) in args.iter().zip(rest.iter()) {
                            if let Expression::Symbol(arg) = arg {
                                temp_env.insert(arg.clone(), value.clone());
                            } else {
                                return Err(EvalError::InvalidForm);
                            }
                        }
                        eval(&mut temp_env, &f)
                    }
                    _ => Err(EvalError::InvalidForm),
                },
                _ => Err(EvalError::InvalidForm),
            }
        }
        Expression::Function(_) => Err(EvalError::UnexpectedFunction),
        Expression::Lambda(_, _) => Err(EvalError::UnexpectedFunction),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse::{parse, tokenize};

    #[test]
    fn eval_boolean() {
        assert_eq!(
            eval(&mut default_env(), &parse(&tokenize("true")).unwrap().0),
            Ok(Expression::Boolean(true))
        );
    }

    #[test]
    fn eval_number() {
        assert_eq!(
            eval(&mut default_env(), &parse(&tokenize("42")).unwrap().0),
            Ok(Expression::Number(42.0))
        );
    }

    #[test]
    fn eval_undefined_symbol() {
        assert_eq!(
            eval(&mut default_env(), &parse(&tokenize("asdf")).unwrap().0),
            Err(EvalError::UnknownSymbol)
        );
    }

    #[test]
    fn eval_list() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(+ 1 2 3)")).unwrap().0
            ),
            Ok(Expression::Number(6.0))
        );
    }

    #[test]
    fn eval_empty_list() {
        assert_eq!(
            eval(&mut default_env(), &parse(&tokenize("()")).unwrap().0),
            Err(EvalError::InvalidForm)
        );
    }

    #[test]
    fn eval_not_a_function_list() {
        let mut env = default_env();
        eval(&mut env, &parse(&tokenize("(def a 42)")).unwrap().0).unwrap();
        assert_eq!(
            eval(&mut env, &parse(&tokenize("(a 1 2 3)")).unwrap().0),
            Err(EvalError::InvalidForm)
        );
    }

    #[test]
    fn eval_def() {
        let mut env = default_env();
        eval(&mut env, &parse(&tokenize("(def a 42)")).unwrap().0).unwrap();
        assert_eq!(
            eval(&mut env, &parse(&tokenize("a")).unwrap().0),
            Ok(Expression::Number(42.0))
        );
    }

    #[test]
    fn eval_if() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(if true 1 2)")).unwrap().0
            ),
            Ok(Expression::Number(1.0))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(if false 1 2)")).unwrap().0
            ),
            Ok(Expression::Number(2.0))
        );
    }

    #[test]
    fn eval_arithmetic() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(+ 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Number(9.0))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(- 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Number(-5.0))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(* 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Number(24.0))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(/ 12 2 3)")).unwrap().0
            ),
            Ok(Expression::Number(2.0))
        );
    }

    #[test]
    fn eval_comparisons() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(> 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(> 3 2 1)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(> 3 3 3)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );

        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(>= 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(>= 3 2 1)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(>= 3 3 3)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );

        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(< 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(< 3 2 1)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(< 3 3 3)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );

        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(<= 2 3 4)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(<= 3 2 1)")).unwrap().0
            ),
            Ok(Expression::Boolean(false))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(<= 3 3 3)")).unwrap().0
            ),
            Ok(Expression::Boolean(true))
        );
    }

    #[test]
    fn eval_quote() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(quote 8)")).unwrap().0
            ),
            Ok(Expression::Number(8.0))
        );
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("(quote (1 2))")).unwrap().0
            ),
            Ok(Expression::List(vec![
                Expression::Number(1.0),
                Expression::Number(2.0)
            ]))
        );
    }

    #[test]
    fn eval_lambda() {
        assert_eq!(
            eval(
                &mut default_env(),
                &parse(&tokenize("((lambda (a b) (+ a b)) 2 3)")).unwrap().0
            ),
            Ok(Expression::Number(5.0))
        );
    }
}
