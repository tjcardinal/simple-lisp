use crate::expression::Expression;

#[derive(Debug, Eq, PartialEq)]
pub enum ParseError {
    EmptyStringList,
    InvalidRightParen,
    MissingRightParen,
}

pub fn tokenize(input: &str) -> Vec<String> {
    input
        .replace('(', " ( ")
        .replace(')', " ) ")
        .split_whitespace()
        .map(|s| s.to_owned())
        .collect()
}

pub fn parse(input: &[String]) -> Result<(Expression, &[String]), ParseError> {
    let (first, rest) = input.split_first().ok_or(ParseError::EmptyStringList)?;
    match first.as_str() {
        ")" => Err(ParseError::InvalidRightParen),
        "(" => Ok(parse_list(rest)?),
        x => Ok((parse_atom(x), rest)),
    }
}

fn parse_list(input: &[String]) -> Result<(Expression, &[String]), ParseError> {
    let mut list = vec![];
    let mut xs = input;
    loop {
        let (first, rest) = xs.split_first().ok_or(ParseError::MissingRightParen)?;
        if first == ")" {
            return Ok((Expression::List(list), rest));
        }
        let (expr, rest) = parse(xs)?;
        list.push(expr);
        xs = rest;
    }
}

fn parse_atom(input: &str) -> Expression {
    match input {
        "true" => Expression::Boolean(true),
        "false" => Expression::Boolean(false),
        x => x
            .parse()
            .map_or(Expression::Symbol(input.to_owned()), Expression::Number),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tokenize_atoms() {
        assert_eq!(tokenize("42"), ["42"]);
        assert_eq!(tokenize("ab"), ["ab"]);
        assert_eq!(tokenize("42 ab"), ["42", "ab"]);
        assert_eq!(tokenize("  42  ab  "), ["42", "ab"]);
    }

    #[test]
    fn tokenize_lists() {
        assert_eq!(tokenize("()"), ["(", ")"]);
        assert_eq!(tokenize("(42 ab)"), ["(", "42", "ab", ")"]);
        assert_eq!(tokenize("(42 ab"), ["(", "42", "ab"]);
        assert_eq!(
            tokenize("(42 (cd ef) ab)"),
            ["(", "42", "(", "cd", "ef", ")", "ab", ")"]
        );
        assert_eq!(tokenize("  (  42  ab  )  "), ["(", "42", "ab", ")"]);
    }

    #[test]
    fn parse_bool() {
        assert_eq!(
            parse(&tokenize("true")),
            Ok((Expression::Boolean(true), &vec![][..]))
        );
    }

    #[test]
    fn parse_number() {
        assert_eq!(
            parse(&tokenize("42")),
            Ok((Expression::Number(42.0), &vec![][..]))
        );
    }

    #[test]
    fn parse_symbol() {
        assert_eq!(
            parse(&tokenize("ab")),
            Ok((Expression::Symbol("ab".to_owned()), &vec![][..]))
        );
    }

    #[test]
    fn parse_list() {
        assert_eq!(
            parse(&tokenize("(true 42 ab (1 2))")),
            Ok((
                Expression::List(vec![
                    Expression::Boolean(true),
                    Expression::Number(42.0),
                    Expression::Symbol("ab".to_owned()),
                    Expression::List(vec![Expression::Number(1.0), Expression::Number(2.0)])
                ]),
                &vec![][..]
            ))
        );
    }

    #[test]
    fn parse_empty_list() {
        assert_eq!(
            parse(&tokenize("()")),
            Ok((Expression::List(vec![]), &vec![][..]))
        );
    }

    #[test]
    fn parse_empty_string() {
        assert_eq!(parse(&tokenize("")), Err(ParseError::EmptyStringList));
    }

    #[test]
    fn parse_invalid_right_parens() {
        assert_eq!(parse(&tokenize(")")), Err(ParseError::InvalidRightParen));
    }

    #[test]
    fn parse_missing_right_parens() {
        assert_eq!(
            parse(&tokenize("(true 42 ab")),
            Err(ParseError::MissingRightParen)
        );
    }
}
