use nom::IResult;
use nom::bytes::complete::tag;
use nom::character::complete::char;
use nom::combinator::opt;
use nom::multi::{many0, separated_list1};
use nom::sequence::preceded;

use crate::ast::Stmt;

use super::expr::expression;
use super::helpers::ws;

/// Parse a `say` statement: `say <expr>, <expr>, ...;`
fn say_stmt(input: &str) -> IResult<&str, Stmt> {
    let (input, _) = tag("say")(input)?;
    let (input, _) = ws(input)?;
    let (input, args) = separated_list1(preceded(ws, preceded(char(','), ws)), expression)(input)?;
    let (input, _) = ws(input)?;
    let (input, _) = opt(char(';'))(input)?;
    Ok((input, Stmt::Say(args)))
}

/// Parse a single statement.
fn statement(input: &str) -> IResult<&str, Stmt> {
    let (input, _) = ws(input)?;
    say_stmt(input)
}

/// Parse a full program (sequence of statements).
pub(super) fn program(input: &str) -> IResult<&str, Vec<Stmt>> {
    let (input, stmts) = many0(statement)(input)?;
    let (input, _) = ws(input)?;
    Ok((input, stmts))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::Value;

    #[test]
    fn parse_say_integer() {
        let result = program("say 42;").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1.len(), 1);
        match &result.1[0] {
            Stmt::Say(args) => {
                assert_eq!(args.len(), 1);
                match &args[0] {
                    crate::ast::Expr::Literal(Value::Int(n)) => assert_eq!(*n, 42),
                    other => panic!("Expected Int literal, got {:?}", other),
                }
            }
            other => panic!("Expected Say stmt, got {:?}", other),
        }
    }

    #[test]
    fn parse_say_string() {
        let result = program("say 'hello';").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1.len(), 1);
        match &result.1[0] {
            Stmt::Say(args) => match &args[0] {
                crate::ast::Expr::Literal(Value::Str(s)) => assert_eq!(s, "hello"),
                other => panic!("Expected Str literal, got {:?}", other),
            },
            other => panic!("Expected Say stmt, got {:?}", other),
        }
    }

    #[test]
    fn parse_multiple_say() {
        let result = program("say 1; say 2;").unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1.len(), 2);
    }
}
