//! RakuAST model emitted by the 6.e `Formatter.AST` API.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::Value;

fn node(class: RakuAstClass, fields: Vec<RakuAstField>) -> Value {
    Value::rakuast(Box::new(RakuAstNode { class, fields }))
}

fn positional(value: Value) -> RakuAstField {
    RakuAstField {
        name: None,
        value: RakuAstFieldValue::Node(value),
    }
}

fn named(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}

/// Build the user-facing AST returned by `Formatter.AST`.
///
/// Rakudo emits a directive-specialised pointy block. The model layer keeps the
/// same observable contract with an equivalent general block:
/// `-> *@args { sprintf($format, @args) }`. It is a genuine, lowerable
/// RakuAST tree rather than a marker node, so callers can inspect or `EVAL` it.
pub fn formatter_ast(format: &str) -> Value {
    let name = |identifier: &str| {
        node(
            RakuAstClass::Name,
            vec![positional(Value::str(identifier.to_string()))],
        )
    };
    let variable = |variable_name: &str| {
        node(
            RakuAstClass::VarLexical,
            vec![positional(Value::str(variable_name.to_string()))],
        )
    };

    let target = node(
        RakuAstClass::ParameterTargetVar,
        vec![named("name", Value::str("@args".to_string()))],
    );
    let slurpy = node(RakuAstClass::ParameterSlurpyFlattened, vec![]);
    let parameter = node(
        RakuAstClass::Parameter,
        vec![named("target", target), named("slurpy", slurpy)],
    );
    let signature = node(
        RakuAstClass::Signature,
        vec![RakuAstField {
            name: Some("parameters"),
            value: RakuAstFieldValue::List(vec![parameter]),
        }],
    );

    let call = node(
        RakuAstClass::CallName,
        vec![
            named("name", name("sprintf")),
            named(
                "args",
                node(
                    RakuAstClass::ArgList,
                    vec![
                        positional(node(
                            RakuAstClass::StrLiteral,
                            vec![positional(Value::str(format.to_string()))],
                        )),
                        positional(variable("@args")),
                    ],
                ),
            ),
        ],
    );
    let statement = node(
        RakuAstClass::StatementExpression,
        vec![named("expression", call)],
    );
    let statements = node(RakuAstClass::StatementList, vec![positional(statement)]);
    let body = node(RakuAstClass::Blockoid, vec![positional(statements)]);
    node(
        RakuAstClass::PointyBlock,
        vec![named("signature", signature), named("body", body)],
    )
}
