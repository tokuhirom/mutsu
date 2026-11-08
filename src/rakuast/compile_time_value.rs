//! `has-compile-time-value`, as rakudo 2026.09 answers it (`src/Raku/ast/`): a
//! node does the `RakuAST::CompileTimeValue` role (always `True`), or computes
//! the answer from its children, or inherits `Node`'s `False`. The set of role
//! classes and the computed ones are measured with `.^lookup('has-compile-time-value')`
//! over the node classes mutsu knows.
//!
//! The computed answers keep rakudo's types: a list that is not exactly one
//! statement long answers the `Int` `0`, not `False`.

use super::{RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::value::{Value, ValueView};

/// Classes that do the `CompileTimeValue` role.
const ROLE: &[&str] = &[
    "RakuAST::Block",
    "RakuAST::Class",
    "RakuAST::ColonPair::False",
    "RakuAST::ColonPair::Number",
    "RakuAST::ColonPair::True",
    "RakuAST::ComplexLiteral",
    "RakuAST::Constant",
    "RakuAST::FakeSignature",
    "RakuAST::Grammar",
    "RakuAST::IntLiteral",
    "RakuAST::Label",
    "RakuAST::Literal",
    "RakuAST::Method",
    "RakuAST::Module",
    "RakuAST::Native",
    "RakuAST::NumLiteral",
    "RakuAST::Package",
    "RakuAST::Parameter",
    "RakuAST::PointyBlock",
    "RakuAST::QuotedRegex",
    "RakuAST::RatLiteral",
    "RakuAST::Regex::Assertion::Named::RegexArg",
    "RakuAST::Regex::CapturingGroup",
    "RakuAST::RegexDeclaration",
    "RakuAST::Role",
    "RakuAST::RoleBody",
    "RakuAST::RuleDeclaration",
    "RakuAST::Signature",
    "RakuAST::StatementPrefix::Gather",
    "RakuAST::StatementPrefix::Once",
    "RakuAST::StatementPrefix::Phaser::Begin",
    "RakuAST::StatementPrefix::Phaser::Check",
    "RakuAST::StatementPrefix::Phaser::Close",
    "RakuAST::StatementPrefix::Phaser::End",
    "RakuAST::StatementPrefix::Phaser::Enter",
    "RakuAST::StatementPrefix::Phaser::First",
    "RakuAST::StatementPrefix::Phaser::Init",
    "RakuAST::StatementPrefix::Phaser::Keep",
    "RakuAST::StatementPrefix::Phaser::Last",
    "RakuAST::StatementPrefix::Phaser::Leave",
    "RakuAST::StatementPrefix::Phaser::Next",
    "RakuAST::StatementPrefix::Phaser::Post",
    "RakuAST::StatementPrefix::Phaser::Pre",
    "RakuAST::StatementPrefix::Phaser::Undo",
    "RakuAST::StatementPrefix::React",
    "RakuAST::StatementPrefix::Start",
    "RakuAST::StatementPrefix::Supply",
    "RakuAST::StatementPrefix::Try",
    "RakuAST::StrLiteral",
    "RakuAST::Sub",
    "RakuAST::Submethod",
    "RakuAST::Substitution",
    "RakuAST::TokenDeclaration",
    "RakuAST::Type::AnyDefinedness",
    "RakuAST::Type::Capture",
    "RakuAST::Type::Coercion",
    "RakuAST::Type::Definedness",
    "RakuAST::Type::Enum",
    "RakuAST::Type::Parameterized",
    "RakuAST::Type::Setting",
    "RakuAST::Type::Simple",
    "RakuAST::Type::Subset",
    "RakuAST::VarDeclaration::Anonymous",
    "RakuAST::VarDeclaration::Constant",
    "RakuAST::VarDeclaration::Simple",
    "RakuAST::VersionLiteral",
];

/// The child nodes of `node` that are its statements or segments (the hidden
/// origin fields excluded).
fn children(node: &RakuAstNode) -> Vec<Value> {
    let mut out = Vec::new();
    for field in &node.fields {
        if super::origin::is_origin(field) {
            continue;
        }
        collect(field, &mut out);
    }
    out
}

fn collect(field: &RakuAstField, out: &mut Vec<Value>) {
    match &field.value {
        RakuAstFieldValue::Node(v) => out.push(v.clone()),
        RakuAstFieldValue::List(items) => out.extend(items.iter().cloned()),
        RakuAstFieldValue::Adverb(_) => {}
    }
}

fn named_node(node: &RakuAstNode, name: &str) -> Option<Value> {
    node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some(n), RakuAstFieldValue::Node(v)) if *n == name => Some(v.clone()),
        _ => None,
    })
}

fn of_value(value: &Value) -> Value {
    match value.view() {
        ValueView::RakuAst(node) => has_compile_time_value(node),
        _ => Value::truth(false),
    }
}

/// `has-compile-time-value` of `node`.
// Cost: O(n), n = size of the node's statement or segment children.
pub(crate) fn has_compile_time_value(node: &RakuAstNode) -> Value {
    let class = node.class.printed_name();
    if ROLE.contains(&class) {
        return Value::truth(true);
    }
    match class {
        // Every segment a plain string.
        "RakuAST::QuotedString" | "RakuAST::Heredoc" => {
            let segments = node
                .fields
                .iter()
                .find(|f| f.name == Some("segments"))
                .map(|f| {
                    let mut out = Vec::new();
                    collect(f, &mut out);
                    out
                })
                .unwrap_or_default();
            Value::truth(segments.iter().all(|s| {
                matches!(s.view(), ValueView::RakuAst(n) if n.class.printed_name() == "RakuAST::StrLiteral")
            }))
        }
        // Exactly one statement, whose answer it is; any other count is `0`.
        "RakuAST::StatementList" | "RakuAST::SemiList" | "RakuAST::StatementSequence" => {
            match children(node).as_slice() {
                [only] => of_value(only),
                _ => Value::int(0),
            }
        }
        "RakuAST::Statement::Expression" => {
            if node
                .fields
                .iter()
                .any(|f| matches!(f.name, Some("condition-modifier" | "loop-modifier")))
            {
                return Value::int(0);
            }
            named_node(node, "expression").map_or_else(|| Value::truth(false), |e| of_value(&e))
        }
        "RakuAST::Circumfix::Parentheses" => children(node)
            .first()
            .map_or_else(|| Value::int(0), of_value),
        "RakuAST::ColonPair::Value" => {
            named_node(node, "value").map_or_else(|| Value::truth(false), |v| of_value(&v))
        }
        _ => Value::truth(false),
    }
}
