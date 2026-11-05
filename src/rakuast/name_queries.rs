//! The questions rakudo 2026.09's `RakuAST::Name` answers about its parts
//! (`src/Raku/ast/name.rakumod`): `is-identifier`, `is-simple`,
//! `is-multi-part`, `is-global-lookup`, `is-package-lookup`,
//! `is-pseudo-package`, `is-installable`, `has-colonpairs`, and the
//! `first-part` / `last-part` / `root-part` accessors. Measured on rakudo, as
//! its Int-or-Bool answer types are part of the contract (nqp comparisons give
//! `0` / `1`, `is-simple` and `has-colonpairs` give a Bool).

use super::{RakuAstClass, RakuAstNode, node_accessor};
use crate::value::{RuntimeError, Value, ValueView};

/// The pseudo-package names rakudo's `Name::Part::Simple.is-pseudo-package`
/// accepts (`GLOBAL`, `PROCESS` and `PARENT` are not among them).
const PSEUDO_PACKAGES: &[&str] = &[
    "MY",
    "OUR",
    "OUTER",
    "OUTERS",
    "CORE",
    "UNIT",
    "SETTING",
    "DYNAMIC",
    "CALLER",
    "CALLERS",
    "COMPILING",
    "LEXICAL",
    "CLIENT",
];

/// What kind of part a name part is.
enum Part {
    Simple(String),
    Empty,
    Other,
}

// Cost: O(f), f = fields of the part.
fn classify(part: &Value) -> Part {
    match part.view() {
        ValueView::RakuAst(n) => match n.class {
            RakuAstClass::NamePartSimple => node_accessor(n, "name")
                .and_then(|v| v.as_str().map(str::to_string))
                .map_or(Part::Other, Part::Simple),
            RakuAstClass::NamePartEmpty | RakuAstClass::NamePartEmptyEdge => Part::Empty,
            _ => Part::Other,
        },
        ValueView::Package(name) if name.with_str(|n| n == "RakuAST::Name::Part::Empty") => {
            Part::Empty
        }
        _ => Part::Other,
    }
}

// Cost: O(p), p = parts of the name.
fn parts(node: &RakuAstNode) -> Vec<Value> {
    node_accessor(node, "parts")
        .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default()
}

/// The answer to `method` on the `Name` `node`, `None` for a method this
/// module does not answer.
// Cost: O(p), p = parts of the name.
pub(crate) fn call(node: &RakuAstNode, method: &str) -> Option<Value> {
    let parts = parts(node);
    let kinds: Vec<Part> = parts.iter().map(classify).collect();
    let all_empty = !kinds.is_empty() && kinds.iter().all(|k| matches!(k, Part::Empty));
    let flag = |b: bool| Some(Value::int(i64::from(b)));
    let first_simple = |wanted: &dyn Fn(&str) -> bool| matches!(kinds.first(), Some(Part::Simple(name)) if wanted(name));
    match method {
        "is-identifier" => flag(kinds.len() == 1 && matches!(kinds[0], Part::Simple(_))),
        "is-simple" => Some(Value::truth(
            kinds.iter().all(|k| matches!(k, Part::Simple(_))),
        )),
        "is-multi-part" => flag(kinds.len() > 1 && !all_empty),
        "is-global-lookup" => flag(first_simple(&|n| matches!(n, "GLOBAL"))),
        "is-package-lookup" => flag(matches!(kinds.last(), Some(Part::Empty))),
        "is-pseudo-package" => flag(all_empty || first_simple(&|n| PSEUDO_PACKAGES.contains(&n))),
        "is-installable" => flag(!all_empty),
        "has-colonpairs" => Some(Value::truth(
            node_accessor(node, "colonpairs")
                .and_then(|v| v.as_list_items().map(|items| !items.is_empty()))
                .unwrap_or(false),
        )),
        // The identifier of a one-part simple name, else the Int `0`.
        "simple-identifier" => match kinds.as_slice() {
            [Part::Simple(name)] if kinds.len() == 1 => Some(Value::str(name.clone())),
            _ => Some(Value::int(0)),
        },
        // A name over the same parts, the colonpairs left off.
        "base-name" | "without-colonpairs" => Some(name_value(parts)),
        "without-first-part" => Some(name_value(parts.get(1..).unwrap_or_default().to_vec())),
        "first-part" | "root-part" => parts.first().cloned(),
        "last-part" => parts.last().cloned(),
        _ => None,
    }
}

// Cost: O(p), p = parts.
fn name_value(parts: Vec<Value>) -> Value {
    Value::rakuast(Box::new(super::name_parts::name_from_parts(parts)))
}

/// `qualified-with($target)`: a name over the parts of `$target` followed by
/// this name's own, except that a name that starts with `GLOBAL` is already
/// absolute and answers its parts without that first one.
// Cost: O(p + q), p, q = parts of this name and of the target.
pub(crate) fn qualified_with(node: &RakuAstNode, target: &Value) -> Result<Value, RuntimeError> {
    let ValueView::RakuAst(target_node) = target.view() else {
        return Err(not_a_name(target));
    };
    if target_node.class != RakuAstClass::Name {
        return Err(not_a_name(target));
    }
    let own = parts(node);
    if matches!(own.first().map(classify), Some(Part::Simple(n)) if matches!(n.as_str(), "GLOBAL")) {
        return Ok(name_value(own[1..].to_vec()));
    }
    let mut all = parts(target_node);
    all.extend(own);
    Ok(name_value(all))
}

fn not_a_name(target: &Value) -> RuntimeError {
    RuntimeError::new(format!(
        "X::TypeCheck::Binding::Parameter: Type check failed in binding to parameter \
         '$target'; expected RakuAST::Name but got {}",
        crate::value::type_name::value_type_name(target)
    ))
}
