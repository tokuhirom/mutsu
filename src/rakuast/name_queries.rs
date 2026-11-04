//! The questions rakudo 2026.09's `RakuAST::Name` answers about its parts
//! (`src/Raku/ast/name.rakumod`): `is-identifier`, `is-simple`,
//! `is-multi-part`, `is-global-lookup`, `is-package-lookup`,
//! `is-pseudo-package`, `is-installable`, `has-colonpairs`, and the
//! `first-part` / `last-part` / `root-part` accessors. Measured on rakudo, as
//! its Int-or-Bool answer types are part of the contract (nqp comparisons give
//! `0` / `1`, `is-simple` and `has-colonpairs` give a Bool).

use super::{RakuAstClass, RakuAstNode, node_accessor};
use crate::value::{Value, ValueView};

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
        "is-global-lookup" => flag(first_simple(&|n| n == "GLOBAL")),
        "is-package-lookup" => flag(matches!(kinds.last(), Some(Part::Empty))),
        "is-pseudo-package" => flag(all_empty || first_simple(&|n| PSEUDO_PACKAGES.contains(&n))),
        "is-installable" => flag(!all_empty),
        "has-colonpairs" => Some(Value::truth(
            node_accessor(node, "colonpairs")
                .and_then(|v| v.as_list_items().map(|items| !items.is_empty()))
                .unwrap_or(false),
        )),
        "first-part" | "root-part" => parts.first().cloned(),
        "last-part" => parts.last().cloned(),
        _ => None,
    }
}
