//! The questions rakudo 2026.09's `RakuAST::Name` answers about its parts
//! (`src/Raku/ast/name.rakumod`): `is-identifier`, `is-simple`,
//! `is-multi-part`, `is-global-lookup`, `is-package-lookup`,
//! `is-pseudo-package`, `is-installable`, `has-colonpairs`, and the
//! `first-part` / `last-part` / `root-part` accessors, and the colonpair
//! queries `has-colonpair` / `first-colonpair` / `without-colonpair`. Measured
//! on rakudo 2026.11, where every predicate answers a Bool (#18976).

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
        ValueView::Package(name)
            if name.with_str(|n| {
                matches!(
                    n,
                    "RakuAST::Name::Part::Empty" | "RakuAST::Name::Part::EmptyEdge"
                )
            }) =>
        {
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
    let flag = |b: bool| Some(Value::truth(b));
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
        // `::Foo`, `::($x)`: a name that starts with `::` searches packages.
        "is-package-search" => flag(parts.first().is_some_and(is_empty_edge)),
        "has-colonpairs" => Some(Value::truth(
            node_accessor(node, "colonpairs")
                .and_then(|v| v.as_list_items().map(|items| !items.is_empty()))
                .unwrap_or(false),
        )),
        // The identifier of a one-part simple name, else the empty string.
        "simple-identifier" => match kinds.as_slice() {
            [Part::Simple(name)] => Some(Value::str(name.clone())),
            _ => Some(Value::str(String::new())),
        },
        // `Foo::` is `Foo`: the trailing `::` of a package lookup left off,
        // the colonpairs kept.
        "base-name" => {
            let mut base = parts;
            if base.len() > 1 && matches!(kinds.last(), Some(Part::Empty)) {
                base.pop();
            }
            Some(with_colonpairs(base, colonpairs(node)))
        }
        // A name over the same parts, the colonpairs left off.
        "without-colonpairs" => Some(name_value(parts)),
        "without-first-part" => Some(name_value(parts.get(1..).unwrap_or_default().to_vec())),
        "first-part" | "root-part" => parts.first().cloned(),
        "last-part" => parts.last().cloned(),
        _ => None,
    }
}

/// `canonicalize` of a name with an indirect part (`::($x)::Foo` is
/// `($x)::Foo`): a leading `::` left off, each `Part::Expression` as its
/// source text in parentheses, and a trailing `::` kept. `None` for a name
/// without an expression part, or one whose expression was not parsed from
/// source (#18976).
// Cost: O(p + t), p = parts, t = length of the expressions' source.
pub(crate) fn indirect_canonical(node: &RakuAstNode) -> Option<String> {
    let parts = parts(node);
    let has_expression = parts.iter().any(|p| {
        matches!(p.view(), ValueView::RakuAst(n) if n.class == RakuAstClass::NamePartExpression)
    });
    if !has_expression {
        return None;
    }
    let mut spelled = Vec::with_capacity(parts.len());
    for (i, part) in parts.iter().enumerate() {
        if i == 0 && is_empty_edge(part) {
            continue;
        }
        spelled.push(match classify(part) {
            Part::Simple(name) => name,
            Part::Empty => String::new(),
            Part::Other => {
                let ValueView::RakuAst(n) = part.view() else {
                    return None;
                };
                let expr = super::infix_origins::child_at(n, 0)?;
                format!("({})", origin_text(expr)?)
            }
        });
    }
    Some(spelled.join("::"))
}

/// The source text `node`'s origin covers.
// Cost: O(t), t = end offset of the origin.
fn origin_text(node: &RakuAstNode) -> Option<String> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let map = attributes.as_map();
    let (ValueView::Int(from), ValueView::Int(to)) = (map.get("from")?.view(), map.get("to")?.view())
    else {
        return None;
    };
    let ValueView::Instance { attributes, .. } = map.get("source")?.view() else {
        return None;
    };
    let orig = attributes
        .as_map()
        .get(super::unit_source::ORIG_ATTR)?
        .to_string_value();
    Some(
        orig.chars()
            .skip(from.max(0) as usize)
            .take((to - from).max(0) as usize)
            .collect(),
    )
}

/// Whether `part` is the `EmptyEdge` a leading or trailing `::` makes.
// Cost: O(1).
fn is_empty_edge(part: &Value) -> bool {
    match part.view() {
        ValueView::RakuAst(n) => n.class == RakuAstClass::NamePartEmptyEdge,
        ValueView::Package(name) => name.with_str(|n| n == "RakuAST::Name::Part::EmptyEdge"),
        _ => false,
    }
}

/// The colonpairs of the `Name` `node`.
// Cost: O(c), c = colonpairs.
fn colonpairs(node: &RakuAstNode) -> Vec<Value> {
    node_accessor(node, "colonpairs")
        .and_then(|v| v.as_list_items().map(<[Value]>::to_vec))
        .unwrap_or_default()
}

/// The key of the colonpair `pair` (`ver` of `:ver<1.1>`).
// Cost: O(f), f = fields of the pair.
fn pair_key(pair: &Value) -> Option<String> {
    let ValueView::RakuAst(node) = pair.view() else {
        return None;
    };
    node_accessor(node, "key").and_then(|v| v.as_str().map(str::to_string))
}

/// `has-colonpair($key)` / `first-colonpair($key)` / `without-colonpair($key)`
/// on the `Name` `node`, `None` for any other method.
// Cost: O(c + p), c = colonpairs, p = parts.
pub(crate) fn call_with_key(node: &RakuAstNode, method: &str, key: &str) -> Option<Value> {
    let pairs = colonpairs(node);
    let matches = |pair: &Value| pair_key(pair).as_deref() == Some(key);
    match method {
        "has-colonpair" => Some(Value::truth(pairs.iter().any(matches))),
        "first-colonpair" => Some(pairs.into_iter().find(matches).unwrap_or(Value::NIL)),
        "without-colonpair" => {
            let kept = pairs.into_iter().filter(|p| !matches(p)).collect();
            Some(with_colonpairs(parts(node), kept))
        }
        _ => None,
    }
}

/// A name over `parts` with the colonpairs `pairs`.
// Cost: O(p + c).
fn with_colonpairs(parts: Vec<Value>, pairs: Vec<Value>) -> Value {
    let mut name = super::name_parts::name_from_parts(parts);
    if !pairs.is_empty() {
        name.fields.push(super::RakuAstField {
            name: Some("colonpairs"),
            value: super::RakuAstFieldValue::List(pairs),
        });
    }
    Value::rakuast(Box::new(name))
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
    if matches!(own.first().map(classify), Some(Part::Simple(n)) if matches!(n.as_str(), "GLOBAL"))
    {
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
