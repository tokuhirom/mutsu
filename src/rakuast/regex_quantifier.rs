//! `RakuAST::Regex::QuantifiedAtom` and `RakuAST::Regex::Quantifier::*`, in
//! both directions (`crate::regex_tree::RegexQuantifier`).
//!
//! Measured on rakudo 2026.09: a quantifier carries its backtracking modifier
//! as a type object (`backtrack => RakuAST::Regex::Backtrack::Frugal`); a
//! `Range` keeps only the bounds and exclusions that were written; the
//! quantified atom holds the `separator` and a `trailing-separator` flag.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::regex_tree::{QuantifierKind, RegexBacktrack, RegexNode, RegexQuantifier};
use crate::value::{RuntimeError, Value, ValueView};

const BACKTRACKS: [(RegexBacktrack, RakuAstClass); 3] = [
    (RegexBacktrack::Frugal, RakuAstClass::RegexBacktrackFrugal),
    (RegexBacktrack::Greedy, RakuAstClass::RegexBacktrackGreedy),
    (RegexBacktrack::Ratchet, RakuAstClass::RegexBacktrackRatchet),
];

fn field(name: &'static str, value: Value) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(value),
    }
}

/// The `QuantifiedAtom` node for `atom` under `quantifier`. `separator` is
/// the already converted separator node.
// Cost: O(1).
pub(super) fn convert(
    atom: RakuAstNode,
    quantifier: &RegexQuantifier,
    separator: Option<RakuAstNode>,
) -> RakuAstNode {
    let mut fields = vec![
        field("atom", Value::rakuast(Box::new(atom))),
        field(
            "quantifier",
            Value::rakuast(Box::new(quantifier_node(quantifier))),
        ),
    ];
    if let Some(separator) = separator {
        fields.push(field("separator", Value::rakuast(Box::new(separator))));
        if quantifier.separator.as_ref().is_some_and(|s| s.trailing) {
            fields.push(field("trailing-separator", Value::truth(true)));
        }
    }
    RakuAstNode {
        class: RakuAstClass::RegexQuantifiedAtom,
        fields,
    }
}

// Cost: O(1).
fn quantifier_node(quantifier: &RegexQuantifier) -> RakuAstNode {
    let mut fields = Vec::new();
    let class = match quantifier.kind {
        QuantifierKind::ZeroOrMore => RakuAstClass::RegexQuantifierZeroOrMore,
        QuantifierKind::OneOrMore => RakuAstClass::RegexQuantifierOneOrMore,
        QuantifierKind::ZeroOrOne => RakuAstClass::RegexQuantifierZeroOrOne,
        QuantifierKind::Range {
            min,
            max,
            excludes_min,
            excludes_max,
        } => {
            if let Some(min) = min {
                fields.push(field("min", Value::int(min as i64)));
            }
            if excludes_min {
                fields.push(field("excludes-min", Value::truth(true)));
            }
            if let Some(max) = max {
                fields.push(field("max", Value::int(max as i64)));
            }
            if excludes_max {
                fields.push(field("excludes-max", Value::truth(true)));
            }
            RakuAstClass::RegexQuantifierRange
        }
    };
    if let Some(backtrack) = quantifier.backtrack {
        fields.push(field("backtrack", backtrack_value(backtrack)));
    }
    RakuAstNode { class, fields }
}

/// The `Backtrack::*` type object for a modifier.
// Cost: O(1).
fn backtrack_value(backtrack: RegexBacktrack) -> Value {
    super::slurpy_marker_value(match backtrack {
        RegexBacktrack::Frugal => RakuAstClass::RegexBacktrackFrugal,
        RegexBacktrack::Greedy => RakuAstClass::RegexBacktrackGreedy,
        RegexBacktrack::Ratchet => RakuAstClass::RegexBacktrackRatchet,
    })
}

fn field_value<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a Value> {
    node.fields.iter().find_map(|f| match (&f.name, &f.value) {
        (Some(n), RakuAstFieldValue::Node(v)) if *n == name => Some(v),
        _ => None,
    })
}

fn flag(node: &RakuAstNode, name: &str) -> bool {
    field_value(node, name).is_some_and(Value::truthy)
}

fn count(node: &RakuAstNode, name: &str) -> Result<Option<u64>, ()> {
    match field_value(node, name) {
        None => Ok(None),
        Some(value) => match value.view() {
            ValueView::Package(_) => Ok(None),
            ValueView::Int(n) if n >= 0 => Ok(Some(n as u64)),
            _ => Err(()),
        },
    }
}

/// The backtracking modifier a field names, by type object or by node.
// Cost: O(1).
fn backtrack(value: &Value) -> Option<RegexBacktrack> {
    let class = match value.view() {
        ValueView::RakuAst(node) => node.class,
        ValueView::Package(name) => super::class_from_name(&name.resolve())?,
        _ => return None,
    };
    BACKTRACKS
        .iter()
        .find_map(|&(b, c)| (c == class).then_some(b))
}

/// The tree quantifier for a quantifier node, without its separator.
// Cost: O(1).
pub(super) fn lower_quantifier(node: &RakuAstNode) -> Option<RegexQuantifier> {
    let kind = match node.class {
        RakuAstClass::RegexQuantifierZeroOrMore => QuantifierKind::ZeroOrMore,
        RakuAstClass::RegexQuantifierOneOrMore => QuantifierKind::OneOrMore,
        RakuAstClass::RegexQuantifierZeroOrOne => QuantifierKind::ZeroOrOne,
        RakuAstClass::RegexQuantifierRange => {
            let min = count(node, "min").ok()?;
            let max = count(node, "max").ok()?;
            let (excludes_min, excludes_max) =
                (flag(node, "excludes-min"), flag(node, "excludes-max"));
            // `**^3` has no min; any other open-bottom range has no spelling.
            if min.is_none() && (max.is_none() || !excludes_max || excludes_min) {
                return None;
            }
            QuantifierKind::Range {
                min,
                max,
                excludes_min,
                excludes_max,
            }
        }
        _ => return None,
    };
    let backtrack = match field_value(node, "backtrack") {
        None => None,
        Some(value) if matches!(value.view(), ValueView::Package(name) if name.resolve() == "RakuAST::Regex::Backtrack") => {
            None
        }
        Some(value) => Some(backtrack(value)?),
    };
    Some(RegexQuantifier {
        kind,
        backtrack,
        separator: None,
    })
}

/// Whether the separator of a `QuantifiedAtom` node is trailing (`%%`).
// Cost: O(1).
pub(super) fn trailing_separator(node: &RakuAstNode) -> bool {
    flag(node, "trailing-separator")
}

fn is_quantifier(value: &Value) -> bool {
    matches!(value.view(), ValueView::RakuAst(node) if matches!(
        node.class,
        RakuAstClass::RegexQuantifierZeroOrMore
            | RakuAstClass::RegexQuantifierOneOrMore
            | RakuAstClass::RegexQuantifierZeroOrOne
            | RakuAstClass::RegexQuantifierRange
    ))
}

fn named(args: &[Value]) -> Result<Vec<(String, Value)>, RuntimeError> {
    args.iter()
        .map(|arg| match arg.view() {
            ValueView::Pair(k, v) => Ok((k.as_str().to_string(), v.clone())),
            ValueView::ValuePair(k, v) => Ok((k.to_string_value(), v.clone())),
            _ => Err(RuntimeError::new(
                "RakuAST::Regex quantifier constructors take only named arguments",
            )),
        })
        .collect()
}

/// The `.new` constructors of the quantifier classes and of
/// `QuantifiedAtom`, or `None` when `class_name` is none of them.
// Cost: O(a), a = number of arguments.
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if method != "new" {
        return None;
    }
    let class = match class_name {
        "RakuAST::Regex::Quantifier::ZeroOrMore" => RakuAstClass::RegexQuantifierZeroOrMore,
        "RakuAST::Regex::Quantifier::OneOrMore" => RakuAstClass::RegexQuantifierOneOrMore,
        "RakuAST::Regex::Quantifier::ZeroOrOne" => RakuAstClass::RegexQuantifierZeroOrOne,
        "RakuAST::Regex::Quantifier::Range" => RakuAstClass::RegexQuantifierRange,
        "RakuAST::Regex::QuantifiedAtom" => RakuAstClass::RegexQuantifiedAtom,
        _ => return None,
    };
    Some(construct_class(class, class_name, args))
}

fn construct_class(
    class: RakuAstClass,
    class_name: &str,
    args: &[Value],
) -> Result<Value, RuntimeError> {
    let args = named(args)?;
    // Rakudo's field order, which the renderer follows.
    let order: &[&str] = match class {
        RakuAstClass::RegexQuantifiedAtom => {
            &["atom", "quantifier", "separator", "trailing-separator"]
        }
        RakuAstClass::RegexQuantifierRange => {
            &["min", "excludes-min", "max", "excludes-max", "backtrack"]
        }
        _ => &["backtrack"],
    };
    let mut fields = Vec::new();
    for &name in order {
        let Some((_, value)) = args.iter().find(|(key, _)| key == name) else {
            continue;
        };
        let valid = match name {
            "atom" | "separator" => super::require_regex_node(value, class_name).is_ok(),
            "quantifier" => is_quantifier(value),
            "backtrack" => backtrack(value).is_some(),
            "min" | "max" => matches!(value.view(), ValueView::Int(n) if n >= 0),
            _ => true,
        };
        if !valid {
            return Err(RuntimeError::new(format!(
                "{class_name}.new: `{name}` is not a valid value"
            )));
        }
        match name {
            // A false flag is the default the renderer elides.
            "trailing-separator" | "excludes-min" | "excludes-max" => {
                if value.truthy() {
                    fields.push(field(name, Value::truth(true)));
                }
            }
            "backtrack" => {
                if let Some(backtrack) = backtrack(value) {
                    fields.push(field(name, backtrack_value(backtrack)));
                }
            }
            _ => fields.push(field(name, value.clone())),
        }
    }
    if let Some((key, _)) = args.iter().find(|(key, _)| !order.contains(&key.as_str())) {
        return Err(RuntimeError::new(format!(
            "{class_name}.new does not accept `{key}`"
        )));
    }
    if class == RakuAstClass::RegexQuantifiedAtom {
        for required in ["atom", "quantifier"] {
            if !fields.iter().any(|f| f.name == Some(required)) {
                return Err(RuntimeError::new(format!(
                    "{class_name}.new requires `{required}`"
                )));
            }
        }
    }
    Ok(Value::rakuast(Box::new(RakuAstNode { class, fields })))
}

/// The tree node of a separator converted back, for the lowerer.
pub(super) type LoweredSeparator = Option<Box<crate::regex_tree::RegexSeparator>>;

/// Build the separator of a lowered `QuantifiedAtom` from its node.
// Cost: O(1).
pub(super) fn separator(node: RegexNode, trailing: bool) -> LoweredSeparator {
    Some(Box::new(crate::regex_tree::RegexSeparator {
        node,
        trailing,
    }))
}
