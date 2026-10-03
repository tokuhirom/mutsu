//! `RakuAST::Regex::CharClass::*`: the backslash classes, `.` and the
//! codepoint escapes, in both directions (`crate::regex_tree::CharClassAtom`).
//!
//! Measured on rakudo 2026.09: every class but `Any` and `Nul` does
//! `RakuAST::Regex::CharClass::Negatable` and takes `:negated`; `Specified`
//! also takes `:characters`, the characters the escape denoted.

use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::regex_tree::{BackslashClass, CharClassAtom};
use crate::value::{RuntimeError, Value, ValueView};

/// Which `RakuAST::Regex::CharClass::*` class a node is.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegexCharClassKind {
    Digit,
    Word,
    Space,
    Newline,
    HorizontalSpace,
    VerticalSpace,
    Tab,
    Escape,
    FormFeed,
    CarriageReturn,
    Nul,
    Any,
    Specified,
}

use RegexCharClassKind as K;

/// Every kind with its class name and its backslash class.
const KINDS: [(RegexCharClassKind, &str, Option<BackslashClass>); 13] = [
    (
        K::Digit,
        "RakuAST::Regex::CharClass::Digit",
        Some(BackslashClass::Digit),
    ),
    (
        K::Word,
        "RakuAST::Regex::CharClass::Word",
        Some(BackslashClass::Word),
    ),
    (
        K::Space,
        "RakuAST::Regex::CharClass::Space",
        Some(BackslashClass::Space),
    ),
    (
        K::Newline,
        "RakuAST::Regex::CharClass::Newline",
        Some(BackslashClass::Newline),
    ),
    (
        K::HorizontalSpace,
        "RakuAST::Regex::CharClass::HorizontalSpace",
        Some(BackslashClass::HorizontalSpace),
    ),
    (
        K::VerticalSpace,
        "RakuAST::Regex::CharClass::VerticalSpace",
        Some(BackslashClass::VerticalSpace),
    ),
    (
        K::Tab,
        "RakuAST::Regex::CharClass::Tab",
        Some(BackslashClass::Tab),
    ),
    (
        K::Escape,
        "RakuAST::Regex::CharClass::Escape",
        Some(BackslashClass::Escape),
    ),
    (
        K::FormFeed,
        "RakuAST::Regex::CharClass::FormFeed",
        Some(BackslashClass::FormFeed),
    ),
    (
        K::CarriageReturn,
        "RakuAST::Regex::CharClass::CarriageReturn",
        Some(BackslashClass::CarriageReturn),
    ),
    (
        K::Nul,
        "RakuAST::Regex::CharClass::Nul",
        Some(BackslashClass::Nul),
    ),
    (K::Any, "RakuAST::Regex::CharClass::Any", None),
    (K::Specified, "RakuAST::Regex::CharClass::Specified", None),
];

impl RegexCharClassKind {
    // Cost: O(1).
    pub(super) fn printed_name(self) -> &'static str {
        KINDS
            .iter()
            .find_map(|&(kind, name, _)| (kind == self).then_some(name))
            .expect("every kind has a name")
    }

    /// Whether the class does `RakuAST::Regex::CharClass::Negatable`.
    // Cost: O(1).
    pub(super) fn negatable(self) -> bool {
        !matches!(self, K::Any | K::Nul)
    }

    /// The model fields the class declares (rakudo's `.^attributes` order).
    // Cost: O(1).
    pub(super) fn model_fields(self) -> &'static [(&'static str, super::fields::Absent)] {
        use super::fields::Absent;
        match self {
            K::Specified => &[("negated", Absent::False), ("characters", Absent::Required)],
            K::Any | K::Nul => &[],
            _ => &[("negated", Absent::False)],
        }
    }

    // Cost: O(1).
    fn from_backslash(class: BackslashClass) -> Self {
        KINDS
            .iter()
            .find_map(|&(kind, _, backslash)| (backslash == Some(class)).then_some(kind))
            .expect("every backslash class has a kind")
    }
}

/// The ancestors of a char-class node, nearest first.
// Cost: O(1).
pub(super) fn ancestors(kind: RegexCharClassKind) -> &'static [&'static str] {
    if kind.negatable() {
        &[
            "RakuAST::Regex::CharClass::Negatable",
            "RakuAST::Regex::CharClass",
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ]
    } else {
        &[
            "RakuAST::Regex::CharClass",
            "RakuAST::Regex::Atom",
            "RakuAST::Regex::Term",
            "RakuAST::Regex",
        ]
    }
}

fn truth_field(name: &'static str) -> RakuAstField {
    RakuAstField {
        name: Some(name),
        value: RakuAstFieldValue::Node(Value::truth(true)),
    }
}

/// The node for a tree atom.
// Cost: O(n), n = number of specified characters.
pub(super) fn convert(atom: &CharClassAtom) -> RakuAstNode {
    let (kind, negated, characters) = match atom {
        CharClassAtom::Backslash { class, negated } => {
            (RegexCharClassKind::from_backslash(*class), *negated, None)
        }
        CharClassAtom::Any => (K::Any, false, None),
        CharClassAtom::Specified {
            characters,
            negated,
        } => (K::Specified, *negated, Some(characters)),
    };
    let mut fields = Vec::new();
    if negated {
        fields.push(truth_field("negated"));
    }
    if let Some(characters) = characters {
        fields.push(RakuAstField {
            name: Some("characters"),
            value: RakuAstFieldValue::Node(Value::str(characters.clone())),
        });
    }
    RakuAstNode {
        class: RakuAstClass::RegexCharClass(kind),
        fields,
    }
}

fn negated(node: &RakuAstNode) -> bool {
    node.fields.iter().any(|f| {
        f.name == Some("negated") && matches!(&f.value, RakuAstFieldValue::Node(v) if v.truthy())
    })
}

/// The tree atom for a node, or `None` when the node is malformed.
// Cost: O(n), n = number of specified characters.
pub(super) fn lower(kind: RegexCharClassKind, node: &RakuAstNode) -> Option<CharClassAtom> {
    let negated = negated(node);
    match kind {
        K::Any => Some(CharClassAtom::Any),
        K::Specified => {
            let characters = node.fields.iter().find_map(|f| match (&f.name, &f.value) {
                (Some("characters"), RakuAstFieldValue::Node(v)) => match v.view() {
                    ValueView::Str(s) => Some(s.to_string()),
                    _ => None,
                },
                _ => None,
            })?;
            // A negated escape denotes one character (`\X41`).
            (!characters.is_empty() && (!negated || characters.chars().count() == 1)).then_some(
                CharClassAtom::Specified {
                    characters,
                    negated,
                },
            )
        }
        _ => {
            let class = KINDS
                .iter()
                .find_map(|&(k, _, backslash)| (k == kind).then_some(backslash))
                .flatten()?;
            Some(CharClassAtom::Backslash { class, negated })
        }
    }
}

/// `RakuAST::Regex::CharClass::<Kind>.new(:negated, :characters)`, or `None`
/// when `class_name` is not a char class.
// Cost: O(a), a = number of arguments.
pub(super) fn construct(
    class_name: &str,
    method: &str,
    args: &[Value],
) -> Option<Result<Value, RuntimeError>> {
    if method != "new" {
        return None;
    }
    let kind = KINDS
        .iter()
        .find_map(|&(kind, name, _)| (name == class_name).then_some(kind))?;
    let mut fields = Vec::new();
    for arg in args {
        let (key, value) = match arg.view() {
            ValueView::Pair(k, v) => (k.as_str().to_string(), v.clone()),
            ValueView::ValuePair(k, v) => (k.to_string_value(), v.clone()),
            _ => {
                return Some(Err(RuntimeError::new(format!(
                    "{class_name}.new takes only named arguments"
                ))));
            }
        };
        match key.as_str() {
            "negated" if kind.negatable() => {
                if value.truthy() {
                    fields.push(truth_field("negated"));
                }
            }
            "characters" if kind == K::Specified => fields.push(RakuAstField {
                name: Some("characters"),
                value: RakuAstFieldValue::Node(Value::str(value.to_string_value())),
            }),
            other => {
                return Some(Err(RuntimeError::new(format!(
                    "{class_name}.new does not accept `{other}`"
                ))));
            }
        }
    }
    // Rakudo's order: `negated` before `characters`.
    fields.sort_by_key(|f| f.name != Some("negated"));
    if kind == K::Specified && !fields.iter().any(|f| f.name == Some("characters")) {
        return Some(Err(RuntimeError::new(format!(
            "{class_name}.new requires `characters`"
        ))));
    }
    Some(Ok(Value::rakuast(Box::new(RakuAstNode {
        class: RakuAstClass::RegexCharClass(kind),
        fields,
    }))))
}
