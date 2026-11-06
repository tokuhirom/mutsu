//! `properties`, `reducer-name` and `default-operator-properties` of the
//! operator nodes, as rakudo 2026.09 answers them (measured with the bare
//! `raku`; ADR-14916):
//!
//! - an `Infix`, `Prefix` or `Postfix` is looked up by its symbol in the
//!   built-in precedence table ([`crate::op_prec`]), falling back to its
//!   category's default; the postcircumfixes `[ ]` / `{ }` answer the
//!   method-call level; `DottyInfix::Call` has its own;
//! - a `FunctionInfix` answers the additive level (rakudo's default for an
//!   infix it knows nothing about);
//! - `MetaInfix::Assign` is item assignment, or list assignment when the
//!   inner infix binds looser than assignment; `Negate` and `Hyper` answer
//!   the inner infix's; `Reverse` flips `left` / `right`; `Cross` and `Zip` are
//!   the list-infix level;
//! - `reducer-name` is `&METAOP_REDUCE_` plus `LEFT`, `RIGHT`, `CHAIN`, `LIST`
//!   or `LISTINFIX`, chosen by the associativity (and the `list-infix` level).
//!
//! The answer is an `OperatorProperties` instance, a class declared by a small
//! prelude (`run_prelude_operator_properties.rs`).

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::op_prec::{self, PrecEntries};
use crate::value::{Value, ValueView};

/// One operator's properties, as the attributes of `OperatorProperties`.
#[derive(Clone, Default)]
struct Props {
    prec: String,
    sub: String,
    assoc: String,
    thunky: String,
    dba: String,
    nextterm: String,
    iffy: bool,
    diffy: bool,
    fiddly: bool,
}

impl Props {
    fn from_entries(entries: PrecEntries) -> Self {
        let mut p = Props::default();
        for (key, value) in entries {
            let text = value.to_string();
            match *key {
                "prec" => p.prec = text,
                "sub" => p.sub = text,
                "assoc" => p.assoc = text,
                "thunky" => p.thunky = text,
                "dba" => p.dba = text,
                "nextterm" => p.nextterm = text,
                "iffy" => p.iffy = true,
                "diffy" => p.diffy = true,
                "fiddly" => p.fiddly = true,
                _ => {}
            }
        }
        p
    }

    fn builtin(category: &str, symbol: &str) -> Self {
        Self::from_entries(
            op_prec::builtin(category, symbol)
                .or_else(|| op_prec::category_default(category))
                .unwrap_or(&[]),
        )
    }

    /// `&METAOP_REDUCE_...`, by associativity.
    fn reducer_name(&self) -> &'static str {
        match (self.assoc.as_str(), self.dba.as_str()) {
            ("right", _) => "&METAOP_REDUCE_RIGHT",
            ("chain", _) => "&METAOP_REDUCE_CHAIN",
            ("list", "list-infix") => "&METAOP_REDUCE_LISTINFIX",
            ("list", _) => "&METAOP_REDUCE_LIST",
            _ => "&METAOP_REDUCE_LEFT",
        }
    }

    fn into_value(self) -> Value {
        let mut attrs = std::collections::HashMap::new();
        let mut text = |name: &str, v: String| {
            attrs.insert(name.to_string(), Value::str(v));
        };
        text("precedence", self.prec);
        text("sub-precedence", self.sub);
        text("associative", self.assoc);
        text("thunky", self.thunky);
        text("dba", self.dba);
        text("next-term", self.nextterm);
        for (name, on) in [
            ("iffy", self.iffy),
            ("diffy", self.diffy),
            ("fiddly", self.fiddly),
            ("adverb", false),
            ("ternary", false),
        ] {
            attrs.insert(name.to_string(), Value::int(i64::from(on)));
        }
        Value::make_instance(crate::symbol::Symbol::intern("OperatorProperties"), attrs)
    }
}

fn symbol_text(node: &RakuAstNode) -> Option<String> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })
}

fn named_operator(node: &RakuAstNode) -> Option<String> {
    node.fields
        .iter()
        .find(|f| f.name == Some("operator"))
        .and_then(|f| match &f.value {
            RakuAstFieldValue::Node(v) => Some(v.to_string_value()),
            _ => None,
        })
}

/// The first child node of `node` (positional, or the named `infix`).
fn inner_infix(node: &RakuAstNode) -> Option<RakuAstNode> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name.is_none() || f.name == Some("infix") => {
            match v.view() {
                ValueView::RakuAst(inner) => Some(inner.clone()),
                _ => None,
            }
        }
        _ => None,
    })
}

/// Infix symbols whose `.prec` rakudo's routine table answers but whose
/// `OperatorProperties` entry it lacks (measured on 2026.09): they get the
/// default infix level.
const NO_OPERATOR_PROPERTIES: &[&str] = &[
    "\u{f7}", "\u{220a}", "\u{220d}", "\u{2260}", "\u{2264}", "\u{2265}", "\u{2a75}", "\u{2a76}",
];

/// Operators the grammar parses without a routine, so the routine table
/// ([`crate::op_prec`]) does not list them.
fn syntax_infix(symbol: &str) -> Option<PrecEntries> {
    const FLIP_FLOP: PrecEntries = &[
        ("assoc", "right"),
        ("dba", "conditional-ff"),
        ("iffy", "1"),
        ("prec", "j="),
        ("thunky", "tt"),
    ];
    const BIND: PrecEntries = &[
        ("assoc", "right"),
        ("dba", "list assignment"),
        ("fiddly", "1"),
        ("prec", "i="),
        ("sub", "e="),
    ];
    const DOTTY: PrecEntries = &[
        ("assoc", "left"),
        ("dba", "dotty-infix"),
        ("fiddly", "1"),
        ("nextterm", "dottyopish"),
        ("prec", "v="),
        ("sub", "z="),
    ];
    match symbol {
        "ff" | "ff^" | "^ff" | "^ff^" | "fff" | "fff^" | "^fff" | "^fff^" => Some(FLIP_FLOP),
        ":=" => Some(BIND),
        "." => Some(DOTTY),
        _ => None,
    }
}

/// `prefix:<//>` is a routine rakudo's `OperatorProperties` knows as symbolic
/// unary; `prefix:<\u{2212}>` (minus sign) is one it does not know.
fn prefix_props(symbol: &str) -> Props {
    match symbol {
        "//" => Props::builtin("prefix", "+"),
        "\u{2212}" => Props::from_entries(op_prec::category_default("prefix").unwrap_or(&[])),
        _ => Props::builtin("prefix", symbol),
    }
}

fn infix_symbol_props(symbol: &str) -> Props {
    if NO_OPERATOR_PROPERTIES.contains(&symbol) {
        return Props::from_entries(op_prec::category_default("infix").unwrap_or(&[]));
    }
    match syntax_infix(symbol) {
        Some(entries) => Props::from_entries(entries),
        None => Props::builtin("infix", symbol),
    }
}

fn infix_props(node: &RakuAstNode) -> Props {
    infix_symbol_props(&symbol_text(node).unwrap_or_default())
}

/// Subscript delimiters the grammar parses that the routine table does not
/// list; rakudo's class-level `circumfix` / `postcircumfix` lookups answer the
/// method-call level for them.
const SUBSCRIPT_SYNTAX: &[&str] = &["< >", "<< >>", "\u{ab} \u{bb}"];

/// `OperatorProperties.infix` / `prefix` / `postfix` / `circumfix` /
/// `postcircumfix`: the class-level lookup by symbol, the same answer an
/// operator node of that category gives (`None` for another category).
// Cost: O(log n), n = built-in operator count.
pub(crate) fn lookup(category: &str, symbol: &str) -> Option<Value> {
    let props = match category {
        "infix" => infix_symbol_props(symbol),
        "prefix" => prefix_props(symbol),
        "postfix" => Props::builtin("postfix", symbol),
        "circumfix" | "postcircumfix" if SUBSCRIPT_SYNTAX.contains(&symbol) => {
            Props::builtin("postcircumfix", "[ ]")
        }
        "circumfix" | "postcircumfix" => Props::builtin(category, symbol),
        _ => return None,
    };
    Some(props.into_value())
}

/// The properties of `node`, or `None` when it is not an operator node.
fn props(node: &RakuAstNode) -> Option<Props> {
    Some(match node.class {
        RakuAstClass::Infix => infix_props(node),
        RakuAstClass::Prefix => prefix_props(&symbol_text(node).unwrap_or_default()),
        RakuAstClass::Postfix => {
            Props::builtin("postfix", &named_operator(node).unwrap_or_default())
        }
        RakuAstClass::FunctionInfix => Props::builtin("infix", "+"),
        RakuAstClass::PostcircumfixArrayIndex
        | RakuAstClass::PostcircumfixHashIndex
        | RakuAstClass::PostcircumfixLiteralHashIndex => Props::builtin("postcircumfix", "[ ]"),
        RakuAstClass::DottyInfixCall => Props::from_entries(syntax_infix(".").unwrap_or(&[])),
        RakuAstClass::MetaInfixNegate | RakuAstClass::MetaInfixHyper => {
            inner_infix(node).map_or_else(|| infix_props(node), |i| props(&i).unwrap_or_default())
        }
        RakuAstClass::MetaInfixAssign => {
            let inner = inner_infix(node)
                .and_then(|i| props(&i))
                .unwrap_or_default();
            let looser = inner.prec.chars().next().is_some_and(|c| c < 'i');
            Props::builtin("infix", if looser { "=" } else { "=>" })
        }
        RakuAstClass::MetaInfixReverse => {
            let mut p = inner_infix(node)
                .and_then(|i| props(&i))
                .unwrap_or_default();
            p.assoc = match p.assoc.as_str() {
                "left" => "right".into(),
                "right" => "left".into(),
                other => other.into(),
            };
            p
        }
        RakuAstClass::MetaInfixCross | RakuAstClass::MetaInfixZip => Props::builtin("infix", "X"),
        _ => return None,
    })
}

/// `properties`.
// Cost: O(d log n), d = nesting of metaoperators, n = built-in operator count.
pub(crate) fn properties(node: &RakuAstNode) -> Option<Value> {
    props(node).map(Props::into_value)
}

/// `reducer-name`.
// Cost: O(d log n), d = nesting of metaoperators, n = built-in operator count.
pub(crate) fn reducer_name(node: &RakuAstNode) -> Option<Value> {
    // `Assign` answers the properties of item / list assignment but reduces
    // like the inner infix (`+=` folds left, `**=` folds right).
    let basis = if node.class == RakuAstClass::MetaInfixAssign {
        inner_infix(node).and_then(|i| props(&i))
    } else {
        props(node)
    };
    basis.map(|p| Value::str(p.reducer_name().to_string()))
}
