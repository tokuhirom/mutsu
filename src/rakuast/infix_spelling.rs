//! The Unicode spelling of an infix operator (ADR-10723 Stage 2).
//!
//! The parser reads `×`, `≤`, `…`, `∈`, … as the operator they alias and
//! keeps the ASCII name (`*`, `<=`, `...`, `(elem)`), which is all execution
//! needs. Rakudo 2026.09 keeps the written symbol in the tree
//! (`Infix.new("×")`), and `.DEPARSE` writes it back. Once an infix
//! application is placed (`infix_origins`), the text between its operands
//! says which spelling was written; when it is one of the operator's Unicode
//! aliases, the `Infix` takes it. Exact or absent: an operator whose text
//! does not read as an alias keeps its ASCII name.

use super::{RakuAstClass, RakuAstNode};

/// The Unicode spellings rakudo keeps for the infix the parser names `op`,
/// each measured on rakudo 2026.09.
// Cost: O(1).
pub(super) fn unicode_aliases(op: &str) -> &'static [&'static str] {
    match op {
        "*" => &["×"],
        "/" => &["÷"],
        "-" => &["−"],
        "<=" => &["≤"],
        ">=" => &["≥"],
        "!=" => &["≠"],
        "==" => &["⩵"],
        "===" => &["⩶"],
        "=~=" => &["≅"],
        "o" => &["∘"],
        "..." => &["…"],
        "...^" => &["…^"],
        "(elem)" => &["∈"],
        "(cont)" => &["∋"],
        "(<=)" => &["⊆", "≼"],
        "(>=)" => &["⊇", "≽"],
        "(<)" => &["⊂"],
        "(>)" => &["⊃"],
        "⚛-=" => &["⚛−="],
        // Not Unicode, but the same kind of alias: the loose `xor` is the
        // parser's `^^` placed at a looser precedence, and rakudo keeps
        // `Infix("xor")`.
        "^^" => &["xor"],
        _ => &[],
    }
}

/// The spelling of `op` that `text` starts with: `op` itself, or one of its
/// Unicode aliases.
// Cost: O(1).
pub(super) fn written_prefix<'a>(op: &'a str, text: &str) -> Option<&'a str> {
    if text.starts_with(op) {
        return Some(op);
    }
    unicode_aliases(op)
        .iter()
        .copied()
        .find(|alias| text.starts_with(alias))
}

/// The operator name an `Infix` written as `name` lowers to. The parser
/// dispatches `×`, `÷` and the set-relation symbols (`∈`, `⊆`, ...) by their
/// own names, so those stay as written (a user's `infix:<×>` keeps
/// overriding them); the other Unicode spellings have no operator of their
/// own in mutsu and lower to the ASCII operator they alias.
// Cost: O(1).
pub(super) fn lowered_name(name: &str) -> &str {
    match name {
        "−" => "-",
        "≤" => "<=",
        "≥" => ">=",
        "≠" => "!=",
        "⩵" => "==",
        "⩶" => "===",
        "≅" => "=~=",
        "∘" => "o",
        "…" => "...",
        "…^" => "...^",
        "≼" => "(<=)",
        "≽" => "(>=)",
        "⚛−=" => "⚛-=",
        "xor" => "^^",
        other => other,
    }
}

/// How an infix operator node is written: its text, and the range of its
/// inner `Infix` within that text (`R-` holds `-` at 1..2).
// Cost: O(d), d = nesting of metaoperators.
pub(super) fn written(infix: &RakuAstNode) -> Option<(String, Option<(usize, usize)>)> {
    // The operator inside: an `Infix`, or a meta-operator over one (`+=`
    // inside `R+=`, `[+]` inside `[+]=`).
    let inner = || {
        let op = super::infix_origins::child_at(infix, 0)?;
        leaf_text_of_infix(op).or_else(|| {
            matches!(
                op.class,
                RakuAstClass::MetaInfixAssign
                    | RakuAstClass::MetaInfixReverse
                    | RakuAstClass::MetaInfixNegate
                    | RakuAstClass::BracketedInfix
            )
            .then(|| written(op).map(|(text, _)| text))
            .flatten()
        })
    };
    Some(match infix.class {
        RakuAstClass::Infix | RakuAstClass::Mixin => {
            (super::infix_origins::leaf_text(infix)?, None)
        }
        RakuAstClass::Assignment => ("=".to_string(), None),
        RakuAstClass::MetaInfixAssign => {
            let op = inner()?;
            let len = op.len();
            (format!("{op}="), Some((0, len)))
        }
        // `[+]`: an operator in brackets.
        RakuAstClass::BracketedInfix => {
            let op = inner()?;
            let len = op.len();
            (format!("[{op}]"), Some((1, 1 + len)))
        }
        // `[&plus]`: a routine used as an operator, its variable inside.
        RakuAstClass::FunctionInfix => {
            let var = super::infix_origins::child_at(infix, 0)
                .and_then(super::infix_origins::leaf_text)?;
            let len = var.len();
            (format!("[{var}]"), Some((1, 1 + len)))
        }
        RakuAstClass::MetaInfixReverse | RakuAstClass::MetaInfixNegate => {
            let op = inner()?;
            let prefix = if infix.class == RakuAstClass::MetaInfixReverse {
                "R"
            } else {
                "!"
            };
            let len = op.len();
            (format!("{prefix}{op}"), Some((1, 1 + len)))
        }
        _ => return None,
    })
}

// Cost: O(f), f = fields of `node`.
fn leaf_text_of_infix(node: &RakuAstNode) -> Option<String> {
    (node.class == RakuAstClass::Infix)
        .then(|| super::infix_origins::leaf_text(node))
        .flatten()
}

/// The operator inside `infix`, a meta-operator written at byte `at`
/// (`+=` inside `R+=`), and the operators inside that, each where its
/// spelling puts it.
// Cost: O(d), d = nesting depth of the meta-operators.
pub(super) fn place_inner(infix: &mut RakuAstNode, at: usize) {
    let Some((_, Some((from, to)))) = written(infix) else {
        return;
    };
    super::infix_origins::with_child_at(infix, 0, |op| {
        super::origin::set_span(op, crate::ast::SrcSpan::new(at + from, at + to));
        place_inner(op, at + from);
    });
}
