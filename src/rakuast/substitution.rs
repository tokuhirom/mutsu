//! `s///`, `S///` and `tr///` across the RakuAST boundary.
//!
//! Measured against rakudo 2026.09:
//!
//! ```text
//! s:g/a/b/    Substitution(immutable => False, samespace => False,
//!                          adverbs => (ColonPair::True("g"),),
//!                          pattern => Regex::Literal("a"),
//!                          replacement => QuotedString(StrLiteral("b")))
//! S/a/b/      the same with immutable => True
//! ss/a/b/     the same with samespace => True and no adverb
//! s[a] = 1    the same with infix => Assignment and the replacement an
//!             expression
//! s:2nth/a/b/ adverbs => (ColonPair::Number(key => "nth", value => 2),)
//! s:x(2)/a/b/ adverbs => (ColonPair::Value(key => "x", value => (2)),)
//! tr:d/a//    Transliteration(destructive => True, left => QuotedString,
//!                             right => QuotedString, adverbs => (True("d"),))
//! TR/a/b/     destructive => False
//! ```
//!
//! The pattern is a regex tree directly, with no `QuotedRegex` around it. The
//! parser carries the tree and the adverbs as written beside what it executes
//! (`Expr::Subst::tree`), and lowering derives the executable form from them
//! again through the parser's own adverb routine
//! ([`subst_pattern_source`](crate::parser::subst_pattern_source)). The
//! replacement lowers to the per-match thunk of `s[...] = EXPR` — mutsu
//! evaluates it with the match bound as `$/`, which is what a `qq` replacement
//! reads — because a tree has no `qq` source to give back.

use super::convert::{
    convert_expr, leaf_field, node_field, quoted_string, regex_node, statement_expression,
    unsupported as unsupported_what,
};
use super::lower::{
    bool_field, leaf_str, list_field, lower_expr, lower_regex_node, named_child, positional_leaf,
    rakuast_node_of, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::Expr;
use crate::regex_tree::{RegexAdverb, RegexTree};
use crate::value::{RuntimeError, Value, ValueView};

/// `s///`, `S///` and `tr///` as their RakuAST nodes.
// Cost: O(n), n = size of the pattern and replacement.
pub(super) fn convert(expr: &Expr) -> Result<RakuAstNode, RuntimeError> {
    match expr {
        Expr::Subst {
            samespace,
            replacement,
            replacement_thunk,
            tree,
            ..
        } => substitution(
            true,
            *samespace,
            replacement,
            replacement_thunk.as_deref(),
            tree.as_deref(),
        ),
        Expr::NonDestructiveSubst {
            samespace,
            replacement,
            replacement_thunk,
            tree,
            ..
        } => substitution(
            false,
            *samespace,
            replacement,
            replacement_thunk.as_deref(),
            tree.as_deref(),
        ),
        Expr::Transliterate {
            from,
            to,
            non_destructive,
            adverbs,
            ..
        } => transliteration(from, to, !*non_destructive, adverbs),
        _ => Err(unsupported_what("substitution")),
    }
}

// Cost: O(n), n = size of the pattern and replacement.
fn substitution(
    destructive: bool,
    samespace: bool,
    replacement: &str,
    thunk: Option<&Expr>,
    tree: Option<&RegexTree>,
) -> Result<RakuAstNode, RuntimeError> {
    let tree =
        tree.ok_or_else(|| unsupported_what("substitution pattern without a source tree"))?;
    // `ss/a/b/` sets `samespace` without writing an adverb; `s:ss/a/b/` writes one.
    let written_ss = tree
        .adverbs
        .iter()
        .any(|a| matches!(a.name.as_str(), "ss" | "samespace"));
    let mut fields = vec![
        leaf_field(Some("immutable"), Value::truth(!destructive)),
        leaf_field(Some("samespace"), Value::truth(samespace && !written_ss)),
    ];
    push_adverbs(&mut fields, tree.adverbs.iter().map(adverb_node))?;
    if thunk.is_some() {
        fields.push(node_field(
            Some("infix"),
            RakuAstNode {
                class: RakuAstClass::Assignment,
                fields: Vec::new(),
            },
        ));
    }
    fields.push(node_field(Some("pattern"), regex_node(&tree.body)?));
    let replacement = match thunk {
        Some(expr) => convert_expr(expr)?,
        None => convert_expr(&crate::parser::interpolate_qq_content(replacement))?,
    };
    fields.push(node_field(Some("replacement"), replacement));
    Ok(RakuAstNode {
        class: RakuAstClass::Substitution,
        fields,
    })
}

// Cost: O(|from| + |to| + a), a = number of adverbs.
fn transliteration(
    from: &str,
    to: &str,
    destructive: bool,
    adverbs: &[String],
) -> Result<RakuAstNode, RuntimeError> {
    let mut fields = vec![
        leaf_field(Some("destructive"), Value::truth(destructive)),
        node_field(Some("left"), quoted_string(Value::str(from.to_string()))),
        node_field(Some("right"), quoted_string(Value::str(to.to_string()))),
    ];
    push_adverbs(
        &mut fields,
        adverbs.iter().map(|name| {
            Ok(RakuAstNode {
                class: RakuAstClass::ColonPairTrue,
                fields: vec![leaf_field(None, Value::str(name.clone()))],
            })
        }),
    )?;
    Ok(RakuAstNode {
        class: RakuAstClass::Transliteration,
        fields,
    })
}

/// The `adverbs` field, left out when there are none.
fn push_adverbs(
    fields: &mut Vec<RakuAstField>,
    nodes: impl Iterator<Item = Result<RakuAstNode, RuntimeError>>,
) -> Result<(), RuntimeError> {
    let nodes = nodes
        .map(|node| node.map(|n| Value::rakuast(Box::new(n))))
        .collect::<Result<Vec<_>, _>>()?;
    if !nodes.is_empty() {
        fields.push(RakuAstField {
            name: Some("adverbs"),
            value: RakuAstFieldValue::List(nodes),
        });
    }
    Ok(())
}

/// One written adverb: `:g`, `:2nth`, `:x(2)`.
// Cost: O(|argument|).
pub(super) fn adverb_node(adverb: &RegexAdverb) -> Result<RakuAstNode, RuntimeError> {
    let digits = adverb
        .name
        .chars()
        .take_while(char::is_ascii_digit)
        .collect::<String>();
    if !digits.is_empty() {
        let key = &adverb.name[digits.len()..];
        let count = digits
            .parse::<i64>()
            .map_err(|_| unsupported_what("regex adverb count"))?;
        return Ok(RakuAstNode {
            class: RakuAstClass::ColonPairNumber,
            fields: vec![
                leaf_field(Some("key"), Value::str(key.to_string())),
                node_field(
                    Some("value"),
                    RakuAstNode {
                        class: RakuAstClass::IntLiteral,
                        fields: vec![leaf_field(None, Value::int(count))],
                    },
                ),
            ],
        });
    }
    let Some(argument) = &adverb.argument else {
        return Ok(RakuAstNode {
            class: RakuAstClass::ColonPairTrue,
            fields: vec![leaf_field(None, Value::str(adverb.name.clone()))],
        });
    };
    let expr = crate::parser::parse_adverb_argument(argument)
        .ok_or_else(|| unsupported_what("regex adverb argument"))?;
    let value = convert_expr(&expr)?;
    // What cannot be spelled again cannot be lowered: refuse it here.
    if argument_text(&value).is_none() {
        return Err(unsupported_what("regex adverb argument"));
    }
    let semilist = RakuAstNode {
        class: RakuAstClass::SemiList,
        fields: vec![node_field(None, statement_expression(value))],
    };
    Ok(RakuAstNode {
        class: RakuAstClass::ColonPairValue,
        fields: vec![
            leaf_field(Some("key"), Value::str(adverb.name.clone())),
            node_field(
                Some("value"),
                RakuAstNode {
                    class: RakuAstClass::CircumfixParentheses,
                    fields: vec![node_field(None, semilist)],
                },
            ),
        ],
    })
}

/// The text of an adverb argument that is a number, `*`, a scalar variable, a
/// range of those or a comma list of those; `None` for anything else.
// Cost: O(n), n = size of the argument.
fn argument_text(node: &RakuAstNode) -> Option<String> {
    let text_of = |v: Value| match v.view() {
        ValueView::Str(s) => Some(s.to_string()),
        ValueView::Int(_) | ValueView::BigInt(_) => Some(v.to_string_value()),
        _ => None,
    };
    match node.class {
        RakuAstClass::IntLiteral | RakuAstClass::VarLexical => text_of(positional_leaf(node).ok()?),
        RakuAstClass::TermWhatever => Some("*".to_string()),
        RakuAstClass::ApplyInfix => {
            let op = text_of(positional_leaf(named_child(node, "infix").ok()?).ok()?)?;
            if !matches!(op.as_str(), ".." | "..^" | "^.." | "^..^") {
                return None;
            }
            let left = argument_text(named_child(node, "left").ok()?)?;
            let right = argument_text(named_child(node, "right").ok()?)?;
            Some(format!("{left}{op}{right}"))
        }
        RakuAstClass::ApplyListInfix => {
            let op = text_of(positional_leaf(named_child(node, "infix").ok()?).ok()?)?;
            if op != "," {
                return None;
            }
            let mut parts = Vec::new();
            for operand in list_field(node, "operands").ok()? {
                parts.push(argument_text(rakuast_node_of(operand)?)?);
            }
            Some(parts.join(","))
        }
        _ => None,
    }
}

/// The adverbs a node lists, as the parser spells them.
// Cost: O(a * n), a = number of adverbs, n = size of an argument.
fn adverbs_of(node: &RakuAstNode) -> Result<Vec<RegexAdverb>, RuntimeError> {
    if !node.fields.iter().any(|f| f.name == Some("adverbs")) {
        return Ok(Vec::new());
    }
    list_field(node, "adverbs")?
        .iter()
        .map(|item| {
            let adverb = rakuast_node_of(item).ok_or_else(|| unsupported(node))?;
            lower_adverb(adverb)
        })
        .collect()
}

// Cost: O(n), n = size of the adverb.
fn lower_adverb(node: &RakuAstNode) -> Result<RegexAdverb, RuntimeError> {
    let string = |v: Value| match v.view() {
        ValueView::Str(s) => Ok(s.to_string()),
        _ => Err(unsupported(node)),
    };
    match node.class {
        RakuAstClass::ColonPairTrue => Ok(RegexAdverb {
            name: string(positional_leaf(node)?)?,
            argument: None,
        }),
        RakuAstClass::ColonPairNumber => {
            let count =
                argument_text(named_child(node, "value")?).ok_or_else(|| unsupported(node))?;
            Ok(RegexAdverb {
                name: format!("{count}{}", leaf_str(node, "key")?),
                argument: None,
            })
        }
        RakuAstClass::ColonPairValue => {
            let value = named_child(node, "value")?;
            let argument = parenthesized_expression(value)
                .and_then(argument_text)
                .ok_or_else(|| unsupported(node))?;
            Ok(RegexAdverb {
                name: leaf_str(node, "key")?,
                argument: Some(argument),
            })
        }
        _ => Err(unsupported(node)),
    }
}

/// The expression of `(EXPR)`: a parenthesis holding one statement.
fn parenthesized_expression(node: &RakuAstNode) -> Option<&RakuAstNode> {
    if node.class != RakuAstClass::CircumfixParentheses {
        return None;
    }
    let semilist = match &node.fields.first()?.value {
        RakuAstFieldValue::Node(v) => rakuast_node_of(v)?,
        _ => return None,
    };
    let [only] = semilist.fields.as_slice() else {
        return None;
    };
    let RakuAstFieldValue::Node(v) = &only.value else {
        return None;
    };
    named_child(rakuast_node_of(v)?, "expression").ok()
}

/// `Substitution` as the parser's `Expr::Subst` / `Expr::NonDestructiveSubst`.
// Cost: O(n), n = size of the pattern and replacement.
pub(super) fn lower_substitution(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let adverbs = adverbs_of(node)?;
    let samespace = bool_field(node, "samespace")?;
    if let Some(infix) = node.fields.iter().find(|f| f.name == Some("infix")) {
        let is_assignment = match &infix.value {
            RakuAstFieldValue::Node(v) => {
                rakuast_node_of(v).is_some_and(|n| n.class == RakuAstClass::Assignment)
            }
            _ => false,
        };
        if !is_assignment {
            return Err(unsupported(node));
        }
    }
    let regex = RegexTree {
        body: lower_regex_node(named_child(node, "pattern")?)?,
        match_immediately: false,
        adverbs: Vec::new(),
        declaration_kind: None,
    };
    let (pattern, flags) =
        crate::parser::subst_pattern_source(regex.to_source(), &adverbs, samespace)
            .ok_or_else(|| unsupported(node))?;
    let thunk = Some(Box::new(lower_expr(named_child(node, "replacement")?)?));
    let (replacement, tree) = (String::new(), None);
    let crate::parser::SubstFlags {
        samecase,
        sigspace,
        samemark,
        samespace,
        global,
        nth,
        x,
    } = flags;
    Ok(if bool_field(node, "immutable")? {
        Expr::NonDestructiveSubst {
            pattern,
            replacement,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth,
            x,
            replacement_thunk: thunk,
            tree,
        }
    } else {
        Expr::Subst {
            pattern,
            replacement,
            samecase,
            sigspace,
            samemark,
            samespace,
            global,
            nth,
            x,
            replacement_thunk: thunk,
            tree,
        }
    })
}

/// A table side: a quoted string of plain text.
fn table_text(node: &RakuAstNode) -> Result<String, RuntimeError> {
    match lower_expr(node)? {
        Expr::Literal(v) | Expr::LiteralSrc(v, _) => match v.view() {
            ValueView::Str(s) => Ok(s.to_string()),
            _ => Err(unsupported(node)),
        },
        _ => Err(unsupported(node)),
    }
}

/// `Transliteration` as the parser's `Expr::Transliterate`.
// Cost: O(|from| + |to| + a), a = number of adverbs.
pub(super) fn lower_transliteration(node: &RakuAstNode) -> Result<Expr, RuntimeError> {
    let (mut delete, mut complement, mut squash) = (false, false, false);
    let mut written = Vec::new();
    for adverb in adverbs_of(node)? {
        match adverb.name.as_str() {
            "d" | "delete" => delete = true,
            "c" | "complement" => complement = true,
            "s" | "squash" => squash = true,
            _ => return Err(unsupported(node)),
        }
        if adverb.argument.is_some() {
            return Err(unsupported(node));
        }
        written.push(adverb.name);
    }
    Ok(Expr::Transliterate {
        from: table_text(named_child(node, "left")?)?,
        to: table_text(named_child(node, "right")?)?,
        delete,
        complement,
        squash,
        non_destructive: !bool_field(node, "destructive")?,
        adverbs: written,
    })
}
