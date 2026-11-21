//! Origins derived from the origins of a node's operands (ADR-10723 Stage 2,
//! expression families): an `ApplyInfix` and its operator, the `Name` and
//! `ArgList` of a named call, and a variable's `Initializer`.
//!
//! The parser notes terms, not the operators between them, so these nodes are
//! placed from their converted children: an infix application spans its
//! operands, and its operator is the text between them. Every range is
//! checked against the source text it claims (the operator written between
//! the operands is the node's operator, a call's text starts with its name),
//! so a node the parser made rather than read — `$x += 1`'s copy of `$x + 1`
//! — fails the check and stays without an origin: exact or absent.

use super::{RakuAstClass, RakuAstFieldValue, RakuAstNode};
use crate::ast::SrcSpan;
use crate::scope::NameScope;
use crate::value::{Value, ValueView};

/// Place `node`, a node just converted, and its operator / name / argument
/// parts, from the origins its children carry.
// Cost: O(f + n), f = fields of `node`, n = length of the source text read.
pub(super) fn derive(names: &NameScope, node: &mut RakuAstNode) {
    match node.class {
        RakuAstClass::ApplyInfix => {
            apply_infix(node);
            super::decl_compound_origins::left_call(node);
        }
        RakuAstClass::CallName | RakuAstClass::CallNameWithoutParentheses => call_name(names, node),
        RakuAstClass::VarDeclarationSimple => {
            initializer(names, node);
            super::call_origins::call_assign_initializer(node);
            super::decl_origins::derive(names, node);
        }
        // `\(1, 2)`: the capture's arguments.
        RakuAstClass::TermCapture => super::capture_term::arg_list_origin(node),
        // `$.foo(1)` / `$.foo: 5`: the accessor call's arguments.
        RakuAstClass::VarAttributePublic => super::attribute_call_origins::public_args(node),
        // `constant X = EXPR` / `my constant %h := EXPR`.
        RakuAstClass::VarDeclarationConstant | RakuAstClass::VarDeclarationAnonymous => {
            initializer(names, node);
            super::decl_origins::constant_traits(names, node);
        }
        // `my \x = EXPR`: the name after the backslash, and the initializer.
        RakuAstClass::VarDeclarationTerm => {
            initializer(names, node);
            super::sigilless_origins::name(node);
        }
        RakuAstClass::VarDeclarationSignature => {
            initializer(names, node);
            super::signature_decl_origins::declaration(node);
        }
        RakuAstClass::ApplyListInfix => {
            super::fragment_origins::list_operands(names, node);
            super::list_origins::apply_list_infix(node)
        }
        RakuAstClass::TypeEnum => super::enum_term::term_origins(names, node),
        RakuAstClass::VarPackage => super::indirect_name_origins::package_variable(node),
        RakuAstClass::VarNamedCapture => super::match_vars::named_capture_origins(node),
        // `HyperWhatever:D` as a term: the inner `Type::Simple` spans all of
        // it, its `Name` the name before the smiley.
        RakuAstClass::TypeDefinedness | RakuAstClass::TypeAnyDefinedness => {
            if let Some((from, to)) = byte_range(node) {
                node.origin.0 = None;
                super::routine_origins::type_origin(node, SrcSpan::new(from, to));
            }
        }
        // `[op] args`: the operator between the brackets, the arguments after.
        RakuAstClass::TermReduce => super::reduce_origins::derive(node),
        // `:16($n)`: the parentheses after the radix.
        RakuAstClass::TermRadixNumber => super::type_origins::radix_parentheses(node),
        RakuAstClass::TypeParameterized => {
            if let Some((from, to)) = byte_range(node) {
                super::type_origins::parameterized(node, SrcSpan::new(from, to));
            }
        }
        RakuAstClass::CircumfixParentheses | RakuAstClass::CircumfixArrayComposer => {
            super::list_origins::place_lone_declaration(names, node);
            super::list_origins::circumfix(node)
        }
        // `s:g/a/b/` / `s[a] = b`: the adverbs, pattern, operator, replacement.
        RakuAstClass::Substitution => super::subst_origins::derive(names, node),
        // `{:2b, :!c, :e(5)}`: the colonpairs among the composer's entries.
        RakuAstClass::CircumfixHashComposer => super::hash_colonpair_origins::derive(names, node),
        RakuAstClass::ContextualizerHash => {
            super::hash_colonpair_origins::contextualizer(names, node);
            super::operator_origins::derive(node);
        }
        RakuAstClass::ApplyPostfix => {
            super::list_origins::postcircumfix(node);
            super::operator_origins::derive(node);
            super::quoted_method_origins::segments(names, node);
            super::attribute_call_origins::meta_item(node);
        }
        RakuAstClass::QuotedString => super::string_origins::interpolated(names, node),
        RakuAstClass::QuotedRegex | RakuAstClass::QuotedRegexRx => {
            super::regex_origins::quoted(names, node)
        }
        RakuAstClass::TokenDeclaration
        | RakuAstClass::RuleDeclaration
        | RakuAstClass::RegexDeclaration => super::regex_decl_origins::declaration(names, node),
        RakuAstClass::Block
        | RakuAstClass::Sub
        | RakuAstClass::Method
        | RakuAstClass::Submethod
        | RakuAstClass::PointyBlock
        | RakuAstClass::StatementPrefixDo
        | RakuAstClass::StatementPrefixTry
        | RakuAstClass::StatementPrefixGather
        | RakuAstClass::StatementPrefixStart
        | RakuAstClass::StatementPrefixQuietly
        | RakuAstClass::StatementPrefixSink
        | RakuAstClass::StatementPrefixLazy
        | RakuAstClass::StatementPrefixHyper
        | RakuAstClass::StatementPrefixRace
        | RakuAstClass::StatementPrefixEager
        | RakuAstClass::StatementPrefixOnce
        | RakuAstClass::StatementPrefixReact
        | RakuAstClass::StatementPrefixSupply => {
            super::expr_block_origins::derive(node);
            // `sub ($x) { put $x }`: the statements of a one-line body.
            super::anon_body_origins::statements(names, node);
        }
        _ => {
            super::expr_block_origins::phaser_statement(names, node);
            super::operator_origins::derive(node);
            super::prefix_operand_origins::place(names, node);
        }
    }
}

/// The byte range a node's origin covers.
// Cost: O(1).
pub(super) fn byte_range(node: &RakuAstNode) -> Option<(usize, usize)> {
    let origin = node.origin.0.as_ref()?;
    let ValueView::Instance { attributes, .. } = origin.view() else {
        return None;
    };
    let map = attributes.as_map();
    let at = |name: &str| match map.get(name)?.view() {
        ValueView::Int(char) => super::unit_source::byte_of(char).map(|b| b as usize),
        _ => None,
    };
    Some((at("from")?, at("to")?))
}

/// The node held in `node`'s field `name`.
// Cost: O(f), f = fields of `node`.
pub(super) fn child<'a>(node: &'a RakuAstNode, name: &str) -> Option<&'a RakuAstNode> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) if f.name == Some(name) => match v.view() {
            ValueView::RakuAst(held) => Some(held),
            _ => None,
        },
        _ => None,
    })
}

/// Apply `f` to the node held in `node`'s field `name`, if it holds one.
// Cost: O(f), f = fields of `node`, plus `f`.
pub(super) fn with_child(node: &mut RakuAstNode, name: &str, f: impl FnOnce(&mut RakuAstNode)) {
    let Some(field) = node
        .fields
        .iter_mut()
        .find(|field| field.name == Some(name))
    else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(inner) = held.view() else {
        return;
    };
    let mut inner = RakuAstNode::clone(inner);
    f(&mut inner);
    *held = Value::rakuast(Box::new(inner));
}

/// The text of the first leaf field of `node` (an `Infix`'s operator, a
/// `Name`'s identifier).
// Cost: O(f), f = fields of `node`.
pub(super) fn leaf_text(node: &RakuAstNode) -> Option<String> {
    node.fields.iter().find_map(|f| match &f.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::Str(s) => Some(s.to_string()),
            _ => None,
        },
        _ => None,
    })
}

/// The node in `node`'s `index`-th field.
// Cost: O(1).
pub(super) fn child_at(node: &RakuAstNode, index: usize) -> Option<&RakuAstNode> {
    match &node.fields.get(index)?.value {
        RakuAstFieldValue::Node(v) => match v.view() {
            ValueView::RakuAst(held) => Some(held),
            _ => None,
        },
        _ => None,
    }
}

/// `left OP right`: the application spans its operands (when it has no
/// origin of its own yet), and its operator is the `OP` written between them.
// Cost: O(n), n = length of the source between the operands.
fn apply_infix(node: &mut RakuAstNode) {
    leading_variable(node);
    super::decl_compound_origins::leading_declaration(node);
    super::lone_term_origins::variable_before_operator(node);
    let left = child(node, "left").and_then(byte_range);
    let right = child(node, "right").and_then(byte_range);
    if child(node, "infix").is_some_and(|i| i.class == RakuAstClass::MetaInfixHyper) {
        super::hyper_infix_origins::place(node, left, right);
        return;
    }
    if child(node, "infix")
        .and_then(|i| child_at(i, 0))
        .is_some_and(|i| i.class == RakuAstClass::MetaInfixHyper)
    {
        super::hyper_infix_origins::place_assign(node, left, right);
        return;
    }
    let Some((text, inner)) = child(node, "infix").and_then(super::infix_spelling::written) else {
        return;
    };
    let (left, right) = super::whatever_origins::operand_ranges(node, &text, left, right);
    let own = byte_range(node);
    // The operator follows the left operand after whitespace, or ends where
    // whitespace before the right operand begins.
    // A plain operator may be written in one of its Unicode spellings
    // (`×` for `*`), which the `Infix` then takes.
    let mut spelled = None;
    let op_at = match (left, right, own) {
        (Some((_, left_end)), Some((right_start, _)), _) if left_end <= right_start => {
            super::unit_source::with_text(SrcSpan::new(left_end, right_start), |between| {
                let after = between.trim_start();
                let written = if inner.is_none()
                    && child(node, "infix").is_some_and(|i| i.class == RakuAstClass::Infix)
                {
                    super::infix_spelling::written_prefix(&text, after)?
                } else {
                    after.strip_prefix(text.as_str()).map(|_| text.as_str())?
                };
                let rest = &after[written.len()..];
                if written != text {
                    spelled = Some(written.to_string());
                }
                (rest.is_empty() || rest.starts_with(char::is_whitespace) || rest.starts_with('#'))
                    .then_some(left_end + (between.len() - after.len()))
            })
        }
        (None, Some((right_start, _)), Some((own_start, _))) if own_start < right_start => {
            super::unit_source::with_text(SrcSpan::new(own_start, right_start), |before| {
                let trimmed = before.trim_end();
                let head = trimmed.strip_suffix(text.as_str())?;
                (head.is_empty() || head.ends_with(char::is_whitespace))
                    .then_some(own_start + trimmed.len() - text.len())
            })
        }
        // `$x = $y = 1`: a right operand the parser left unplaced; the
        // operator follows the left one after blanks, and blanks follow it.
        (Some((_, left_end)), None, Some((_, own_end))) if left_end < own_end => {
            super::unit_source::with_text(SrcSpan::new(left_end, own_end), |after_left| {
                let after = after_left.trim_start();
                let rest = after.strip_prefix(text.as_str())?;
                rest.starts_with(char::is_whitespace)
                    .then_some(left_end + (after_left.len() - after.len()))
            })
        }
        _ => None,
    };
    let Some(op_at) = op_at else {
        return;
    };
    if own.is_none()
        && let (Some((from, _)), Some((_, to))) = (left, right)
    {
        super::origin::set_span(node, SrcSpan::new(from, to));
    }
    super::hash_colonpair_origins::infix_adverbs(node);
    super::lone_term_origins::left_before_operator(node, op_at);
    super::lone_term_origins::right_after_operator(node, op_at + text.len());
    let text = spelled.unwrap_or(text);
    with_child(node, "infix", |infix| {
        if text != leaf_text(infix).unwrap_or_default() {
            set_leaf_text(infix, &text);
        }
        super::origin::set_span(infix, SrcSpan::new(op_at, op_at + text.len()));
        if let Some((from, to)) = inner {
            with_child_at(infix, 0, |op| {
                super::origin::set_span(op, SrcSpan::new(op_at + from, op_at + to));
                super::infix_spelling::place_inner(op, op_at + from);
            });
        }
    });
}

/// The variable an infix application placed at the statement level starts
/// with (`$x = 1`, `$x += 1`): an assignment keeps its target by name, so the
/// parser's note for it finds no term to attach to. Its range is the
/// variable's spelling at the start of the application, when it is written
/// there.
// Cost: O(v), v = length of the variable's name.
fn leading_variable(node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(left) = child(node, "left") else {
        return;
    };
    if left.origin.0.is_some() {
        return;
    }
    let name = match left.class {
        RakuAstClass::VarLexical
        | RakuAstClass::VarDynamic
        | RakuAstClass::VarAttribute
        | RakuAstClass::VarAttributePublic => leaf_text(left),
        // `% = a => 1`: an anonymous variable is its sigil.
        RakuAstClass::VarDeclarationAnonymous => left.fields.iter().find_map(|f| match &f.value {
            RakuAstFieldValue::Node(v) if f.name == Some("sigil") => match v.view() {
                ValueView::Str(s) => Some(s.to_string()),
                _ => None,
            },
            _ => None,
        }),
        // `foo = 7`: a sigilless term, its `Name` over the same text.
        RakuAstClass::TermName => match child_at(left, 0).and_then(super::name_parts::name_shape) {
            Some(super::name_parts::NameShape::Identifier(name)) => Some(name),
            _ => None,
        },
        _ => None,
    };
    let Some(name) = name else {
        return;
    };
    let written =
        super::unit_source::with_text(SrcSpan::new(from, from + name.len() + 1), |text| {
            let rest = text.strip_prefix(name.as_str())?;
            (!rest.starts_with(|c: char| c.is_alphanumeric() || matches!(c, '_' | '-' | '\'')))
                .then_some(())
        })
        .or_else(|| {
            // The variable ends the source.
            super::unit_source::with_text(SrcSpan::new(from, from + name.len()), |text| {
                (text == name).then_some(())
            })
        });
    if written.is_some() {
        let span = SrcSpan::new(from, from + name.len());
        with_child(node, "left", |left| {
            super::origin::set_span(left, span);
            super::origin::named_term(left, span);
        });
    }
}

/// Apply `f` to the node in `node`'s `index`-th field.
// Cost: O(1) plus `f`.
pub(super) fn with_child_at(
    node: &mut RakuAstNode,
    index: usize,
    f: impl FnOnce(&mut RakuAstNode),
) {
    let Some(field) = node.fields.get_mut(index) else {
        return;
    };
    let RakuAstFieldValue::Node(held) = &mut field.value else {
        return;
    };
    let ValueView::RakuAst(inner) = held.view() else {
        return;
    };
    let mut inner = RakuAstNode::clone(inner);
    f(&mut inner);
    *held = Value::rakuast(Box::new(inner));
}

/// `name(args)` / `name args`: the `Name` is the identifier the call starts
/// with, the `ArgList` its arguments, in parentheses
/// ([`super::call_origins::parenthesized`]) or from its first argument to its
/// last.
// Cost: O(n + a), n = length of the call's source, a = its arguments.
fn call_name(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, to)) = byte_range(node) else {
        return;
    };
    if super::indirect_name_origins::call(node, SrcSpan::new(from, to))
        || super::indirect_name_origins::bare_call(node, SrcSpan::new(from, to))
    {
        return;
    }
    let Some(name) = child(node, "name")
        .and_then(|n| super::indirect_name_origins::written_operator(n).or_else(|| leaf_text(n)))
    else {
        return;
    };
    let parens = node.class == RakuAstClass::CallName;
    let fits = super::unit_source::with_text(SrcSpan::new(from, to), |text| {
        let rest = text.strip_prefix(name.as_str())?;
        let ok = if parens {
            rest.starts_with('(') && rest.ends_with(')')
        } else {
            rest.is_empty() || rest.starts_with(char::is_whitespace)
        };
        ok.then_some(())
    });
    if fits.is_none() {
        return;
    }
    let name_end = from + name.len();
    if !parens {
        super::label_origins::name_argument(node, name_end, to);
        super::call_origins::leading_block_argument(node, name_end, to);
        super::fragment_origins::arguments(names, node, SrcSpan::new(name_end, to));
    }
    let args = if parens {
        super::fragment_origins::arguments(names, node, SrcSpan::new(name_end + 1, to - 1));
        super::call_origins::parenthesized(child(node, "args"), SrcSpan::new(name_end + 1, to - 1))
    } else if child(node, "args").is_none() {
        // A bare `foo`: Rakudo's empty `ArgList` sits right after the name.
        (name_end == to).then_some((to, to))
    } else {
        child(node, "args").and_then(|list| {
            let first = child_at(list, 0).and_then(byte_range)?;
            let last = child_at(list, list.fields.len().checked_sub(1)?).and_then(byte_range)?;
            // `say :{ 1 => 2 }`: an object hash's range starts at its `{`.
            let brace =
                super::unit_source::with_text_from(first.0, |t| t.starts_with('{').then_some(()));
            let gap = super::unit_source::with_text(SrcSpan::new(name_end, first.0), |gap| {
                let gap = brace.and(gap.strip_suffix(':')).unwrap_or(gap);
                gap.chars().all(char::is_whitespace).then_some(())
            });
            (gap.is_some() && last.1 == to).then_some((first.0, last.1))
        })
    };
    with_child(node, "name", |n| {
        super::origin::set_span(n, SrcSpan::new(from, name_end));
        super::indirect_name_origins::operator_quote(n, SrcSpan::new(from, name_end));
    });
    if let Some((args_from, args_to)) = args {
        super::empty_fields::set_span(node, "args", SrcSpan::new(args_from, args_to));
        with_child(node, "args", |list| {
            super::origin::set_span(list, SrcSpan::new(args_from, args_to))
        });
    }
}

/// `my $x = EXPR` / `my $x := EXPR`: the `Initializer` runs from its `=` /
/// `:=` to the end of the expression.
// Cost: O(n), n = length of the declaration's source.
fn initializer(names: &NameScope, node: &mut RakuAstNode) {
    let Some((from, _)) = byte_range(node) else {
        return;
    };
    let Some(init) = child(node, "initializer") else {
        return;
    };
    let op = match init.class {
        RakuAstClass::InitializerAssign => "=",
        RakuAstClass::InitializerBind => ":=",
        _ => return,
    };
    if !super::initializer_origins::placed_by_rakudo(node, init) {
        return;
    }
    let Some((expr_from, expr_to)) = child_at(init, 0).and_then(byte_range) else {
        super::initializer_origins::unplaced_expression(names, node, op);
        return;
    };
    if expr_from <= from {
        return;
    }
    let op_at = super::unit_source::with_text(SrcSpan::new(from, expr_from), |before| {
        // `my %o = :{ 1 => 2 }`: an object hash's range starts at its `{`.
        let before = before.strip_suffix(':').unwrap_or(before);
        let head = before.trim_end().strip_suffix(op)?;
        // `==`, `::=` and the like are other operators.
        (!head.ends_with(['=', ':', '!', '<', '>', '~'])).then_some(from + head.len())
    });
    if let Some(op_at) = op_at {
        with_child(node, "initializer", |init| {
            super::origin::set_span(init, SrcSpan::new(op_at, expr_to))
        });
    }
}

/// Replace the text of `node`'s first leaf field (an `Infix`'s operator).
// Cost: O(f), f = fields of `node`.
pub(super) fn set_leaf_text(node: &mut RakuAstNode, text: &str) {
    if let Some(field) = node
        .fields
        .iter_mut()
        .find(|f| matches!(&f.value, RakuAstFieldValue::Node(v) if v.as_str().is_some()))
    {
        field.value = RakuAstFieldValue::Node(Value::str(text.to_string()));
    }
}
