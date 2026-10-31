//! The source-level regex tree as a RakuAST regex node (read direction).

use super::convert::*;
use super::*;
use crate::regex_tree::{RegexExtension, RegexModifierKind, RegexNode};
use crate::scope::NameScope;
use crate::value::{RuntimeError, Value};

/// Convert the source-level regex tree to the corresponding RakuAST regex
/// node. The tree is deliberately separate from `RegexPattern`: the latter is
/// an execution plan and has already lost source-level declaration and
/// whitespace information by the time matching begins.
pub(super) fn regex_node(names: &NameScope, node: &RegexNode) -> Result<RakuAstNode, RuntimeError> {
    let (class, fields) = match node {
        RegexNode::Extension(extension) => {
            return super::regex_extension::convert(names, extension);
        }
        RegexNode::Literal(text) => (
            RakuAstClass::RegexLiteral,
            vec![leaf_field(None, Value::str(text.clone()))],
        ),
        RegexNode::Quote(text) => (
            RakuAstClass::RegexQuote,
            vec![node_field(None, quoted_string(Value::str(text.clone())))],
        ),
        RegexNode::Sequence(nodes) => (
            RakuAstClass::RegexSequence,
            nodes
                .iter()
                .map(|child| regex_node(names, child).map(|node| node_field(None, node)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        RegexNode::Alternation(nodes) => (
            RakuAstClass::RegexAlternation,
            nodes
                .iter()
                .map(|child| regex_node(names, child).map(|node| node_field(None, node)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        RegexNode::SequentialAlternation(nodes) => (
            RakuAstClass::RegexSequentialAlternation,
            nodes
                .iter()
                .map(|child| regex_node(names, child).map(|node| node_field(None, node)))
                .collect::<Result<Vec<_>, _>>()?,
        ),
        RegexNode::Group(child) => (
            RakuAstClass::RegexGroup,
            vec![node_field(None, regex_node(names, child)?)],
        ),
        RegexNode::CapturingGroup(child) => (
            RakuAstClass::RegexCapturingGroup,
            vec![node_field(None, regex_node(names, child)?)],
        ),
        RegexNode::NamedCapture {
            name,
            array,
            hash,
            regex,
        } => {
            let mut fields = vec![leaf_field(Some("name"), Value::str(name.clone()))];
            if *array {
                fields.push(leaf_field(Some("array"), Value::truth(true)));
            }
            if *hash {
                fields.push(leaf_field(Some("hash"), Value::truth(true)));
            }
            fields.push(node_field(Some("regex"), regex_node(names, regex)?));
            (RakuAstClass::RegexNamedCapture, fields)
        }
        RegexNode::Subrule {
            name,
            capturing,
            args,
            ..
        } => {
            let mut name_node = name_from_identifier(name);
            // `<value:sym<number>>`: the adverb belongs to the name.
            let sym = args.as_deref().and_then(super::regex_sym::name_colonpairs);
            if let Some(colonpairs) = sym.clone() {
                name_node.fields.push(colonpairs);
            }
            let mut fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
                && sym.is_none()
            {
                fields.push(node_field(Some("args"), regex_arg_list(names, args)?));
            }
            if *capturing {
                fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            (
                if args.is_some() && sym.is_none() {
                    RakuAstClass::RegexAssertionNamedArgs
                } else {
                    RakuAstClass::RegexAssertionNamed
                },
                fields,
            )
        }
        RegexNode::SubruleAlias {
            alias,
            name,
            capturing,
            args,
        } => {
            let mut name_node = name_from_identifier(name);
            let sym = args.as_deref().and_then(super::regex_sym::name_colonpairs);
            if let Some(colonpairs) = sym.clone() {
                name_node.fields.push(colonpairs);
            }
            let mut assertion_fields = vec![node_field(Some("name"), name_node)];
            if let Some(args) = args
                && !args.args.is_empty()
                && sym.is_none()
            {
                assertion_fields.push(node_field(Some("args"), regex_arg_list(names, args)?));
            }
            if *capturing {
                assertion_fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            let assertion = RakuAstNode {
                origin: Default::default(),
                class: if args.is_some() && sym.is_none() {
                    RakuAstClass::RegexAssertionNamedArgs
                } else {
                    RakuAstClass::RegexAssertionNamed
                },
                fields: assertion_fields,
            };
            (
                RakuAstClass::RegexAssertionAlias,
                vec![
                    leaf_field(Some("name"), Value::str(alias.clone())),
                    node_field(Some("assertion"), assertion),
                ],
            )
        }
        RegexNode::Lookaround {
            assertion,
            negated,
            is_behind,
        } => {
            let keyword = if *is_behind { "after" } else { "before" };
            let named_assertion = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::RegexAssertionNamedRegexArg,
                fields: vec![
                    node_field(Some("name"), name_from_identifier(keyword)),
                    node_field(Some("regex-arg"), regex_node(names, assertion)?),
                    leaf_field(Some("capturing"), Value::truth(true)),
                ],
            };
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("assertion"), named_assertion));
            (RakuAstClass::RegexAssertionLookahead, fields)
        }
        RegexNode::ArrayLookaround { name, negated } => {
            let interpolated = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::RegexAssertionInterpolatedVar,
                fields: vec![
                    leaf_field(Some("sequential"), Value::truth(false)),
                    node_field(Some("var"), var_lexical("@", name)),
                ],
            };
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("assertion"), interpolated));
            (RakuAstClass::RegexAssertionLookahead, fields)
        }
        // `<::(EXPR)>` -> `Assertion::Named` over `Name(Part::Empty,
        // Part::Expression(EXPR))` (measured on 2026.09).
        RegexNode::SymbolicSubrule {
            expr, capturing, ..
        } => {
            let part = RakuAstNode {
                origin: Default::default(),
                class: RakuAstClass::NamePartExpression,
                fields: vec![node_field(None, convert_expr(names, expr)?)],
            };
            let name = name_parts::name_from_parts(vec![
                name_parts::leading_empty(),
                Value::rakuast(Box::new(part)),
            ]);
            let mut fields = vec![node_field(Some("name"), name)];
            if *capturing {
                fields.push(leaf_field(Some("capturing"), Value::truth(true)));
            }
            (RakuAstClass::RegexAssertionNamed, fields)
        }
        RegexNode::Callable { name, args, .. } => {
            // `<&$var>`: rakudo item-contextualizes the scalar.
            let callee = match name.strip_prefix('$') {
                Some(name) => RakuAstNode {
                    origin: Default::default(),
                    class: RakuAstClass::ContextualizerItem,
                    fields: vec![node_field(None, var_lexical("$", name))],
                },
                None => var_lexical("&", name),
            };
            let mut fields = vec![node_field(Some("callee"), callee)];
            if !args.is_empty() {
                fields.push(node_field(Some("args"), arg_list(names, args)?));
            }
            (RakuAstClass::RegexAssertionCallable, fields)
        }
        RegexNode::CodeAssertion {
            body,
            negated,
            code,
        } => {
            let mut fields = Vec::new();
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            fields.push(node_field(Some("block"), block_node(names, body)?));
            fields.push(super::regex_code::source_field(code));
            (RakuAstClass::RegexAssertionPredicateBlock, fields)
        }
        RegexNode::CodeBlock { body, code } => (
            RakuAstClass::RegexBlock,
            vec![
                node_field(None, block_node(names, body)?),
                super::regex_code::source_field(code),
            ],
        ),
        RegexNode::InterpolatedBlock {
            body,
            sequential,
            code,
        } => (
            RakuAstClass::RegexAssertionInterpolatedBlock,
            vec![
                node_field(Some("block"), block_node(names, body)?),
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                super::regex_code::source_field(code),
            ],
        ),
        RegexNode::NamedLookaround {
            assertion,
            is_behind,
            capturing,
        } => {
            let keyword = if *is_behind { "after" } else { "before" };
            (
                RakuAstClass::RegexAssertionNamedRegexArg,
                vec![
                    node_field(Some("name"), name_from_identifier(keyword)),
                    node_field(Some("regex-arg"), regex_node(names, assertion)?),
                    leaf_field(Some("capturing"), Value::truth(*capturing)),
                ],
            )
        }
        RegexNode::Interpolation { name, sequential } => (
            RakuAstClass::RegexInterpolation,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical("$", name)),
            ],
        ),
        RegexNode::RegexValueInterpolation {
            name,
            sequential,
            sigil,
        } => (
            RakuAstClass::RegexAssertionInterpolatedVar,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical(&sigil.to_string(), name)),
            ],
        ),
        RegexNode::ArrayInterpolation { name, sequential } => (
            RakuAstClass::RegexInterpolation,
            vec![
                leaf_field(Some("sequential"), Value::truth(*sequential)),
                node_field(Some("var"), var_lexical("@", name)),
            ],
        ),
        RegexNode::Quantified { atom, quantifier } => {
            let separator = quantifier
                .separator
                .as_ref()
                .map(|separator| regex_node(names, &separator.node))
                .transpose()?;
            return super::regex_quantifier::convert(
                names,
                regex_node(names, atom)?,
                quantifier,
                separator,
            );
        }
        RegexNode::AnchorBeginningOfString => {
            (RakuAstClass::RegexAnchorBeginningOfString, Vec::new())
        }
        RegexNode::AnchorBeginningOfLine => (RakuAstClass::RegexAnchorBeginningOfLine, Vec::new()),
        RegexNode::AnchorEndOfString => (RakuAstClass::RegexAnchorEndOfString, Vec::new()),
        RegexNode::AnchorEndOfLine => (RakuAstClass::RegexAnchorEndOfLine, Vec::new()),
        RegexNode::AnchorLeftWordBoundary => {
            (RakuAstClass::RegexAnchorLeftWordBoundary, Vec::new())
        }
        RegexNode::MatchFrom => (RakuAstClass::RegexMatchFrom, Vec::new()),
        RegexNode::AssertionPass => (RakuAstClass::RegexAssertionPass, Vec::new()),
        RegexNode::AssertionFail => (RakuAstClass::RegexAssertionFail, Vec::new()),
        RegexNode::MatchTo => (RakuAstClass::RegexMatchTo, Vec::new()),
        RegexNode::AnchorRightWordBoundary => {
            (RakuAstClass::RegexAnchorRightWordBoundary, Vec::new())
        }
        RegexNode::CharClass(atom) => return Ok(super::regex_char_class::convert(atom)),
        RegexNode::CharClassAssertion(elements) => {
            return super::regex_enumeration::convert(names, elements);
        }
        RegexNode::InternalModifier {
            kind,
            long,
            negated,
        } => {
            let class = match kind {
                RegexModifierKind::IgnoreCase => RakuAstClass::RegexInternalModifierIgnoreCase,
                RegexModifierKind::IgnoreMark => RakuAstClass::RegexInternalModifierIgnoreMark,
                RegexModifierKind::Sigspace => RakuAstClass::RegexInternalModifierSigspace,
                RegexModifierKind::Ratchet => RakuAstClass::RegexInternalModifierRatchet,
            };
            let mut fields = Vec::new();
            if *long {
                fields.push(leaf_field(
                    Some("modifier"),
                    Value::str(kind.spellings().1.to_string()),
                ));
            }
            if *negated {
                fields.push(leaf_field(Some("negated"), Value::truth(true)));
            }
            (class, fields)
        }
        RegexNode::WithWhitespace(child)
            if matches!(
                **child,
                RegexNode::Extension(RegexExtension::VariableAlias { .. })
            ) =>
        {
            return super::regex_variable_alias::convert_spaced(names, child);
        }
        RegexNode::WithWhitespace(child) => (
            RakuAstClass::RegexWithWhitespace,
            vec![node_field(None, regex_node(names, child)?)],
        ),
    };
    Ok(RakuAstNode {
        origin: Default::default(),
        class,
        fields,
    })
}
