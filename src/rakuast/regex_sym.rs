//! `<name:sym<text>>` regex assertions (RakuAST ↔ the subrule's arguments).
//!
//! mutsu's regex parser reads the `:sym<text>` adverb of `<value:sym<number>>`
//! as one subrule argument, `sym<number>` (an angle `Index` of the bareword
//! `sym`). Rakudo keeps it on the assertion's name instead:
//! `Assertion::Named(name => Name.from-identifier("value", colonpairs =>
//! (ColonPair::Value(key => "sym", value => QuotedString<words val>("number")),)))`
//! with no `args` (measured on 2026.09). This module maps between the two.

use super::convert::{leaf_field, node_field, word_quote};
use super::lower::{
    leaf_str, list_field, named_child, positional_leaf, rakuast_node_of, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, IndexSpelling};
use crate::regex_tree::SubruleArgs;
use crate::value::{RuntimeError, Value};

/// The `colonpairs` field of the assertion name for arguments that are exactly
/// one `sym<text>` adverb, or `None` for any other arguments.
// Cost: O(k), k = length of the adverb text.
pub(super) fn name_colonpairs(args: &SubruleArgs) -> Option<RakuAstField> {
    let text = args.sym_adverb()?;
    let pair = RakuAstNode {
        class: RakuAstClass::ColonPairValue,
        fields: vec![
            leaf_field(Some("key"), Value::str("sym".to_string())),
            node_field(Some("value"), word_quote(&text)),
        ],
    };
    Some(RakuAstField {
        name: Some("colonpairs"),
        value: RakuAstFieldValue::List(vec![Value::rakuast(Box::new(pair))]),
    })
}

/// The arguments a name's `sym<text>` colonpair stands for, or `None` for a
/// name without colonpairs. The inverse of [`name_colonpairs`].
// Cost: O(k), k = length of the adverb text.
pub(super) fn args_from_name(name: &RakuAstNode) -> Result<Option<SubruleArgs>, RuntimeError> {
    if !name.fields.iter().any(|f| f.name == Some("colonpairs")) {
        return Ok(None);
    }
    let [pair] = list_field(name, "colonpairs")? else {
        return Err(unsupported(name));
    };
    let pair = rakuast_node_of(pair).ok_or_else(|| unsupported(name))?;
    if pair.class != RakuAstClass::ColonPairValue {
        return Err(unsupported(name));
    }
    let key = leaf_str(pair, "key")?;
    if key != "sym" {
        return Err(unsupported(name));
    }
    let quote = named_child(pair, "value")?;
    let text = match list_field(quote, "segments")? {
        [segment] => {
            let segment = rakuast_node_of(segment).ok_or_else(|| unsupported(name))?;
            if segment.class != RakuAstClass::StrLiteral {
                return Err(unsupported(name));
            }
            positional_leaf(segment)?.to_string_value()
        }
        _ => return Err(unsupported(name)),
    };
    Ok(Some(SubruleArgs {
        source: Some(format!("{key}<{text}>")),
        args: vec![Expr::Index {
            target: Box::new(Expr::BareWord(key)),
            index: Box::new(Expr::Literal(Value::str(text))),
            is_positional: false,
            spelling: IndexSpelling::Angle,
        }],
        literal_hash_indices: vec![false],
        colonpair_values: vec![false],
        colonpair_variables: vec![false],
        colonpair_trues: vec![false],
        colonpair_falses: vec![false],
    }))
}
