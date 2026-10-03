//! Named parameters across the RakuAST boundary, in both directions.
//!
//! Rakudo models every named parameter as one `RakuAST::Parameter` whose
//! `names` list holds each name it binds under, innermost first:
//!
//! | source                | `names`           | `target` |
//! | --------------------- | ----------------- | -------- |
//! | `:$a`                 | `("a",)`          | `$a`     |
//! | `:s(:$sort)`          | `("sort", "s")`   | `$sort`  |
//! | `:a(:b(:$c))`         | `("c", "b", "a")` | `$c`     |
//! | `:foo($bar)`          | `("foo",)`        | `$bar`   |
//! | `:x(:y($z))`          | `("y", "x")`      | `$z`     |
//!
//! The parser keeps an alias as a chain instead: the outer `ParamDef` is the
//! alias (`named_alias`), and its one-element `sub_signature` is the next
//! level, down to a named (`:$sort`) or positional (`$bar`) innermost
//! parameter. The type, default, `where` clause and required/optional marker
//! of the whole parameter sit on whichever level the source wrote them; in
//! RakuAST they are the one node's fields, and lowering puts them back on the
//! outermost level, where the binder checks them (measured on rakudo 2026.09).
//!
//! A named parameter that is *not* an alias but has a sub-signature
//! (`:$a ($x, $y)`) destructures; it is rendered with a `sub-signature` field
//! like a positional one.

use super::convert::{
    build_type_node, convert_expr, implicit_parameter_type, node_field, split_sigil,
    type_capture_name, type_capture_node, type_captures_field, unsupported,
};
use super::{RakuAstClass, RakuAstField, RakuAstFieldValue, RakuAstNode};
use crate::ast::{Expr, ParamDef};
use crate::value::{RuntimeError, Value};

/// The parts of a named parameter, read off the parser's alias chain.
struct Flat<'a> {
    /// Every name the parameter binds under, innermost first.
    names: Vec<String>,
    /// The innermost level: its name is the parameter's target.
    target: &'a ParamDef,
    type_constraint: Option<&'a str>,
    default: Option<&'a Expr>,
    where_constraint: Option<&'a Expr>,
}

/// Follow `pd`'s alias chain down to the parameter it binds.
fn flatten(pd: &ParamDef) -> Result<Flat<'_>, RuntimeError> {
    let mut aliases = Vec::new();
    let mut levels = vec![pd];
    let mut cur = pd;
    while cur.named && cur.named_alias {
        let Some([next]) = cur.sub_signature.as_deref() else {
            return Err(unsupported(
                "named alias without exactly one inner parameter",
            ));
        };
        aliases.push(cur.name.clone());
        levels.push(next);
        cur = next;
    }
    let target = cur;
    let mut names = Vec::with_capacity(aliases.len() + 1);
    if target.named {
        names.push(split_sigil(&target.name).1.to_string());
    }
    names.extend(aliases.into_iter().rev());
    let mut flat = Flat {
        names,
        target,
        type_constraint: None,
        default: None,
        where_constraint: None,
    };
    for (depth, level) in levels.iter().enumerate() {
        if depth > 0 && !inner_level_is_plain(level, std::ptr::eq(*level, target)) {
            return Err(unsupported("named alias with a marker on an inner level"));
        }
        if let Some(t) = level.type_constraint.as_deref()
            && type_capture_name(level).is_none()
        {
            set_once(&mut flat.type_constraint, t)?;
        }
        if let Some(d) = &level.default {
            set_once(&mut flat.default, d)?;
        }
        if let Some(w) = level.where_constraint.as_deref() {
            set_once(&mut flat.where_constraint, w)?;
        }
    }
    Ok(flat)
}

/// An inner level of an alias chain may carry a type, a default and a `where`
/// clause (they move to the parameter as a whole); any other marker has no
/// place in the one RakuAST node.
fn inner_level_is_plain(level: &ParamDef, is_target: bool) -> bool {
    !level.required
        && !level.optional_marker
        && !level.slurpy
        && !level.double_slurpy
        && !level.onearg
        && !level.sigilless
        && !level.is_invocant
        && level.literal_value.is_none()
        && level.traits.is_empty()
        && level.trait_args.is_empty()
        && type_capture_name(level).is_none()
        && level.shape_constraints.is_none()
        && level.code_signature.is_none()
        && level.outer_sub_signature.is_none()
        && (!is_target || level.sub_signature.is_none())
}

fn set_once<'a, T: ?Sized>(slot: &mut Option<&'a T>, value: &'a T) -> Result<(), RuntimeError> {
    if slot.is_some() {
        return Err(unsupported("named alias with a property on two levels"));
    }
    *slot = Some(value);
    Ok(())
}

/// The `RakuAST::Parameter` of the named parameter `pd`, every field but
/// `sub-signature` and `traits` (the caller appends those after it).
pub(super) fn named_parameter(
    pd: &ParamDef,
    type_setting: bool,
) -> Result<RakuAstNode, RuntimeError> {
    let flat = flatten(pd)?;
    if flat.names.is_empty() {
        return Err(unsupported("named alias without a name"));
    }
    let (sigil, desigil) = split_sigil(&flat.target.name);
    let mut fields = Vec::with_capacity(7);
    match flat.type_constraint {
        Some(t) => fields.push(node_field(Some("type"), build_type_node(t)?)),
        None => {
            if let Some(implicit) = implicit_parameter_type(sigil, type_setting) {
                fields.push(node_field(Some("type"), implicit));
            }
        }
    }
    fields.push(RakuAstField {
        name: Some("names"),
        value: RakuAstFieldValue::List(flat.names.into_iter().map(Value::str).collect()),
    });
    if let Some(name) = type_capture_name(pd) {
        fields.push(type_captures_field(type_capture_node(name)?));
    }
    fields.push(node_field(
        Some("target"),
        RakuAstNode {
            class: RakuAstClass::ParameterTargetVar,
            fields: vec![super::convert::leaf_field(
                Some("name"),
                Value::str(format!("{sigil}{desigil}")),
            )],
        },
    ));
    // A named parameter is optional unless marked `!`; rakudo writes the
    // field only when the source spelled a marker.
    match flat.default {
        Some(d) => fields.push(node_field(Some("default"), convert_expr(d)?)),
        None if pd.required || pd.optional_marker => fields.push(RakuAstField {
            name: Some("optional"),
            value: RakuAstFieldValue::Node(Value::truth(!pd.required)),
        }),
        None => {}
    }
    if let Some(w) = flat.where_constraint {
        fields.push(node_field(Some("where"), convert_expr(w)?));
    }
    Ok(RakuAstNode {
        class: RakuAstClass::Parameter,
        fields,
    })
}

/// The sub-signature `pd` destructures into, if any: an alias chain's
/// `sub_signature` is the chain itself, not a destructuring.
pub(super) fn destructuring_sub_signature(pd: &ParamDef) -> Option<&[ParamDef]> {
    if pd.named && pd.named_alias {
        None
    } else {
        pd.sub_signature.as_deref()
    }
}

/// Rebuild the parser's alias chain for a lowered named parameter. `def` is
/// the parameter as lowered from its fields, named after its target; `names`
/// is its `names` list.
pub(super) fn wrap_aliases(
    mut def: ParamDef,
    names: &[String],
    owner: &RakuAstNode,
) -> Result<ParamDef, RuntimeError> {
    let target_key = def.name.trim_start_matches(['@', '%', '&']).to_string();
    let target_is_named = names.first() == Some(&target_key);
    let aliases = if target_is_named { &names[1..] } else { names };
    def.named = true;
    let Some((outermost, inner_aliases)) = aliases.split_last() else {
        if names.is_empty() {
            return Err(super::lower::unsupported(owner));
        }
        return Ok(def);
    };
    if def.sigilless || def.sub_signature.is_some() {
        return Err(super::lower::unsupported(owner));
    }
    let mut inner = bare_level(&def.name, target_is_named, &def);
    for alias in inner_aliases {
        let mut level = bare_level(alias, true, &def);
        level.named_alias = true;
        level.sub_signature = Some(vec![inner]);
        inner = level;
    }
    def.name = outermost.clone();
    def.named_alias = true;
    def.sub_signature = Some(vec![inner]);
    Ok(def)
}

/// An inner level of an alias chain: no marker of its own, optional (the
/// outermost level says whether the parameter as a whole is required).
fn bare_level(name: &str, named: bool, like: &ParamDef) -> ParamDef {
    ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: like.multi_invocant,
        required: false,
        named,
        named_alias: false,
        slurpy: false,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        type_capture: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        trait_args: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
        block_param: like.block_param,
        code: Default::default(),
    }
}
