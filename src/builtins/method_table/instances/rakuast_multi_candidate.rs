//! `meta-object` of a `multi` `RakuAST::Method` / `RakuAST::Sub`: the candidate
//! the declaration installed, not the dispatcher its name finds (#16888, EEL
//! 0.3's `multi pattern` declarator mixes a role into each candidate).
//!
//! The node keeps no link to the routine object, so the candidate is the one
//! whose written parameters (names, types, kinds) are the node's; the implicit
//! invocant and a method's implicit `*%_` are not written in either.

use crate::ast::ParamDef;
use crate::runtime::Interpreter;
use crate::value::{Value, ValueView};

/// What identifies a written parameter for the match.
type ParamKey<'a> = (&'a str, Option<&'a str>, bool, bool, bool, bool);

// Cost: O(p), p = parameters.
fn written_keys(defs: &[ParamDef]) -> Vec<ParamKey<'_>> {
    defs.iter()
        .filter(|p| !p.is_invocant && !p.implicit_invocant)
        .filter(|p| !((p.slurpy || p.double_slurpy) && p.name == "%_"))
        .map(|p| {
            (
                p.name.as_str(),
                p.type_constraint.as_deref(),
                p.named,
                p.slurpy,
                p.double_slurpy,
                p.sigilless,
            )
        })
        .collect()
}

/// The candidate of `dispatcher` whose written signature is `node`'s, when the
/// node is a `multi` routine; `None` otherwise or when none matches.
// Cost: O(c * p), c = candidates, p = parameters.
pub(super) fn multi_candidate(
    interp: &mut Interpreter,
    dispatcher: &Value,
    node: &crate::rakuast::RakuAstNode,
) -> Option<Value> {
    if crate::rakuast::routine_multiness(node).as_deref() != Some("multi") {
        return None;
    }
    let wanted = crate::rakuast::routine_param_defs(node)?;
    let wanted = written_keys(&wanted);
    let candidates = interp
        .call_method_with_values(dispatcher.clone(), "candidates", Vec::new())
        .ok()?;
    let items: Vec<Value> = match candidates.view() {
        ValueView::Array(items, _) => items.as_slice().to_vec(),
        _ => return None,
    };
    items.into_iter().find(|candidate| {
        let Some(info) = candidate_signature(interp, candidate) else {
            return false;
        };
        info.param_defs
            .as_ref()
            .is_some_and(|defs| written_keys(defs) == wanted)
    })
}

/// The signature of a candidate: a `Sub`'s own, or a `Method` object's.
// Cost: O(p), p = parameters.
fn candidate_signature(
    interp: &mut Interpreter,
    candidate: &Value,
) -> Option<crate::value::signature::SigInfo> {
    let signature = interp
        .call_method_with_values(candidate.clone(), "signature", Vec::new())
        .ok()?;
    crate::value::signature::extract_sig_info(&signature)
}
