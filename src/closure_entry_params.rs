//! The declaration half of the closure entry bind (#18267): what the general
//! signature binder derives from each `ParamDef` of a plain positional
//! signature, settled once per code object. It lives below the VM so the
//! `SubData` signature memo (`value::ParamNameSyms`) can hold it; the per-call
//! half is `vm::vm_closure_entry_bind`.

use crate::ast::{ParamDef, ReadonlyKind};
use crate::light_closure_shape::is_plain_scalar_param_name;
use crate::meta_ns::MetaNs;
use crate::symbol::Symbol;

/// What binding one parameter does, settled from its `ParamDef`.
#[derive(Debug)]
pub(crate) enum EntryParamKind {
    /// A positional `$` scalar, untyped: an item binding, or (`is raw`) the
    /// value as passed, with the readonly mark the general binder's readonly
    /// pass leaves on it.
    Scalar { raw: bool, readonly: ReadonlyKind },
    /// A sigilless `\x`: bound as passed under its term key, with the two
    /// sigilless meta keys the general binder writes for a value binding.
    Sigilless {
        alias_key: Symbol,
        readonly_key: Symbol,
    },
}

/// One parameter of a signature [`entry_params`] accepted.
#[derive(Debug)]
pub(crate) struct EntryParam {
    /// The env key the parameter is stored under: the name for a `$`
    /// parameter, the term key for a sigilless one.
    pub(crate) storage: Symbol,
    /// `ParamDef::name`, for the `&`-sigiled alias of a `Callable`.
    pub(crate) name: Box<str>,
    pub(crate) kind: EntryParamKind,
    /// The parameter is implicitly `Any` (untyped); `false` for `Mu \x`.
    pub(crate) implicit_any: bool,
}

/// The [`EntryParam`]s of `param_defs`, or `None` when the entry bind may not
/// serve this signature.
// Cost: O(p + t), p = parameters, t = their total trait count.
pub(crate) fn entry_params(param_defs: &[ParamDef]) -> Option<Box<[EntryParam]>> {
    if param_defs.is_empty() {
        return None;
    }
    param_defs.iter().map(entry_param).collect()
}

// Cost: O(t + n), t = the parameter's trait count, n = its name length.
fn entry_param(pd: &ParamDef) -> Option<EntryParam> {
    let plain_positional = !pd.named
        && !pd.named_alias
        && !pd.slurpy
        && !pd.double_slurpy
        && !pd.onearg
        && pd.default.is_none()
        && pd.type_capture.is_none()
        && pd.literal_value.is_none()
        && pd.sub_signature.is_none()
        && pd.where_constraint.is_none()
        && pd.trait_args.is_empty()
        && !pd.optional_marker
        && pd.outer_sub_signature.is_none()
        && pd.code_signature.is_none()
        && !pd.is_invocant
        && !pd.does_target
        && pd.shape_constraints.is_none()
        && !pd.block_param;
    if !plain_positional || !is_plain_scalar_param_name(&pd.name) || pd.declares_self_lexical() {
        return None;
    }
    if pd.sigilless {
        // `\x` or `Mu \x`; no trait (a sigilless parameter is raw already).
        if !pd.traits.is_empty() || !matches!(pd.type_constraint.as_deref(), None | Some("Mu")) {
            return None;
        }
        let storage = crate::term_names::term_key_sym(Symbol::intern(&pd.name));
        return Some(EntryParam {
            storage,
            name: pd.name.as_str().into(),
            kind: EntryParamKind::Sigilless {
                alias_key: MetaNs::SigillessAlias.key(storage),
                readonly_key: MetaNs::SigillessReadonly.key(storage),
            },
            implicit_any: pd.type_constraint.is_none(),
        });
    }
    // A `$` scalar: untyped, with no trait but `is raw`.
    let raw = match pd.traits.as_slice() {
        [] => false,
        [t] if t == "raw" => true,
        _ => return None,
    };
    if pd.type_constraint.is_some() {
        return None;
    }
    Some(EntryParam {
        storage: Symbol::intern(&pd.name),
        name: pd.name.as_str().into(),
        kind: EntryParamKind::Scalar {
            raw,
            readonly: if raw {
                ReadonlyKind::Immutable
            } else {
                ReadonlyKind::Alias
            },
        },
        implicit_any: true,
    })
}
