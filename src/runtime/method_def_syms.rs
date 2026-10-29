//! The interned names a [`MethodDef`](super::MethodDef) dispatch needs.
//!
//! A method's parameter names and its source file never change once the
//! method is declared, yet the compiled-method fast path used to hand each of
//! them to [`Symbol::intern`] on every call — a thread-local string-hash probe
//! per name per call (#10961). [`MethodDefSyms`] interns them once, on the
//! first dispatch, and every later call (the def lives behind an `Arc` in the
//! method resolve caches) reads the stored `Symbol`s.
//!
//! The cache is filled lazily rather than at each of the many `MethodDef`
//! construction sites, so a site cannot forget it. That relies on the cached
//! fields never being reassigned after construction — nothing does today; a
//! new mutation of `params`, `param_defs[..].name` or `source_file` on an
//! existing def must build a fresh `MethodDefSyms::default()` alongside it.

use crate::symbol::Symbol;
use std::sync::OnceLock;

#[derive(Debug, Clone, Default)]
pub(crate) struct MethodDefSyms {
    params: OnceLock<Box<[Symbol]>>,
    param_storage: OnceLock<Box<[String]>>,
    param_def_names: OnceLock<Box<[Symbol]>>,
    source_file: OnceLock<Option<Symbol>>,
    named_fast: OnceLock<Box<[Option<NamedFast>]>>,
}

/// What the compiled-method fast path needs to bind a plain named scalar
/// parameter (`Num :$value = 1e0`) without the general binder: its type check,
/// and the value an unsupplied call binds.
#[derive(Debug, Clone)]
pub(crate) struct NamedFast {
    pub(crate) check: crate::opcode::FastParamCheck,
    /// The constant default, already verified against `check`; `None` keeps
    /// an unsupplied call on the general binder.
    pub(crate) fill: Option<crate::value::Value>,
}

impl super::MethodDef {
    /// The storage key of `params[i]`: its name, or the term key of a
    /// sigilless parameter (ADR-12441), whose `param_defs[i]` says so.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn param_storage_names(&self) -> &[String] {
        self.syms.param_storage.get_or_init(|| {
            self.params
                .iter()
                .enumerate()
                .map(|(i, n)| match self.param_defs.get(i) {
                    Some(pd) if pd.name == *n => pd.storage_name().into_owned(),
                    _ => n.clone(),
                })
                .collect()
        })
    }

    /// [`Self::param_storage_names`], interned.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn param_syms(&self) -> &[Symbol] {
        self.syms.params.get_or_init(|| {
            self.param_storage_names()
                .iter()
                .map(|n| Symbol::intern(n))
                .collect()
        })
    }

    /// `param_defs[i].storage_name()`, interned.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn param_def_name_syms(&self) -> &[Symbol] {
        self.syms.param_def_names.get_or_init(|| {
            self.param_defs
                .iter()
                .map(|pd| Symbol::intern(&pd.storage_name()))
                .collect()
        })
    }

    /// The fast-path plan for `param_defs[i]` when it is a plain named scalar
    /// parameter whose type check and default the fast binder can answer
    /// itself; `None` for every other parameter.
    // Cost: O(1) after the first call; O(p) once, p = number of parameters.
    #[inline]
    pub(crate) fn named_fast(&self, i: usize) -> Option<&NamedFast> {
        self.syms
            .named_fast
            .get_or_init(|| self.param_defs.iter().map(NamedFast::of_param).collect())
            .get(i)
            .and_then(Option::as_ref)
    }

    /// `source_file`, interned.
    // Cost: O(1) after the first call.
    #[inline]
    pub(crate) fn source_file_sym(&self) -> Option<Symbol> {
        *self
            .syms
            .source_file
            .get_or_init(|| self.source_file.as_deref().map(Symbol::intern))
    }
}

impl NamedFast {
    // Cost: O(1).
    pub(crate) fn of_param(pd: &crate::ast::ParamDef) -> Option<Self> {
        use crate::opcode::{CompiledFunction, FastParamCheck};
        if !pd.named
            || pd.has_traits()
            || pd.slurpy
            || pd.double_slurpy
            || pd.onearg
            || pd.sigilless
            || pd.is_invocant
            || pd.required
            || pd.where_constraint.is_some()
            || pd.sub_signature.is_some()
            || pd.outer_sub_signature.is_some()
            || pd.code_signature.is_some()
            || pd.literal_value.is_some()
            || pd.shape_constraints.is_some()
            || pd.captured_type_name().is_some()
            || pd.name == "_"
            || pd.name == "self"
            || crate::value::attr_twigil_base(&pd.name).is_some()
            || !pd
                .name
                .as_bytes()
                .first()
                .is_some_and(|b| b.is_ascii_alphabetic() || *b == b'_')
        {
            return None;
        }
        // A native (`int`, `num`, ...) constraint binds a wrapped value, which
        // only the general binder produces.
        if pd
            .type_constraint
            .as_deref()
            .is_some_and(|tc| !tc.as_bytes().first().is_some_and(u8::is_ascii_uppercase))
        {
            return None;
        }
        let check = FastParamCheck::of_param(pd)?;
        if matches!(check, FastParamCheck::ImplicitCallable) {
            return None;
        }
        let fill = pd
            .default
            .as_ref()
            .and_then(CompiledFunction::constant_default_value)
            .filter(|value| match check {
                FastParamCheck::Unconstrained | FastParamCheck::RequiresAny => true,
                FastParamCheck::ImplicitCallable => false,
                FastParamCheck::Fast { kind, name_sym } => {
                    crate::runtime::Interpreter::fast_type_check_tagged(value, kind, name_sym)
                }
            });
        Some(Self { check, fill })
    }

    /// Whether the supplied `value` satisfies the parameter's type, as far as
    /// the fast path can tell without an interpreter.
    // Cost: O(1).
    pub(crate) fn accepts(&self, value: &crate::value::Value) -> bool {
        use crate::opcode::FastParamCheck;
        match self.check {
            // A `Mu`-only argument to an untyped parameter is refused by the
            // caller's `named_mu_only_arg` gate.
            FastParamCheck::Unconstrained | FastParamCheck::RequiresAny => true,
            FastParamCheck::ImplicitCallable => false,
            FastParamCheck::Fast { kind, name_sym } => {
                crate::runtime::Interpreter::fast_type_check_tagged(value, kind, name_sym)
            }
        }
    }
}
