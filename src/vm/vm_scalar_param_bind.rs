//! How a `$`-sigiled parameter holds the value bound to it.
//!
//! Rakudo's binder (`lower_signature` in `Perl6/Actions.nqp`) gives a plain,
//! non-`is copy` `$` parameter a read-only Scalar wrapper only when its
//! nominal type could be Iterable (`nqp::istype($nomtype, Iterable) ||
//! nqp::istype(Iterable, $nomtype)`); otherwise the value "can't flatten" and
//! is bound decontainerized. `is raw` / `is rw` / sigilless parameters bind
//! the argument as passed. The three answers are [`ScalarParamBind`]:
//!
//! ```text
//! sub a($x)            { my @a = $x; @a.elems }   a([1,2]) -> 1  (Itemize)
//! sub b(Positional $x) { my @a = $x; @a.elems }   b([1,2]) -> 2  (Decont)
//! sub c($x is raw)     { my @a = $x; @a.elems }   c([1,2]) -> 2,
//!                                            c(my $s = [1,2]) -> 1  (Keep)
//! ```
//!
//! The binder applies the mode to the value; the compiler reads the same mode
//! (via [`Interpreter::scalar_param_bind`]) to know that `@a = $x` must not
//! re-itemize a Keep/Decont parameter by its `$` sigil alone.

use super::*;

/// What binding a parameter does to the incoming value's itemization.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum ScalarParamBind {
    /// A plain `$` parameter: the value is put in a (read-only) Scalar, so an
    /// Array/List/Hash argument is ONE item in list context.
    Itemize,
    /// Bound as passed: `@`/`%`/`&`, sigilless, `is raw` / `is rw`, invocants.
    Keep,
    /// A `$` parameter whose nominal type cannot be Iterable-related
    /// (`Positional $x`): the argument is decontainerized, so even an
    /// itemized `$[1, 2]` flattens.
    Decont,
}

impl Interpreter {
    /// Apply `pd`'s [`ScalarParamBind`] to an argument bound to it.
    ///
    /// An itemizing bind flips only the container kind over the same backing
    /// Gc, so in-place mutation through the param still reaches the caller's
    /// data; a decontainerizing one strips it the same way.
    ///
    /// An *invocant* parameter is exempt: `self` is bound raw, so
    /// `<a b c>.&(method (List:D:) { self.raku })` reports `("a", "b", "c")`,
    /// not the itemized `$("a", "b", "c")`.
    // Cost: O(1) (a trait scan of the declaration; the value flip shares its Gc).
    #[inline]
    pub(crate) fn itemize_plain_scalar_param(pd: &crate::ast::ParamDef, val: Value) -> Value {
        Self::apply_scalar_param_bind(Self::scalar_param_bind(pd), val)
    }

    /// [`Self::itemize_plain_scalar_param`] for a parameter of a compiled
    /// routine, answered from the precomputed `param_itemize_on_bind` mode.
    /// An index past the end means the chunk never ran the precompute (a
    /// hand-built one), which falls back to deriving the answer per bind.
    // Cost: O(1).
    #[inline]
    pub(crate) fn bind_itemize_param(
        cf: &crate::opcode::CompiledFunction,
        param_idx: usize,
        val: Value,
    ) -> Value {
        match cf.param_itemize_on_bind.get(param_idx) {
            Some(mode) => Self::apply_scalar_param_bind(*mode, val),
            None => Self::itemize_plain_scalar_param(&cf.param_defs[param_idx], val),
        }
    }

    #[inline]
    fn apply_scalar_param_bind(mode: ScalarParamBind, val: Value) -> Value {
        match mode {
            ScalarParamBind::Itemize => Self::itemize_scalar_store_value(val),
            ScalarParamBind::Keep => val,
            ScalarParamBind::Decont => match val.view() {
                // The `@`-sourced container share (`binding_signature.rs`)
                // boxes the argument in an itemized cell so in-body mutation
                // reaches the caller; keep the shared cell, drop the item flag.
                ValueView::ContainerRef(cell) if val.container_ref_is_itemized() => {
                    Value::container_ref(cell.clone())
                }
                _ => val.deitemize_for_sigil_bind(),
            },
        }
    }

    /// The [`ScalarParamBind`] of a parameter declaration.
    ///
    /// Depends only on the declaration -- its sigil, traits, name and nominal
    /// type -- never on the argument. A routine's signature is fixed, so
    /// `CompiledFunction::param_itemize_on_bind` settles it once at
    /// registration time; this is the definition that precompute (and the
    /// compiler's `@a = $x` itemization) uses, kept here so they never drift.
    // Cost: O(t + len(tc)), t = the parameter's trait count, tc = its type
    // constraint's spelling.
    pub(crate) fn scalar_param_bind(pd: &crate::ast::ParamDef) -> ScalarParamBind {
        let has_trait = |name: &str| pd.traits.iter().any(|t| t == name);
        if pd.sigilless
            || pd.is_invocant
            || has_trait("invocant")
            || pd.name.starts_with(['@', '%', '&'])
            || has_trait("raw")
            || has_trait("rw")
            // The name half of `itemize_scalar_store`'s own guard, folded in so
            // the precomputed mode answers the whole question.
            || Self::name_is_itemize_exempt(&pd.name)
        {
            return ScalarParamBind::Keep;
        }
        if !has_trait("copy")
            && pd
                .type_constraint
                .as_deref()
                .is_some_and(nominal_type_skips_item_wrap)
        {
            return ScalarParamBind::Decont;
        }
        ScalarParamBind::Itemize
    }
}

/// Does a `$` parameter whose nominal type is `tc` bind WITHOUT the read-only
/// Scalar wrapper?
///
/// Only the value kinds mutsu itemizes (Array/List/Hash/Seq/Slip/Range) can
/// observe the difference, and the Iterable-unrelated nominal types those
/// values satisfy are the three roles below: every other builtin either
/// relates to Iterable (`Any`, `Cool`, `List`, `Map`, ... wrap) or admits none
/// of those values. A `subset` of one of these roles still wraps: resolving a
/// subset's refinee needs the runtime type registry, which this
/// declaration-only predicate (precomputed per routine) does not have.
// Cost: O(len(tc)), tc = the constraint's spelling.
fn nominal_type_skips_item_wrap(tc: &str) -> bool {
    // `Positional:D`, `Positional[Int]`, `Positional(Any)` (a coercion whose
    // nominal target is `Positional`) all reduce to the base name.
    let base = tc.split(['(', '[', ':']).next().unwrap_or(tc);
    matches!(
        base,
        "Positional" | "Associative" | "PositionalBindFailover"
    )
}
