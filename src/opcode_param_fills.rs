//! Which parameters a light call may leave out, and what it binds for them.
//!
//! A `CompiledFunction`'s [`param_const_fills`](CompiledFunction::param_const_fills)
//! and [`light_required_positionals`](CompiledFunction::light_required_positionals)
//! are derived here, once per registration, from the signature alone. Keeping
//! the derivation next to the narrow rules it enforces -- and next to the test
//! that pins them -- is what stops a later widening from quietly admitting a
//! default the general binder would have evaluated differently.

use crate::ast::{Expr, ParamDef};
use crate::opcode::{CompiledFunction, FastParamCheck};
use crate::value::{Value, ValueView};

impl CompiledFunction {
    /// The constant this parameter binds when the call omits it, if there is one.
    ///
    /// Deliberately narrow. A default is an arbitrary expression that may read
    /// earlier parameters or close over the outer scope, and the general binder
    /// evaluates it with the parameter shadowed by its own type object
    /// (`eval_param_default`); nothing here may reproduce that. Only two shapes
    /// qualify:
    ///
    /// * a literal default (`$desc = ''`, `$n = 1`, `$x = Nil`) that is an
    ///   immutable scalar — a container literal is excluded because the general
    ///   binder evaluates the expression afresh per call and so hands every call
    ///   its own container, which a shared constant would not;
    /// * a bare `?` with no default, whose bound value is the pure
    ///   `missing_optional_param_value`.
    ///
    /// Anything carrying a trait, a `where`, a sub-signature, a type capture or
    /// a coercion answers `None` and keeps the routine on the general binder.
    pub(crate) fn const_fill_for_param(pd: &ParamDef) -> Option<Value> {
        if pd.named
            || pd.slurpy
            || pd.double_slurpy
            || pd.onearg
            || pd.sigilless
            || pd.is_invocant
            || pd.name.is_empty()
            || pd.where_constraint.is_some()
            || pd.sub_signature.is_some()
            || pd.outer_sub_signature.is_some()
            || pd.code_signature.is_some()
            || pd.shape_constraints.is_some()
            || pd.literal_value.is_some()
            || !pd.traits.is_empty()
            // An `@`/`%` parameter's missing-optional value is a FRESH
            // container per call (`Value::element_descriptor_array`), which a
            // shared constant cannot be. The light path's eligibility check
            // excludes these sigils anyway; excluding them here keeps this
            // function correct on its own terms rather than by that coincidence.
            || pd.name.starts_with('@')
            || pd.name.starts_with('%')
        {
            return None;
        }
        // A `::T` type capture binds the capture, not the parameter, so it
        // stays on the general binder.
        if pd
            .type_constraint
            .as_deref()
            .is_some_and(|tc| tc.starts_with("::"))
        {
            return None;
        }
        let value = match &pd.default {
            Some(Expr::Literal(v)) if Self::is_immutable_scalar_literal(v) => v.clone(),
            Some(_) => return None,
            None if pd.optional_marker => {
                crate::runtime::Interpreter::missing_optional_param_value(pd)
            }
            None => return None,
        };
        // The filled value has to satisfy the parameter's own constraint, or the
        // light path would bind something the general binder rejects. Verifying
        // it here (once) rather than per call is what lets the bind loop skip
        // type-checking filled slots entirely. A constraint the fast check
        // cannot classify answers `None`, which keeps the routine off the light
        // path -- the same conservative direction as an outright failure.
        //
        // Note this legitimately rejects `Int:D $x?`: an unsupplied optional
        // binds its own (undefined) type object, which `:D` refuses. Rakudo
        // rejects that signature outright; mutsu just keeps it on the general
        // binder rather than deciding the question here.
        match FastParamCheck::of(pd.type_constraint.as_ref()) {
            Some(FastParamCheck::Unconstrained) => Some(value),
            Some(FastParamCheck::Fast { kind, name_sym }) => {
                match crate::runtime::Interpreter::fast_type_check_tagged(&value, kind, name_sym) {
                    true => Some(value),
                    false => None,
                }
            }
            None => None,
        }
    }

    /// A literal whose value can be shared by every call that omits its
    /// parameter: an immutable scalar with no interior mutability and no
    /// container identity a caller could observe or mutate. A container literal
    /// (`$x = []`) is excluded on purpose — the general binder re-evaluates the
    /// expression per call and so hands every call a *fresh* container, which a
    /// shared constant would not.
    pub(crate) fn is_immutable_scalar_literal(v: &Value) -> bool {
        matches!(
            v.view(),
            ValueView::Int(_)
                | ValueView::Num(_)
                | ValueView::Str(_)
                | ValueView::Bool(_)
                | ValueView::Rat(..)
        )
    }

    /// See [`Self::light_required_positionals`]. Takes the two inputs rather
    /// than `&self` so the rule can be exercised without building a whole
    /// `CompiledFunction`.
    pub(crate) fn compute_light_required_positionals(
        param_defs: &[ParamDef],
        param_const_fills: &[Option<Value>],
    ) -> Option<usize> {
        let mut required = 0usize;
        let mut seen_optional = false;
        for (i, pd) in param_defs.iter().enumerate() {
            let optional = pd.default.is_some() || pd.optional_marker;
            if optional {
                // An optional parameter the fill table cannot represent keeps
                // the whole routine on the general binder.
                param_const_fills.get(i)?.as_ref()?;
                seen_optional = true;
            } else {
                // Raku puts every required positional ahead of every optional
                // one; a signature that does not is not one this plan can
                // serve, because the light bind fills a *suffix*.
                if seen_optional {
                    return None;
                }
                required += 1;
            }
        }
        Some(required)
    }
}

#[cfg(test)]
mod param_fill_tests {
    use super::*;

    fn param(name: &str) -> ParamDef {
        ParamDef {
            type_capture: None,
            name: name.to_string(),
            default: None,
            multi_invocant: false,
            required: true,
            named: false,
            named_alias: false,
            slurpy: false,
            double_slurpy: false,
            onearg: false,
            sigilless: false,
            type_constraint: None,
            literal_value: None,
            sub_signature: None,
            where_constraint: None,
            traits: Vec::new(),
            optional_marker: false,
            outer_sub_signature: None,
            code_signature: None,
            is_invocant: false,
            shape_constraints: None,
            block_param: false,
        }
    }

    /// A literal default and a bare `?` are the only two shapes that reduce to a
    /// constant. Everything else has to keep answering `None`, because the
    /// general binder does something a shared constant cannot reproduce.
    #[test]
    fn only_constant_shapes_get_a_fill() {
        // A literal default: the constant itself.
        let mut pd = param("$y");
        pd.default = Some(Expr::Literal(Value::int(5)));
        assert_eq!(
            CompiledFunction::const_fill_for_param(&pd),
            Some(Value::int(5))
        );

        // A bare `?`: the parameter's own (undefined) type object.
        let mut pd = param("$y");
        pd.optional_marker = true;
        assert!(CompiledFunction::const_fill_for_param(&pd).is_some());

        // A mandatory parameter is not fillable at all.
        assert!(CompiledFunction::const_fill_for_param(&param("$y")).is_none());

        // A non-constant default must be evaluated per call.
        let mut pd = param("$y");
        pd.default = Some(Expr::Var("x".to_string()));
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // A container literal must be a FRESH container per call.
        let mut pd = param("$y");
        pd.default = Some(Expr::Literal(Value::array(vec![Value::int(1)])));
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // A `where` is enforced against the bound value by the general binder.
        let mut pd = param("$y");
        pd.default = Some(Expr::Literal(Value::int(5)));
        pd.where_constraint = Some(Box::new(Expr::Literal(Value::TRUE)));
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // A trait (`is copy`, `is rw`) changes what binding means.
        let mut pd = param("$y");
        pd.default = Some(Expr::Literal(Value::int(5)));
        pd.traits = vec!["copy".to_string()];
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // An `@`/`%` optional binds a fresh anonymous container per call.
        let mut pd = param("@y");
        pd.optional_marker = true;
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // A default that does not satisfy its own declared constraint keeps the
        // routine on the general binder, which reports the mismatch.
        let mut pd = param("$y");
        pd.type_constraint = Some("Int".to_string());
        pd.default = Some(Expr::Literal(Value::str("no".to_string())));
        assert!(CompiledFunction::const_fill_for_param(&pd).is_none());

        // ...and one that does satisfy it is admitted.
        let mut pd = param("$y");
        pd.type_constraint = Some("Int".to_string());
        pd.default = Some(Expr::Literal(Value::int(5)));
        assert!(CompiledFunction::const_fill_for_param(&pd).is_some());
    }

    /// `light_required_positionals` counts the mandatory PREFIX, and refuses a
    /// signature the suffix-filling bind loop cannot serve.
    #[test]
    fn required_prefix_is_counted_and_out_of_order_signatures_are_refused() {
        let with = |defs: Vec<ParamDef>| {
            let fills: Vec<Option<Value>> = defs
                .iter()
                .map(CompiledFunction::const_fill_for_param)
                .collect();
            CompiledFunction::compute_light_required_positionals(&defs, &fills)
        };

        let opt = |name: &str| {
            let mut pd = param(name);
            pd.default = Some(Expr::Literal(Value::int(1)));
            pd
        };
        let nonconst = |name: &str| {
            let mut pd = param(name);
            pd.default = Some(Expr::Var("x".to_string()));
            pd
        };

        assert_eq!(with(vec![param("$a"), param("$b")]), Some(2));
        assert_eq!(with(vec![param("$a"), opt("$b")]), Some(1));
        assert_eq!(with(vec![opt("$a"), opt("$b")]), Some(0));
        assert_eq!(with(vec![]), Some(0));
        // A non-constant default anywhere disqualifies the whole signature.
        assert_eq!(with(vec![param("$a"), nonconst("$b")]), None);
        // A required parameter AFTER an optional one cannot be suffix-filled.
        assert_eq!(with(vec![opt("$a"), param("$b")]), None);
    }
}
