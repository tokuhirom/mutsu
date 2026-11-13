//! The value an omitted optional parameter binds, and the constraint check
//! Rakudo runs against it.

use super::*;

/// A subset may refine another subset; this bounds the walk to the refinee
/// against a (malformed) cyclic registry entry.
const MAX_SUBSET_DEPTH: usize = 64;

impl Interpreter {
    /// The value an unpassed optional parameter (no default) binds: its
    /// implicit default, the parameter's *nominal* type object.
    ///
    /// For a subset-typed parameter that is the subset's refinee, not the
    /// subset itself: Rakudo binds `Str` for an omitted `S :$s` with `subset S
    /// of Str where ...` (and `Int` for `UInt $x?`) in every language
    /// version, then checks the subset's predicate against it
    /// ([`Self::check_omitted_optional_subset`]). Binding the subset's own type
    /// object instead made that check vacuous, because `S` trivially is an
    /// `S`. (A `my S $v` *variable* is different: before 6.e it does hold the
    /// subset type object -- `nominal_type_object_name_for_constraint`.)
    // Cost: O(d), d = depth of the subset refinement chain (registry lookups).
    pub(crate) fn omitted_optional_param_value(&self, pd: &ParamDef) -> Value {
        let value = Self::missing_optional_param_value(pd);
        if pd.name.starts_with(['@', '%', '&']) {
            return value;
        }
        let ValueView::Package(name) = value.view() else {
            return value;
        };
        let mut name = name.resolve();
        let mut changed = false;
        // A nested-name type of the declaring package, spelled short in the
        // signature (`Window::Clauses $w?` for `Pk::Window::Clauses`), was not
        // registered yet when the routine was hoisted: resolve it now, against
        // the package the routine runs in (#16359).
        if crate::qualified::is_qualified_str(&name) && !self.has_type_direct(&name) {
            let resolved = self.resolve_method_type_name(&self.current_package(), &name);
            if resolved != name {
                name = resolved;
                changed = true;
            }
        }
        for _ in 0..MAX_SUBSET_DEPTH {
            let refinee = if name == "UInt" {
                "Int".to_string()
            } else {
                let registry = self.registry();
                if registry.subsets.is_empty() {
                    break;
                }
                match registry.subsets.get(&name) {
                    Some(def) => def.base.clone(),
                    None => break,
                }
            };
            name = Self::optional_type_object_name(&refinee);
            changed = true;
        }
        // A lexical class or role (`my class TOC::Calc`) is registered under
        // its mangled storage name (`TOC::Calc\0<decl-id>`, ADR-0047), which
        // the scope's env binds the source name to. The omitted value is that
        // type object, as when the source names the type: a type object built
        // from the bare name names no registered type, so `$toc.new` on it
        // could not find the class (Pod::To::HTML's `TOC::Calculator :$toc`).
        // A routine of a module binds its parameters in an env that does not
        // hold the module's lexical aliases (the call comes from another
        // unit), so the spelling is then resolved against the registry -- but
        // only when exactly one lexical type carries it, as the type check
        // does (`type_matches_value`): which of several same-named ones was
        // meant is not recoverable here.
        // TODO: resolve a parameter's type in its declaring scope when the
        // routine is registered, so same-named lexical types in different
        // scopes each give their own routine's omitted value.
        if !self.has_type_direct(&name) {
            if let Some(lexical) = self.resolve_type_object(&name)
                && matches!(lexical.view(), ValueView::Package(p) if p.resolve() != name)
            {
                return lexical;
            }
            if let Some(key) = self.unique_lexical_type_key(&name) {
                return Value::package(Symbol::intern(&key));
            }
        }
        // A package-relative type (`Column::List` written inside `unit class
        // SQ`, declared as `SQ::Column::List`) binds the declared type's
        // object, so the omitted value passes a typed attribute's check.
        if crate::qualified::is_qualified_str(&name)
            && !self.has_type_direct(&name)
            && let Some(qualified) = self.resolve_relative_compound_type(&name)
        {
            return Value::package(Symbol::intern(qualified));
        }
        if changed {
            Value::package(Symbol::intern(&name))
        } else {
            value
        }
    }

    /// Check an omitted optional's implicit default (see
    /// [`Self::omitted_optional_param_value`]) against the parameter's subset
    /// constraint, as Rakudo does: `sub f(S :$s) {}; f()` dies with
    /// "Constraint type check failed ... expected S but got Str (Str)" when
    /// `S`'s predicate rejects the `Str` type object. A non-subset nominal
    /// type always accepts its own type object, so only a subset can fail.
    /// The definedness smiley is not checked here: whether `Int:D $x?` may be
    /// omitted is a separate question this does not decide.
    // Cost: O(1) plus the subset predicate's own body.
    pub(crate) fn check_omitted_optional_subset(
        &mut self,
        pd: &ParamDef,
        value: &Value,
    ) -> Result<(), RuntimeError> {
        if pd.name.starts_with(['@', '%', '&']) {
            return Ok(());
        }
        let Some(constraint) = &pd.type_constraint else {
            return Ok(());
        };
        let (base, _) = strip_type_smiley(constraint);
        if base != "UInt" && !self.is_subset_type_name(base) {
            return Ok(());
        }
        if self.type_matches_value(base, value) {
            return Ok(());
        }
        Err(self
            .subset_constraint_binding_error(pd, &pd.name, constraint, base, value)
            .with_omitted_optional_note())
    }

    /// Rakudo's "Constraint type check failed" binding error for a value of a
    /// subset's refinee type that the subset's predicate rejected: "...in
    /// binding to parameter '$x'; expected Even but got Int (3)". The expected
    /// type is named by `base`, the constraint without its smiley; the value is
    /// spelled as `.raku` does (`Str ("a")`, `Str (Str)`), like the `where`
    /// failure (`typecheck_binding_parameter_where`).
    pub(crate) fn subset_constraint_binding_error(
        &self,
        pd: &ParamDef,
        display_name: &str,
        constraint: &str,
        base: &str,
        value: &Value,
    ) -> RuntimeError {
        let got = crate::runtime::utils::got_type_name(value);
        RuntimeError::typecheck_binding_parameter(
            display_name,
            constraint,
            &got,
            Some(format!(
                "Constraint type check failed in binding to parameter '{}'; {}",
                param_display_name(pd),
                crate::word_wrap::naive_word_wrap(
                    &format!(
                        "expected {} but got {} ({})",
                        base,
                        got,
                        crate::runtime::utils::constraint_value_repr(value)
                    ),
                    72
                ),
            )),
        )
        .with_parameter_object(pd, Some(self))
    }
}
