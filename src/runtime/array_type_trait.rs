//! `is array_type(T)`: the element type a class's HOW records (#11310).
//!
//! In rakudo `array_type` is a core trait, `multi trait_mod:<is>(Mu:U $type,
//! :$array_type!) { $type.^set_array_type($array_type) }`, and every class
//! and role HOW answers `.^set_array_type` / `.^array_type`. Upstream
//! `NativeCall::Types` declares its typed arrays with it
//! (`class CArray is repr('CArray') is array_type(Pointer)`, and the
//! parametric `role IntTypedCArray[::TValue] ... is array_type(TValue)`), so
//! the trait has to be core here too: before, mutsu dropped it, and once any
//! user `trait_mod:<is>` candidate was in scope it was dispatched to that
//! candidate instead and failed.
//!
//! A role's trait is applied to whatever composes the role, with the role's
//! type arguments bound, unless the class declared its own:
//!
//! ```raku
//! role R[::T] is array_type(T) { }
//! class C is repr('CArray') does R[int32] { }   # C.^array_type: int32
//! ```

use super::*;
use crate::opcode::DeclTraitArg;

/// Whether `name` is the `array_type` trait, which is handled here and never
/// dispatched to a user `trait_mod:<is>`.
// Cost: O(1).
pub(crate) fn is_array_type_trait(name: &str) -> bool {
    name == "array_type"
}

/// The `is array_type(...)` argument among a declaration's traits.
// Cost: O(t), t = traits on the declaration.
fn array_type_arg(traits: &[(String, Option<DeclTraitArg>)]) -> Option<&DeclTraitArg> {
    traits
        .iter()
        .find(|(name, _)| is_array_type_trait(name))
        .and_then(|(_, arg)| arg.as_ref())
}

impl Interpreter {
    /// Record a class's own `is array_type(T)`.
    // Cost: O(t), t = traits on the declaration, plus evaluating the argument.
    pub(crate) fn apply_class_array_type_trait(
        &mut self,
        class_name: &str,
        traits: &[(String, Option<DeclTraitArg>)],
    ) -> Result<(), RuntimeError> {
        let Some(arg) = array_type_arg(traits) else {
            return Ok(());
        };
        let value = self.eval_decl_trait_arg(arg)?;
        self.set_array_type(class_name, value);
        Ok(())
    }

    /// Keep a role's `is array_type(...)` argument unevaluated: it may name
    /// the role's own type parameter, which only a composition binds.
    // Cost: O(t), t = traits on the declaration.
    pub(crate) fn record_role_array_type_trait(
        &mut self,
        role_name: &str,
        traits: &[(String, Option<DeclTraitArg>)],
    ) {
        if let Some(arg) = array_type_arg(traits) {
            self.registry_mut()
                .role_array_type_args
                .insert(role_name.to_string(), arg.clone());
        }
    }

    /// Apply the `is array_type(...)` of role `role_name`, composed with type
    /// arguments `arg_values` for its parameters `param_names`, to
    /// `class_name` -- unless the class recorded one of its own.
    // Cost: O(p), p = role parameters, plus evaluating the argument.
    pub(crate) fn compose_role_array_type(
        &mut self,
        class_name: &str,
        role_name: &str,
        param_names: &[String],
        arg_values: &[Value],
    ) -> Result<(), RuntimeError> {
        if self.registry().array_types.contains_key(class_name) {
            return Ok(());
        }
        if let Some(value) = self.role_array_type(role_name, param_names, arg_values)? {
            self.set_array_type(class_name, value);
        }
        Ok(())
    }

    /// The `is array_type(...)` of role `role_name` with its parameters
    /// `param_names` bound to `arg_values`; `None` when the role declares none.
    // Cost: O(p), p = role parameters, plus evaluating the argument.
    pub(crate) fn role_array_type(
        &mut self,
        role_name: &str,
        param_names: &[String],
        arg_values: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let Some(arg) = self.registry().role_array_type_args.get(role_name).cloned() else {
            return Ok(None);
        };
        // `eval_decl_trait_arg_with_captured_env` only fills names the
        // running scope does not already bind, so a composing scope's own
        // `T` would win over the role's. Bind the role's arguments over it
        // and put the scope's values back afterwards.
        let shadowed: Vec<(String, Option<Value>)> = param_names
            .iter()
            .zip(arg_values)
            .map(|(name, value)| (name.clone(), self.env.insert(name.clone(), value.clone())))
            .collect();
        let result = self.eval_decl_trait_arg(&arg);
        for (name, previous) in shadowed {
            match previous {
                Some(value) => {
                    self.env.insert(name, value);
                }
                None => {
                    self.env.remove(&name);
                }
            }
        }
        result.map(Some)
    }

    /// `.^array_type` of type `ty`. A mixin type object
    /// (`CArray.^mixin(IntTypedCArray[int32])`, what upstream
    /// `NativeCall::Types`' `^parameterize` builds) is a fresh subtype that
    /// declares nothing itself, so a composed role's trait answers before the
    /// base class's own -- rakudo's role applier sets the trait on the mixin
    /// type (#11209). `None` when nothing recorded one.
    // Cost: O(r), r = roles mixed in, each resolving its candidate and evaluating its trait.
    pub(crate) fn type_array_type(&mut self, ty: &Value) -> Result<Option<Value>, RuntimeError> {
        if let ValueView::Mixin(inner, mixins) = ty.view() {
            // The last role composed wins, as each application re-sets it.
            for (role_name, args) in Self::mixin_role_applications(mixins).into_iter().rev() {
                if !self
                    .registry()
                    .role_array_type_args
                    .contains_key(role_name.as_str())
                {
                    continue;
                }
                let args = args.unwrap_or_default();
                let (param_names, arg_values) =
                    match self.resolve_role_candidate_with_args(&role_name, Some(&args))? {
                        Some((_, names, values)) => (names, values),
                        None => (Vec::new(), Vec::new()),
                    };
                if let Some(value) = self.role_array_type(&role_name, &param_names, &arg_values)? {
                    return Ok(Some(value));
                }
            }
            return self.type_array_type(inner);
        }
        let owner = self.mop_receiver_owner(ty);
        Ok(self.recorded_array_type(&owner))
    }

    /// `.^set_array_type(T)`.
    // Cost: O(1) amortized.
    pub(crate) fn set_array_type(&mut self, type_name: &str, value: Value) {
        self.registry_mut()
            .array_types
            .insert(type_name.to_string(), value);
    }

    /// The `.^array_type` a class recorded, if any.
    // Cost: O(1).
    pub(crate) fn recorded_array_type(&self, type_name: &str) -> Option<Value> {
        self.registry().array_types.get(type_name).cloned()
    }
}
