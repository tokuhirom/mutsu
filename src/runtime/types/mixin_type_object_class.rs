//! The class a mixin type object constructs.
//!
//! `Base but R` on a type object is, in Rakudo, the class `Base+{R}`: a class
//! that inherits from `Base` and composes `R`, built once. `.new` on it builds
//! an instance of that class, whose BUILDPLAN initializes the role's attributes
//! from the named arguments like any other attribute (`(B but R).new(:type<x>)`
//! sets R's `$.type`, #17539).
//!
//! mutsu used to construct the bare `Base` and then mix each role into the new
//! object (`compose_mixin_type_roles`): a rebless into the very same class plus
//! a second, per-instance role composition — so the named arguments never
//! reached the role's attributes, and every construction paid the composition
//! again (Monad builds every value as `(pun but Some).new(:$value)`, #17331).

use super::*;

impl Interpreter {
    /// The registered class `base+{R1}+{R2}...` a `.new` on the mixin type
    /// object `inner but roles` builds, composing it on first use; `None`
    /// when the composition is not one an instance rebless would make (an
    /// unregistered or built-in base, an unregistered role, a parameterised
    /// role, or an initializer), which keeps the construct-then-compose path.
    ///
    /// The roles are stacked one at a time in the same (sorted) order
    /// `compose_mixin_type_roles` applies them, so the class is exactly the
    /// one that path reblesses the constructed object into.
    // Cost: O(r) registry probes, r = number of mixed-in roles, once the
    // class exists; composing it the first time is a class declaration.
    pub(crate) fn mixin_type_object_class(
        &mut self,
        inner: &Value,
        mixins: &crate::value::MixinOverrides,
    ) -> Result<Option<String>, RuntimeError> {
        let ValueView::Package(base) = inner.view() else {
            return Ok(None);
        };
        let base = base.resolve();
        if !self.registry().classes.contains_key(base.as_str()) {
            return Ok(None);
        }
        let roles = Self::mixin_role_applications(mixins);
        if roles.is_empty() {
            return Ok(None);
        }
        {
            let registry = self.registry();
            if roles
                .iter()
                .any(|(role, args)| args.is_some() || !registry.roles.contains_key(role))
            {
                return Ok(None);
            }
        }
        let mut class = base.to_string();
        for (role, _) in roles {
            class = self.ensure_mixin_class(&class, std::slice::from_ref(&role))?;
        }
        Ok(Some(class))
    }
}
