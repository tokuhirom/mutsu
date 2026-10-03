//! Which role attributes of a mixin a method call may reach as accessors.

use super::*;

impl Interpreter {
    /// Whether `name` is an attribute that some role mixed into `mixins`
    /// declares private (`has $!name`) while none declares it public
    /// (`has $.name`). Such an attribute has no accessor, so a `.name` call
    /// must reach the mixed-in value's own method instead.
    // Cost: O(r * a), r = roles mixed in, a = attributes per role.
    pub(crate) fn mixin_role_attr_is_private_only(
        &self,
        mixins: &crate::value::MixinOverrides,
        name: &str,
    ) -> bool {
        let registry = self.registry();
        let mut private = false;
        for role_name in mixins
            .keys()
            .filter_map(|key| key.strip_prefix("__mutsu_role__"))
        {
            let Some(role) = registry.roles.get(role_name) else {
                continue;
            };
            for attr in role.attributes.iter().filter(|a| a.name == name) {
                if attr.is_public {
                    return false;
                }
                private = true;
            }
        }
        private
    }
}
