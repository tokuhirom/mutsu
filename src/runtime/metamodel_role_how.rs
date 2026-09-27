//! Building a role through the metaobject protocol rather than a `role`
//! declaration: `Metamodel::ParametricRoleHOW.new_type(:name<R>)` followed by
//! `.^add_method`, `.^set_body_block` and `.^compose`, then consumed by
//! `.^mixin`/`does`/`but`. Tinky builds its per-workflow transition role
//! exactly this way.

use super::*;

impl Interpreter {
    /// Register an empty role named `name`, the type a
    /// `Metamodel::ParametricRoleHOW.new_type` call mints. Composition reads a
    /// role's methods and attributes from its `RoleDef`, so a MOP-built role
    /// needs one just like a declared role does.
    pub(super) fn register_metamodel_role(&mut self, name: &str) {
        if self.registry().roles.contains_key(name) {
            return;
        }
        let role_def = super::registration_class::builtin_role_def();
        self.registry_mut().roles.insert(name.to_string(), role_def);
    }

    /// `$role.^add_method($name, $code)` on a role: the candidates go to the
    /// role's own method table, which `does`/`but`/`.^mixin` copy into the
    /// consumer. A private candidate already under the name survives, as for
    /// a class (mutsu keys public and private methods under one name).
    // Cost: O(k), k = candidates already registered under `method_name`.
    pub(crate) fn add_methods_to_role(
        &mut self,
        role_name: &str,
        method_name: &str,
        mut defs: Vec<MethodDef>,
    ) -> Result<Value, RuntimeError> {
        let mut registry = self.registry_mut();
        let Some(role) = registry.roles.get_mut(role_name) else {
            return Ok(Value::NIL);
        };
        if let Some(existing) = role.methods.get(method_name) {
            defs.extend(existing.iter().filter(|def| def.is_private).cloned());
        }
        role.methods.insert(method_name.to_string(), defs);
        Ok(Value::NIL)
    }

    /// `$role.^set_body_block(&block)`: remember the block every composition
    /// of the role calls (see [`Interpreter::run_role_body_block`]).
    // Cost: O(1).
    pub(crate) fn set_role_body_block(&mut self, role_name: &str, block: Value) -> Value {
        self.registry_mut()
            .role_body_blocks
            .insert(role_name.to_string(), block);
        Value::NIL
    }

    /// Call the body block `set_body_block` installed on `role_name`, if any,
    /// with the consuming type as its only argument — Rakudo's
    /// `ParametricRoleHOW.specialize` runs `$!body_block(|@pos_args)` with the
    /// target type first. Only a MOP-built role has one; a declared role's
    /// body is its `deferred_body`, run by the composition sites themselves.
    // Cost: O(1) plus the block's own run time.
    pub(crate) fn run_role_body_block(
        &mut self,
        role_name: &str,
        target: Value,
    ) -> Result<(), RuntimeError> {
        let Some(block) = self.registry().role_body_blocks.get(role_name).cloned() else {
            return Ok(());
        };
        self.call_sub_value(block, vec![target], false)?;
        Ok(())
    }
}
