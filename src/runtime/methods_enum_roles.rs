//! `enum Foo does Role (...)` — role composition on an enum declaration.
//!
//! Rakudo's `enum` accepts the same `does` trait a class does: the role's
//! methods become methods of the enum's type object and of every enum value,
//! and the enum type-checks as doing the role. mutsu records the composition in
//! the same `class_composed_roles` registry a class uses (an enum has no
//! `ClassDef`, but role membership is keyed by type *name*, so the store is
//! shared), and dispatches the role's methods for an `Enum` / enum-`Package`
//! receiver from here.

use super::*;

impl Interpreter {
    /// Record the roles a `enum X does R` declaration composes. Transitive role
    /// parents are included, so `R2 does R1` makes the enum do `R1` too.
    pub(crate) fn compose_roles_onto_enum(
        &mut self,
        enum_type_name: &str,
        roles: &[String],
    ) -> Result<(), RuntimeError> {
        if roles.is_empty() {
            return Ok(());
        }
        let mut direct = Vec::new();
        let mut composed = Vec::new();
        for role in roles {
            let resolved = self.resolve_declared_type_name(role);
            let base = resolved
                .split_once('[')
                .map_or(resolved.as_str(), |(base, _)| base)
                .to_string();
            if !self.has_role(&base) {
                return Err(RuntimeError::new(format!("Unknown role: {resolved}")));
            }
            direct.push(resolved.clone());
            // Walk role parents so an enum doing `R2` (which does `R1`) also
            // type-checks as doing `R1`, matching class composition.
            let mut stack = vec![base];
            let mut seen = std::collections::HashSet::new();
            while let Some(name) = stack.pop() {
                if !seen.insert(name.clone()) {
                    continue;
                }
                if !composed.contains(&name) {
                    composed.push(name.clone());
                }
                if let Some(parents) = self.registry().role_parents.get(&name) {
                    for parent in parents.clone() {
                        let parent_base = parent
                            .split_once('[')
                            .map_or(parent.as_str(), |(base, _)| base)
                            .to_string();
                        stack.push(parent_base);
                    }
                }
            }
        }
        let mut registry = self.registry_mut();
        registry
            .class_composed_roles
            .insert(enum_type_name.to_string(), composed);
        registry
            .class_direct_composed_roles
            .insert(enum_type_name.to_string(), direct);
        Ok(())
    }

    /// The enum type name a receiver stands for, when that enum composes roles.
    fn enum_receiver_with_roles(&self, target: &Value) -> Option<String> {
        let name = match target.view() {
            ValueView::Enum { enum_type, .. } => enum_type.resolve(),
            ValueView::Package(name) => name.resolve(),
            _ => return None,
        };
        if !self.registry().enum_types.contains_key(&name) {
            return None;
        }
        let has_roles = self
            .registry()
            .class_composed_roles
            .get(&name)
            .is_some_and(|roles| !roles.is_empty());
        has_roles.then_some(name)
    }

    /// Does the enum this receiver belongs to compose a role supplying
    /// `method`? Cheap registry lookup, no dispatch — used to decide whether
    /// the smartmatch `ACCEPTS` protocol applies to an enum matcher.
    pub(crate) fn enum_composes_role_method(&self, target: &Value, method: &str) -> bool {
        let Some(enum_name) = self.enum_receiver_with_roles(target) else {
            return false;
        };
        self.registry()
            .class_composed_roles
            .get(&enum_name)
            .is_some_and(|roles| {
                roles
                    .iter()
                    .any(|role| self.role_or_parent_has_method(role, method))
            })
    }

    /// Run a composed role's method for an enum receiver (`red.greet`,
    /// `Col.greet`, `A.ACCEPTS(5)`). `None` means no composed role supplies it,
    /// so the caller keeps its own dispatch.
    pub(crate) fn dispatch_enum_role_method(
        &mut self,
        target: &Value,
        method: &str,
        args: &[Value],
    ) -> Option<Result<Value, RuntimeError>> {
        let enum_name = self.enum_receiver_with_roles(target)?;
        let roles = self
            .registry()
            .class_composed_roles
            .get(&enum_name)
            .cloned()
            .unwrap_or_default();
        for role_name in roles {
            let Some(overloads) = self
                .registry()
                .roles
                .get(&role_name)
                .and_then(|role| role.methods.get(method))
                .cloned()
            else {
                continue;
            };
            for def in overloads {
                if def.is_private || !self.method_args_match(args, &def.param_defs) {
                    continue;
                }
                let result = self.run_resolved_method_compiled_or_treewalk(
                    &role_name,
                    &role_name,
                    method,
                    def,
                    AttrMap::new(),
                    args.to_vec(),
                    Some(target.clone()),
                );
                return Some(result.map(|(value, _)| value));
            }
        }
        None
    }
}
