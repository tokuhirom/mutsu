use super::super::*;
use crate::symbol::Symbol;

impl Interpreter {
    pub(in crate::runtime) fn role_is_subtype(&self, lhs_role: &str, rhs_role: &str) -> bool {
        if lhs_role == rhs_role {
            return true;
        }
        let mut stack = vec![lhs_role.to_string()];
        let mut seen = HashSet::new();
        while let Some(role) = stack.pop() {
            if !seen.insert(role.clone()) {
                continue;
            }
            if let Some(parents) = self.role_parents.get(&role) {
                for parent in parents {
                    if parent == rhs_role {
                        return true;
                    }
                    stack.push(parent.clone());
                }
            }
        }
        false
    }

    /// Check if `lhs` type arg is a subtype of `rhs` type arg for parametric role subtyping.
    /// E.g., Package("C2") subtypes Package("C1") if C2 isa C1.
    pub(in crate::runtime) fn parametric_arg_subtypes(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs, rhs) {
            // Both are packages (type objects): check class hierarchy
            (Value::Package(l_name), Value::Package(r_name)) => {
                if l_name == r_name {
                    return true;
                }
                let l_resolved = l_name.resolve();
                let r_resolved = r_name.resolve();
                // Built-in type hierarchy relationships (e.g. Int <: Cool <: Any)
                if Self::type_matches(&r_resolved, &l_resolved) {
                    return true;
                }
                // Check if l_name is a subclass of r_name
                if let Some(class_def) = self.classes.get(&l_resolved) {
                    if class_def.parents.iter().any(|p| p == &r_resolved) {
                        return true;
                    }
                    // Transitive: check if any parent subtypes r_name
                    for parent in &class_def.parents {
                        if self
                            .parametric_arg_subtypes(&Value::Package(Symbol::intern(parent)), rhs)
                        {
                            return true;
                        }
                    }
                }
                false
            }
            // Both are parametric roles: recursively check
            (
                Value::ParametricRole {
                    base_name: lb,
                    type_args: la,
                },
                Value::ParametricRole {
                    base_name: rb,
                    type_args: ra,
                },
            ) => {
                if lb != rb || la.len() != ra.len() {
                    return false;
                }
                la.iter()
                    .zip(ra.iter())
                    .all(|(l, r)| self.parametric_arg_subtypes(l, r))
            }
            _ => lhs == rhs,
        }
    }
}
