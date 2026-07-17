use super::super::*;
use crate::symbol::Symbol;
use crate::value::ValueView;

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
            if let Some(parents) = self.registry().role_parents.get(&role) {
                for parent in parents {
                    // A parent may be recorded in parametric form ("P2[Int]");
                    // compare and continue the walk on its base name.
                    let parent_base = parent
                        .split_once('[')
                        .map(|(base, _)| base)
                        .unwrap_or(parent.as_str());
                    if parent_base == rhs_role {
                        return true;
                    }
                    stack.push(parent_base.to_string());
                }
            }
        }
        false
    }

    /// Check if `lhs` type arg is a subtype of `rhs` type arg for parametric role subtyping.
    /// E.g., Package("C2") subtypes Package("C1") if C2 isa C1.
    pub(in crate::runtime) fn parametric_arg_subtypes(&self, lhs: &Value, rhs: &Value) -> bool {
        match (lhs.view(), rhs.view()) {
            // Both are packages (type objects): check class hierarchy
            (ValueView::Package(l_name), ValueView::Package(r_name)) => {
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
                if let Some(class_def) = self.registry().classes.get(&l_resolved) {
                    if class_def.parents.iter().any(|p| p == &r_resolved) {
                        return true;
                    }
                    // Transitive: check if any parent subtypes r_name
                    for parent in &class_def.parents {
                        if self
                            .parametric_arg_subtypes(&Value::package(Symbol::intern(parent)), rhs)
                        {
                            return true;
                        }
                    }
                }
                false
            }
            // Both are parametric roles: recursively check
            (
                ValueView::ParametricRole {
                    base_name: lb,
                    type_args: la,
                },
                ValueView::ParametricRole {
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
