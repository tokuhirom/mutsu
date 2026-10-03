//! `.of` of a type that composes `Positional[V]` / `Associative[V, K]`.
//!
//! The role's type argument is the answer, wherever the composition happens:
//! directly (`class A does Positional[Int]`), through a parametric role that
//! passes its own parameter on (`role R[::T] does Positional[T]`;
//! `class B does R[Str]` answers `Str`), and on a type object as well as on an
//! instance. Upstream NativeCall's `CArray[uint8].of` is the last shape: its
//! `^parameterize` mixes in `IntTypedCArray[uint8]`, which `does
//! Positional[TValue]` (#11726).

use super::*;

/// How deep a chain of roles passing a parameter on is followed. A cycle is
/// not composable, so this only bounds a pathological registry.
const MAX_ROLE_DEPTH: usize = 16;

impl Interpreter {
    /// The value type of the container role `class_name` (or a class in its
    /// MRO) composes, if any.
    // Cost: O(m * r * d), m = classes in the MRO, r = roles each composes,
    // d = role chain depth (bounded by MAX_ROLE_DEPTH).
    pub(super) fn composed_container_role_value_type(
        &mut self,
        class_name: &str,
    ) -> Option<String> {
        let mro = self.class_mro(class_name);
        let mut classes: Vec<String> = mro.iter().map(|s| s.resolve()).collect();
        if !classes.iter().any(|c| c == class_name) {
            classes.insert(0, class_name.to_string());
        }
        for class in &classes {
            let roles = self
                .registry()
                .class_composed_roles
                .get(class.as_str())
                .cloned()
                .unwrap_or_default();
            for role in roles {
                if let Some(value_type) = self.role_spelling_value_type(&role, 0) {
                    return Some(value_type);
                }
            }
        }
        None
    }

    /// The container value type a role spelled `role` (`Positional[Int]`,
    /// `R[Str]`) supplies.
    // Cost: as `composed_container_role_value_type`, for one role.
    pub(crate) fn role_spelling_value_type(&self, role: &str, depth: usize) -> Option<String> {
        if depth > MAX_ROLE_DEPTH {
            return None;
        }
        let (base, args) = match role.split_once('[') {
            Some((base, rest)) => (base, rest.strip_suffix(']').map(parse_args)),
            None => (role, None),
        };
        if matches!(base, "Associative" | "Positional") {
            return args.and_then(|a| a.into_iter().next());
        }
        // A role's own parents, with its type parameters bound to `args`.
        let (params, parents) = {
            let reg = self.registry();
            (
                reg.role_type_params.get(base).cloned().unwrap_or_default(),
                reg.role_parents.get(base).cloned().unwrap_or_default(),
            )
        };
        let args = args.unwrap_or_default();
        for parent in parents {
            let bound = substitute_role_params(&parent, &params, &args);
            if let Some(value_type) = self.role_spelling_value_type(&bound, depth + 1) {
                return Some(value_type);
            }
        }
        None
    }
}

fn parse_args(inner: &str) -> Vec<String> {
    super::registration_class::parse_role_type_args(inner)
}

/// `parent` (`Positional[T]`) with each argument that names one of the role's
/// type parameters (`::T` / `T`) replaced by the argument it was given.
// Cost: O(a * p), a = parent's arguments, p = role parameters.
fn substitute_role_params(parent: &str, params: &[String], args: &[String]) -> String {
    let Some((base, rest)) = parent.split_once('[') else {
        return parent.to_string();
    };
    let Some(inner) = rest.strip_suffix(']') else {
        return parent.to_string();
    };
    let bound: Vec<String> = parse_args(inner)
        .into_iter()
        .map(|arg| {
            params
                .iter()
                .position(|p| p.trim_start_matches("::") == arg)
                .and_then(|i| args.get(i).cloned())
                .unwrap_or(arg)
        })
        .collect();
    format!("{base}[{}]", bound.join(","))
}
