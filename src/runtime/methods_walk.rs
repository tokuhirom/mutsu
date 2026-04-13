//! Implementation of `Mu.WALK(method-name, :roles, :super, :submethod, :name, :omit)`.
//!
//! WALK walks the receiver's class hierarchy (and optionally its composed roles)
//! looking up *own* candidates of the named method on each level. It returns a
//! callable: invoking it with `()` produces a list of the results of calling
//! each found method on the original invocant.
//!
//! Reference: <https://docs.raku.org/type/Any#method_WALK>
//!
//! Supported options for now:
//! - `:name<methodname>` — the method name (also accepted as the first
//!   positional argument)
//! - `:roles` — also include each composed role in BFS order
//! - `:super` — include parent classes (default)
//! - `:method` / `:submethod` — currently both are looked up via the same
//!   per-class method table; this is enough for what roast tests exercise.
//!
//! The implementation pre-evaluates each method call eagerly when WALK is
//! invoked, then returns a no-arg `Sub` whose body returns the resulting
//! list literal. This keeps the result trivially callable as `WALK(...)()`
//! without needing a bespoke runtime callable kind.

use super::*;
use crate::ast::{Expr, Stmt};
use crate::env::Env;
use crate::symbol::Symbol;
use crate::value::Value;
use std::collections::HashSet;

impl Interpreter {
    pub(crate) fn try_walk_method(
        &mut self,
        target: &Value,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        // Only handle WALK on instances or type objects backed by a class def.
        let receiver_class_name = match target {
            Value::Instance { class_name, .. } => class_name.resolve(),
            Value::Package(name) => name.resolve(),
            _ => return Ok(None),
        };
        if !self.classes.contains_key(&receiver_class_name) {
            return Ok(None);
        }

        // Parse arguments.
        let mut method_name: Option<String> = None;
        let mut want_roles = false;
        let mut want_super = true; // default: walk class MRO
        for arg in args {
            match arg {
                Value::Pair(k, v) => match k.as_str() {
                    "name" => method_name = Some(v.to_string_value()),
                    "roles" => want_roles = v.truthy(),
                    "super" => want_super = v.truthy(),
                    "method" | "submethod" | "omit" => { /* accepted; default behaviour */ }
                    _ => {}
                },
                Value::ValuePair(k, v) => {
                    let key = k.to_string_value();
                    match key.as_str() {
                        "name" => method_name = Some(v.to_string_value()),
                        "roles" => want_roles = v.truthy(),
                        "super" => want_super = v.truthy(),
                        "method" | "submethod" | "omit" => {}
                        _ => {}
                    }
                }
                other => {
                    if method_name.is_none() {
                        method_name = Some(other.to_string_value());
                    }
                }
            }
        }
        let Some(method_name) = method_name else {
            return Ok(None);
        };

        // Build the walk order: BFS over class MRO, optionally including
        // composed roles via `class_composed_roles` and `role_parents`.
        let walk_targets = self.build_walk_targets(&receiver_class_name, want_super, want_roles);

        // For each (kind, owner-name), look up an "own" MethodDef for the
        // named method and invoke it on the original invocant.
        let mut results: Vec<Value> = Vec::new();
        for (kind, owner) in &walk_targets {
            if let Some(method_def) =
                self.lookup_own_walk_method(kind, owner, &receiver_class_name, &method_name)
            {
                let attributes = match target {
                    Value::Instance { attributes, .. } => (**attributes).clone(),
                    _ => std::collections::HashMap::new(),
                };
                let result = self.run_instance_method_resolved(
                    &receiver_class_name,
                    owner,
                    method_def,
                    attributes,
                    Vec::new(),
                    Some(target.clone()),
                )?;
                results.push(result.0);
            }
        }

        // Wrap the precomputed results in a no-arg Sub whose body is the
        // list literal `(v1, v2, ..., vN)`.
        let body_exprs: Vec<Expr> = results.into_iter().map(Expr::Literal).collect();
        let body = vec![Stmt::Expr(Expr::ArrayLiteral(body_exprs))];
        let sub = Value::make_sub(
            Symbol::intern(""),
            Symbol::intern("WALK"),
            Vec::new(),
            Vec::new(),
            body,
            false,
            Env::new(),
        );
        Ok(Some(sub))
    }

    fn build_walk_targets(
        &self,
        class_name: &str,
        want_super: bool,
        want_roles: bool,
    ) -> Vec<(WalkKind, String)> {
        let mut out: Vec<(WalkKind, String)> = Vec::new();
        let mut visited_classes: HashSet<String> = HashSet::new();
        let mut visited_roles: HashSet<String> = HashSet::new();

        // Walk classes in MRO order. For each class, add the class itself
        // and (if `:roles`) BFS over its composed roles.
        let mro = self
            .class_mro_readonly(class_name)
            .unwrap_or_else(|| vec![class_name.to_string()]);
        let class_chain: Vec<String> = if want_super {
            mro
        } else {
            vec![class_name.to_string()]
        };

        for cn in &class_chain {
            if cn == "Mu" || cn == "Any" || cn == "Cool" {
                continue;
            }
            if visited_classes.insert(cn.clone()) {
                out.push((WalkKind::Class, cn.clone()));
            }
            if !want_roles {
                continue;
            }
            // BFS over composed roles for this class.
            let initial: Vec<String> = self
                .class_composed_roles
                .get(cn)
                .cloned()
                .unwrap_or_default();
            let mut queue: std::collections::VecDeque<String> = initial.into_iter().collect();
            while let Some(role_name) = queue.pop_front() {
                let base = role_name
                    .split_once('[')
                    .map(|(b, _)| b.to_string())
                    .unwrap_or(role_name.clone());
                if !visited_roles.insert(base.clone()) {
                    continue;
                }
                out.push((WalkKind::Role, base.clone()));
                if let Some(parents) = self.role_parents.get(&base) {
                    for p in parents {
                        let p_base = p
                            .split_once('[')
                            .map(|(b, _)| b.to_string())
                            .unwrap_or(p.clone());
                        if !visited_roles.contains(&p_base) {
                            queue.push_back(p_base);
                        }
                    }
                }
            }
        }
        out
    }

    /// Read-only variant of class_mro for use from non-mut helpers.
    fn class_mro_readonly(&self, class_name: &str) -> Option<Vec<String>> {
        let class_def = self.classes.get(class_name)?;
        if !class_def.mro.is_empty() {
            return Some(class_def.mro.clone());
        }
        Some(vec![class_name.to_string()])
    }

    /// Look up the "own" MethodDef for `method_name` on the given class or role.
    /// "Own" means defined directly on that level (not inherited from a parent).
    fn lookup_own_walk_method(
        &self,
        kind: &WalkKind,
        owner: &str,
        receiver_class: &str,
        method_name: &str,
    ) -> Option<MethodDef> {
        match kind {
            WalkKind::Class => {
                // For the receiver's own class, "own" is any candidate stored
                // on the class with role_origin == None.
                let class_def = self.classes.get(owner)?;
                let overloads = class_def.methods.get(method_name)?;
                if owner == receiver_class {
                    overloads
                        .iter()
                        .find(|m| m.role_origin.is_none() && !m.is_private)
                        .cloned()
                } else {
                    overloads
                        .iter()
                        .find(|m| m.role_origin.is_none() && !m.is_private && !m.is_my)
                        .cloned()
                }
            }
            WalkKind::Role => {
                // Submethods on roles are not composed into the consuming
                // class's method table, so look them up on the role's own
                // method table first.
                if let Some(role_def) = self.roles.get(owner)
                    && let Some(overloads) = role_def.methods.get(method_name)
                    && let Some(m) = overloads.iter().find(|m| !m.is_private)
                {
                    return Some(m.clone());
                }
                // Fall back to a method composed into the receiver's class
                // table whose role origin matches this role.
                let class_def = self.classes.get(receiver_class)?;
                let overloads = class_def.methods.get(method_name)?;
                overloads
                    .iter()
                    .find(|m| {
                        let from_role = m.original_role.as_deref().or(m.role_origin.as_deref());
                        from_role == Some(owner) && !m.is_private
                    })
                    .cloned()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
enum WalkKind {
    Class,
    Role,
}
