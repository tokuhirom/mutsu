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
        if !self.registry().classes.contains_key(&receiver_class_name) {
            return Ok(None);
        }

        // Parse arguments.
        let mut method_name: Option<String> = None;
        let mut want_roles = false;
        let mut order = WalkOrder::Canonical; // default ordering
        for arg in args {
            let named: Option<(String, &Value)> = match arg {
                Value::Pair(k, v) => Some((k.clone(), v.as_ref())),
                Value::ValuePair(k, v) => Some((k.to_string_value(), v.as_ref())),
                other => {
                    if method_name.is_none() {
                        method_name = Some(other.to_string_value());
                    }
                    None
                }
            };
            if let Some((key, v)) = named {
                match key.as_str() {
                    "name" => method_name = Some(v.to_string_value()),
                    "roles" => want_roles = v.truthy(),
                    _ => {
                        // `:canonical`/`:super`/`:breadth`/`:ascendant`/`:descendant`
                        // /`:preorder`/`:postorder` select the ordering.
                        // `:method`/`:submethod`/`:omit`/`:include` accepted; handled
                        // elsewhere or defaulted.
                        if let Some(o) = WalkOrder::from_adverb(&key)
                            && v.truthy()
                        {
                            order = o;
                        }
                    }
                }
            }
        }
        let Some(method_name) = method_name else {
            return Ok(None);
        };

        // Build the walk order over the class hierarchy in the requested order,
        // optionally including composed roles.
        let walk_targets = self.build_walk_targets(&receiver_class_name, order, want_roles);

        // For each (kind, owner-name), look up an "own" MethodDef for the
        // named method and invoke it on the original invocant.
        let mut results: Vec<Value> = Vec::new();
        for (kind, owner) in &walk_targets {
            if let Some(method_def) =
                self.lookup_own_walk_method(kind, owner, &receiver_class_name, &method_name)
            {
                let attributes = match target {
                    Value::Instance { attributes, .. } => attributes.to_map(),
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
        order: WalkOrder,
        want_roles: bool,
    ) -> Vec<(WalkKind, String)> {
        let mut out: Vec<(WalkKind, String)> = Vec::new();
        let mut visited_classes: HashSet<String> = HashSet::new();
        let mut visited_roles: HashSet<String> = HashSet::new();

        // Walk classes in the requested order. For each class, add the class
        // itself and (if `:roles`) BFS over its composed roles.
        let class_chain: Vec<String> = self.walk_class_order(class_name, order);

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
                .registry()
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
                if let Some(parents) = self.registry().role_parents.get(&base) {
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
        self.registry().class_mro_cached(class_name)
    }

    /// Direct (declared) parent classes of `class_name`, in declaration order,
    /// restricted to user/builtin class definitions and excluding the implicit
    /// `Any`/`Mu`/`Cool` roots (which never carry WALKable methods here).
    fn walk_direct_parents(&self, class_name: &str) -> Vec<String> {
        let registry = self.registry();
        let Some(def) = registry.classes.get(class_name) else {
            return Vec::new();
        };
        def.parents
            .iter()
            .filter(|p| !matches!(p.as_str(), "Any" | "Mu" | "Cool"))
            .filter(|p| registry.classes.contains_key(*p))
            .cloned()
            .collect()
    }

    /// Compute the ordered list of class names to walk for the requested
    /// ordering. All walks dedup keeping the FIRST occurrence. `Any`/`Mu`/`Cool`
    /// are excluded. Reverse-engineered from S12-introspection/walk.t:
    /// for `E is C is D`, `C is A is B`, `D is A` the receiver `E` yields
    /// canonical=ECDAB, super=CD, breadth=ECDAB, ascendant/preorder=ECABD,
    /// descendant=ABCDE.
    fn walk_class_order(&self, receiver: &str, ordering: WalkOrder) -> Vec<String> {
        let exclude = |n: &str| matches!(n, "Any" | "Mu" | "Cool");
        match ordering {
            WalkOrder::Canonical => self
                .class_mro_readonly(receiver)
                .unwrap_or_else(|| vec![receiver.to_string()])
                .into_iter()
                .filter(|n| !exclude(n))
                .collect(),
            WalkOrder::Super => self.walk_direct_parents(receiver),
            WalkOrder::Breadth => {
                let mut out = Vec::new();
                let mut seen = HashSet::new();
                let mut queue: std::collections::VecDeque<String> =
                    std::collections::VecDeque::new();
                queue.push_back(receiver.to_string());
                while let Some(c) = queue.pop_front() {
                    if exclude(&c) || !seen.insert(c.clone()) {
                        continue;
                    }
                    out.push(c.clone());
                    for p in self.walk_direct_parents(&c) {
                        queue.push_back(p);
                    }
                }
                out
            }
            WalkOrder::Ascendant => {
                // DFS preorder, dedup keeping first.
                let mut out = Vec::new();
                let mut seen = HashSet::new();
                self.walk_preorder(receiver, &mut out, &mut seen);
                out
            }
            WalkOrder::Descendant => {
                // DFS postorder, dedup keeping first.
                let mut out = Vec::new();
                let mut seen = HashSet::new();
                self.walk_postorder(receiver, &mut out, &mut seen);
                out
            }
        }
    }

    fn walk_preorder(&self, class_name: &str, out: &mut Vec<String>, seen: &mut HashSet<String>) {
        if matches!(class_name, "Any" | "Mu" | "Cool") {
            return;
        }
        if seen.insert(class_name.to_string()) {
            out.push(class_name.to_string());
        }
        for p in self.walk_direct_parents(class_name) {
            self.walk_preorder(&p, out, seen);
        }
    }

    fn walk_postorder(&self, class_name: &str, out: &mut Vec<String>, seen: &mut HashSet<String>) {
        if matches!(class_name, "Any" | "Mu" | "Cool") {
            return;
        }
        for p in self.walk_direct_parents(class_name) {
            self.walk_postorder(&p, out, seen);
        }
        if seen.insert(class_name.to_string()) {
            out.push(class_name.to_string());
        }
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
                let registry = self.registry();
                let class_def = registry.classes.get(owner)?;
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
                if let Some(role_def) = self.registry().roles.get(owner)
                    && let Some(overloads) = role_def.methods.get(method_name)
                    && let Some(m) = overloads.iter().find(|m| !m.is_private)
                {
                    return Some(m.clone());
                }
                // Fall back to a method composed into the receiver's class
                // table whose role origin matches this role.
                let registry = self.registry();
                let class_def = registry.classes.get(receiver_class)?;
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

/// Traversal ordering for `WALK`. `:preorder` is an alias for `:ascendant`.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) enum WalkOrder {
    Canonical,
    Super,
    Breadth,
    Ascendant,
    Descendant,
}

impl WalkOrder {
    fn from_adverb(name: &str) -> Option<WalkOrder> {
        match name {
            "canonical" => Some(WalkOrder::Canonical),
            "super" => Some(WalkOrder::Super),
            "breadth" => Some(WalkOrder::Breadth),
            "ascendant" | "preorder" => Some(WalkOrder::Ascendant),
            "descendant" | "postorder" => Some(WalkOrder::Descendant),
            _ => None,
        }
    }
}
