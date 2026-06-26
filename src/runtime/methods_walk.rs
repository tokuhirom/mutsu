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
            // A built-in type (e.g. `Grammar`) has no user `class_def`, but WALK
            // must still find its native methods. Resolve the method name from a
            // small table of WALKable built-in methods and return a single
            // candidate so `Grammar.WALK(:name<parse>)` yields a `.name`-able
            // candidate.
            return Ok(self.try_walk_builtin_type(target, &receiver_class_name, args));
        }

        // Parse arguments.
        let mut method_name: Option<String> = None;
        let mut want_roles = false;
        let mut order = WalkOrder::Canonical; // default ordering
        let mut omit_cb: Option<Value> = None;
        let mut include_cb: Option<Value> = None;
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
                    "omit" => omit_cb = Some(v.clone()),
                    "include" => include_cb = Some(v.clone()),
                    _ => {
                        // `:canonical`/`:super`/`:breadth`/`:ascendant`/`:descendant`
                        // /`:preorder`/`:postorder` select the ordering.
                        // `:method`/`:submethod` accepted; handled elsewhere or defaulted.
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

        // For each (kind, owner-name) that has an "own" candidate for the named
        // method, build a candidate closure `-> $inst, *@a { $inst.OWNER::name(|@a) }`.
        // Reusing qualified method-call dispatch (which already handles
        // submethods) means `$cand($instance)` and the batch-invoke share one
        // code path.
        let mut candidates: Vec<Value> = Vec::new();
        let mut matched: Vec<(WalkKind, String)> = Vec::new();
        for (kind, owner) in &walk_targets {
            if self
                .lookup_own_walk_method(kind, owner, &receiver_class_name, &method_name)
                .is_some()
            {
                // `:include(&cb)` / `:omit(&cb)` filter by calling the callback
                // with the OWNER type object: keep the candidate only when
                // include (if given) returns truthy AND omit (if given) does not.
                if include_cb.is_some() || omit_cb.is_some() {
                    let owner_type = Value::Package(Symbol::intern(owner));
                    if let Some(inc) = &include_cb {
                        let keep = self
                            .call_sub_value(inc.clone(), vec![owner_type.clone()], false)?
                            .truthy();
                        if !keep {
                            continue;
                        }
                    }
                    if let Some(om) = &omit_cb {
                        let drop = self
                            .call_sub_value(om.clone(), vec![owner_type.clone()], false)?
                            .truthy();
                        if drop {
                            continue;
                        }
                    }
                }
                candidates.push(Self::make_walk_candidate(owner, &method_name));
                matched.push((kind.clone(), owner.clone()));
            }
        }

        Ok(Some(self.make_walk_list(
            candidates,
            target.clone(),
            &receiver_class_name,
            &method_name,
            &matched,
        )))
    }

    /// WALK over a built-in type that has no user `class_def`. Built-in types
    /// dispatch their methods natively (not through a `ClassDef.methods` table),
    /// so we consult a small table of well-known WALKable built-in methods. If
    /// the named method is one of them, return a `WalkList` with a single
    /// candidate named after the method; otherwise `None` (so dispatch reports
    /// "no such method").
    fn try_walk_builtin_type(
        &self,
        target: &Value,
        type_name: &str,
        args: &[Value],
    ) -> Option<Value> {
        // Parse the method name (named `:name<...>` or first positional).
        let mut method_name: Option<String> = None;
        for arg in args {
            match arg {
                Value::Pair(k, v) if k == "name" => method_name = Some(v.to_string_value()),
                Value::ValuePair(k, v) if k.to_string_value() == "name" => {
                    method_name = Some(v.to_string_value())
                }
                Value::Pair(..) | Value::ValuePair(..) => {}
                other if method_name.is_none() => method_name = Some(other.to_string_value()),
                _ => {}
            }
        }
        let method_name = method_name?;
        if !builtin_type_has_method(type_name, &method_name) {
            return None;
        }
        let candidate = Self::make_walk_candidate(type_name, &method_name);
        Some(self.make_walk_list(
            vec![candidate],
            target.clone(),
            type_name,
            &method_name,
            &[(WalkKind::Class, type_name.to_string())],
        ))
    }

    /// Build a candidate closure that invokes `owner::method_name` on its first
    /// argument, forwarding any additional arguments: `-> $inst, *@a { $inst.OWNER::name(|@a) }`.
    fn make_walk_candidate(owner: &str, method_name: &str) -> Value {
        let qualified = Symbol::intern(&format!("{owner}::{method_name}"));
        let body = vec![Stmt::Expr(Expr::MethodCall {
            target: Box::new(Expr::Var("__walk_inst".to_string())),
            name: qualified,
            args: vec![Expr::Unary {
                op: crate::token_kind::TokenKind::Pipe,
                expr: Box::new(Expr::ArrayVar("__walk_args".to_string())),
            }],
            modifier: None,
            quoted: false,
        })];
        let inst_param = walk_param("__walk_inst", false);
        let slurpy_param = walk_param("@__walk_args", true);
        Value::make_sub(
            Symbol::intern(""),
            // Each candidate behaves like the named method: `.name` returns the
            // method name (e.g. `Grammar.WALK(:name<parse>)` → candidate `.name`
            // is `parse`).
            Symbol::intern(method_name),
            vec!["__walk_inst".to_string(), "@__walk_args".to_string()],
            vec![inst_param, slurpy_param],
            body,
            false,
            Env::new(),
        )
    }

    /// Construct a `WalkList` instance. Stored as a built-in `WalkList` class
    /// instance so it can be both list-iterated (its candidate closures, for
    /// `my @cands = $x.WALK(...)`) and invoked (`()`/`.invoke`). The matched
    /// targets (`kind|owner` pairs) plus the receiver class and method name are
    /// stored so the invoke path can resolve and call each method DIRECTLY —
    /// which is correct for role submethods, where a qualified-call closure would
    /// not resolve.
    fn make_walk_list(
        &self,
        candidates: Vec<Value>,
        invocant: Value,
        receiver_class: &str,
        method_name: &str,
        matched: &[(WalkKind, String)],
    ) -> Value {
        let targets: Vec<Value> = matched
            .iter()
            .map(|(kind, owner)| {
                let tag = match kind {
                    WalkKind::Class => "C",
                    WalkKind::Role => "R",
                };
                Value::str(format!("{tag}|{owner}"))
            })
            .collect();
        let mut attrs = std::collections::HashMap::new();
        attrs.insert("candidates".to_string(), Value::real_array(candidates));
        attrs.insert("invocant".to_string(), invocant);
        attrs.insert("reversed".to_string(), Value::Bool(false));
        attrs.insert("quiet".to_string(), Value::Bool(false));
        attrs.insert("targets".to_string(), Value::real_array(targets));
        attrs.insert(
            "receiver_class".to_string(),
            Value::str(receiver_class.to_string()),
        );
        attrs.insert(
            "method_name".to_string(),
            Value::str(method_name.to_string()),
        );
        Value::make_instance(Symbol::intern("WalkList"), attrs)
    }

    /// Invoke a WalkList: resolve and call each matched method DIRECTLY on the
    /// original invocant (correct for both class methods/submethods and role
    /// submethods), forwarding `args`. In quiet mode a thrown exception becomes a
    /// `Failure` in that slot. Slips are returned as-is.
    pub(crate) fn walk_list_invoke_direct(
        &mut self,
        walk_list: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let Value::Instance { attributes, .. } = walk_list else {
            return Ok(Value::real_array(Vec::new()));
        };
        let (targets, invocant, receiver_class, method_name, reversed, quiet) = {
            let map = attributes.as_map();
            let targets: Vec<String> = match map.get("targets") {
                Some(Value::Array(items, ..)) => {
                    items.iter().map(|v| v.to_string_value()).collect()
                }
                _ => Vec::new(),
            };
            let invocant = map.get("invocant").cloned().unwrap_or(Value::Nil);
            let receiver_class = map
                .get("receiver_class")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let method_name = map
                .get("method_name")
                .map(|v| v.to_string_value())
                .unwrap_or_default();
            let reversed = matches!(map.get("reversed"), Some(Value::Bool(true)));
            let quiet = matches!(map.get("quiet"), Some(Value::Bool(true)));
            (
                targets,
                invocant,
                receiver_class,
                method_name,
                reversed,
                quiet,
            )
        };
        let attributes_map = match &invocant {
            Value::Instance { attributes, .. } => attributes.to_map(),
            _ => std::collections::HashMap::new(),
        };
        let mut order: Vec<String> = targets;
        if reversed {
            order.reverse();
        }
        let mut results: Vec<Value> = Vec::with_capacity(order.len());
        for tag in &order {
            let (kind, owner) = match tag.split_once('|') {
                Some(("C", o)) => (WalkKind::Class, o.to_string()),
                Some(("R", o)) => (WalkKind::Role, o.to_string()),
                _ => continue,
            };
            let Some(method_def) =
                self.lookup_own_walk_method(&kind, &owner, &receiver_class, &method_name)
            else {
                continue;
            };
            let call = self.run_resolved_method_compiled_or_treewalk(
                &receiver_class,
                &owner,
                &method_name,
                method_def,
                attributes_map.clone(),
                args.clone(),
                Some(invocant.clone()),
            );
            match call {
                Ok((v, _)) => results.push(v),
                Err(e) if quiet => results.push(self.fail_error_to_failure_value(&e)),
                Err(e) => return Err(e),
            }
        }
        Ok(Value::real_array(results))
    }

    /// Dispatch a method call on a `WalkList` instance. Returns `Ok(None)` for
    /// methods this type does not handle (so normal dispatch can take over, e.g.
    /// `gist`/`isa`). `invoke`/`()` batch-call the matched candidates;
    /// `reverse`/`quiet` return a derived WalkList with the corresponding flag
    /// changed.
    pub(crate) fn try_walk_list_method(
        &mut self,
        walk_list: &Value,
        method: &str,
        args: Vec<Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        match method {
            "invoke" => Ok(Some(self.walk_list_invoke_direct(walk_list, args)?)),
            "reverse" => Ok(Some(Self::walk_list_with_flag(walk_list, "reversed", true))),
            "quiet" => Ok(Some(Self::walk_list_with_flag(walk_list, "quiet", true))),
            _ => Ok(None),
        }
    }

    /// Return a copy of a `WalkList` instance with one boolean flag set. For
    /// `reversed` the flag is toggled relative to the current value so chained
    /// `.reverse` calls compose correctly.
    fn walk_list_with_flag(walk_list: &Value, flag: &str, value: bool) -> Value {
        let Value::Instance { attributes, .. } = walk_list else {
            return walk_list.clone();
        };
        let mut attrs = attributes.to_map();
        let new_val = if flag == "reversed" {
            !matches!(attrs.get("reversed"), Some(Value::Bool(true)))
        } else {
            value
        };
        attrs.insert(flag.to_string(), Value::Bool(new_val));
        Value::make_instance(Symbol::intern("WalkList"), attrs)
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
                    // Submethods are not inherited (often flagged `is_my` so normal
                    // dispatch skips them on subclasses), but WALK explicitly visits
                    // each level's OWN submethods, so they must not be filtered here.
                    overloads
                        .iter()
                        .find(|m| {
                            m.role_origin.is_none() && !m.is_private && (!m.is_my || m.is_submethod)
                        })
                        .cloned()
                }
            }
            WalkKind::Role => {
                // Only role SUBMETHODS are WALKed: regular role methods are
                // composed into the consuming class's method table and are
                // already visited via the class chain, so including them again
                // as role candidates would duplicate them ("WALK doesn't use
                // methods in roles"). Submethods are not composed, so look them
                // up on the role's own method table.
                if let Some(role_def) = self.registry().roles.get(owner)
                    && let Some(overloads) = role_def.methods.get(method_name)
                    && let Some(m) = overloads.iter().find(|m| !m.is_private && m.is_submethod)
                {
                    return Some(m.clone());
                }
                // Fall back to a submethod composed into the receiver's class
                // table whose role origin matches this role.
                let registry = self.registry();
                let class_def = registry.classes.get(receiver_class)?;
                let overloads = class_def.methods.get(method_name)?;
                overloads
                    .iter()
                    .find(|m| {
                        let from_role = m.original_role.as_deref().or(m.role_origin.as_deref());
                        from_role == Some(owner) && !m.is_private && m.is_submethod
                    })
                    .cloned()
            }
        }
    }
}

/// Whether a built-in type (one with no user `class_def`) responds to a method
/// that WALK should surface. Built-in methods are dispatched natively in mutsu,
/// so there is no `ClassDef.methods` table to consult; this records the
/// well-known WALKable methods per built-in type.
fn builtin_type_has_method(type_name: &str, method: &str) -> bool {
    matches!(
        (type_name, method),
        ("Grammar", "parse" | "subparse" | "parsefile")
    )
}

/// Build a minimal `ParamDef` for a WALK candidate closure: a single positional
/// `$name`, or a slurpy `*@name` when `slurpy` is true.
fn walk_param(name: &str, slurpy: bool) -> crate::ast::ParamDef {
    crate::ast::ParamDef {
        name: name.to_string(),
        default: None,
        multi_invocant: true,
        required: false,
        named: false,
        slurpy,
        double_slurpy: false,
        onearg: false,
        sigilless: false,
        type_constraint: None,
        literal_value: None,
        sub_signature: None,
        where_constraint: None,
        traits: Vec::new(),
        optional_marker: false,
        outer_sub_signature: None,
        code_signature: None,
        is_invocant: false,
        shape_constraints: None,
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
