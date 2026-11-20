//! The grammar-action walk: run the action method of a reduced Match node
//! and of every child node that has not run its own yet (ADR-7576).
//!
//! An action runs on the node as the matcher stored it. The walk reads the
//! node's children straight from its capture node
//! ([`Value::match_action_walk_node`]) instead of materializing the Match into
//! an `Instance` attribute map, so a node whose action never inspects `$/`
//! never allocates one. Before #18207 every non-leaf node with an action was
//! materialized here (~16k instructions) and its map dropped again (~10k),
//! which was a third of a trivial action's whole cost.
//!
//! The env ceremony around the call — `$/`, `$_`, `$<name>`, `$0`.. installed
//! for the action and the parent's put back afterwards — is keyed by
//! pre-interned symbols ([`wk`]), never by a freshly allocated `String`.

use super::*;
use crate::symbol::{Symbol, wk};
use crate::value::ActionWalkNode;

/// `FALLBACK`, which an actions class may answer every rule name with.
fn fallback_sym() -> Symbol {
    static SYM: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
    *SYM.get_or_init(|| Symbol::intern("FALLBACK"))
}

impl Interpreter {
    /// Extract `action_name` from a Match object (set for aliased captures).
    pub(crate) fn get_action_name(match_obj: &Value) -> Option<String> {
        match_obj.match_action_name()
    }

    /// The class an actions object dispatches its methods on, if it names one.
    /// A stateless `:actions(Actions)` is often passed as the bare type object,
    /// so a `Package` counts just as much as an `Instance`.
    // Cost: O(1).
    pub(crate) fn actions_class_sym(actions: &Value) -> Option<Symbol> {
        match actions.view() {
            ValueView::Instance { class_name, .. } => Some(class_name),
            ValueView::Package(name) => Some(name),
            _ => None,
        }
    }

    /// [`Self::actions_answer_method`] for an interned class and method name.
    // Cost: O(1) amortized, two memoized `has_user_method_of` probes.
    pub(crate) fn actions_answer_method_of(&mut self, class: Symbol, method: Symbol) -> bool {
        self.has_user_method_of(class, method) || self.has_user_method_of(class, fallback_sym())
    }

    /// The action-method name the winning proto-regex variant dispatches to.
    ///
    /// The winning-variant marker records only the adverb's *value* (`num`),
    /// which two different declaration spellings share:
    ///
    /// | declaration | action method |
    /// | --- | --- |
    /// | `token pair:sym<num>` | `method pair:sym<num>` |
    /// | `token pair:num` (bare adverb) | `method pair:num` |
    ///
    /// So the name is recovered by asking the actions class which of the two it
    /// actually declares, preferring the `:sym<...>` spelling (by far the more
    /// common one, and the only one that binds a `<sym>` literal). With no
    /// actions class to ask, the `:sym<...>` spelling is assumed.
    pub(crate) fn variant_action_method_name(
        &mut self,
        actions_class: Option<&str>,
        rule_name: &str,
        sym_val: &str,
    ) -> String {
        let bracketed = if sym_val.contains('<') || sym_val.contains('>') {
            format!("{rule_name}:sym\u{ab}{sym_val}\u{bb}")
        } else {
            format!("{rule_name}:sym<{sym_val}>")
        };
        let Some(cn) = actions_class else {
            return bracketed;
        };
        if self.has_user_method(cn, &bracketed) {
            return bracketed;
        }
        let bare = format!("{rule_name}:{sym_val}");
        if self.has_user_method(cn, &bare) {
            return bare;
        }
        // A shorthand proto candidate (`rule modifier:<null>`) has the same
        // runtime variant value as `rule modifier:sym<null>`, but its grammar
        // action method keeps the shorthand spelling. Try that spelling after
        // the established `:sym<>` and bare-adverb forms so existing action
        // dispatch remains preferred when both declarations are present.
        let shorthand = format!("{rule_name}:<{sym_val}>");
        if self.has_user_method(cn, &shorthand) {
            return shorthand;
        }
        bracketed
    }

    /// [`Self::invoke_grammar_actions`], but at most once per capture node
    /// within one parent's child walk.
    ///
    /// `seen` records the node identities of the siblings dispatched so far.
    /// The one shape that repeats a node among siblings is a non-suppressing
    /// alias (`<x=rule>` files the same node under `x` and `rule`), where
    /// raku hands out one cursor — so the second slot must not dispatch
    /// again; it reads the `.made` the single dispatch wrote into the node.
    fn invoke_grammar_actions_once(
        &mut self,
        seen: &mut Vec<usize>,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<(), RuntimeError> {
        let identity = match_obj.match_node_identity();
        if let Some(id) = identity {
            if seen.contains(&id) {
                return Ok(());
            }
            seen.push(id);
        }
        self.invoke_grammar_actions(match_obj, actions, rule_name)?;
        Ok(())
    }

    fn restore_action_reduce_vars(&mut self, saved: &[(String, Option<Value>)]) {
        for (key, previous) in saved {
            match previous {
                Some(value) => {
                    self.env.insert(key.clone(), value.clone());
                }
                None => {
                    self.env.remove(key);
                }
            }
        }
    }

    /// Walk the match tree bottom-up and invoke action methods on the actions
    /// object.
    pub(crate) fn invoke_grammar_actions(
        &mut self,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<Value, RuntimeError> {
        let result = self.invoke_grammar_actions_unmarked(match_obj.clone(), actions, rule_name);
        // Whoever dispatched this node (the walk, an assertion's `.made` probe),
        // no later pass runs its action again.
        match_obj.mark_match_node_reduced();
        result
    }

    /// The walk's view of an eager (or already materialized) Match: the same
    /// [`ActionWalkNode`] a lazy node answers, read from its attribute map.
    // Cost: O(k), k = the node's capture slots.
    fn eager_action_walk_node(match_obj: &Value) -> Option<ActionWalkNode> {
        let ValueView::Instance { attributes, .. } = match_obj.view() else {
            return None;
        };
        let amap = attributes.as_map();
        let silent = match amap.get(wk::match_silent_caps()).map(Value::view) {
            Some(ValueView::Array(items, _)) => items.iter().cloned().collect(),
            _ => Vec::new(),
        };
        let mut walk = ActionWalkNode::eager(
            amap.get(wk::match_named()).cloned(),
            amap.get(wk::match_list()).cloned(),
            silent,
        );
        if let Some(ValueView::Hash(vars)) = amap.get(wk::match_reduce_time_vars()).map(Value::view)
        {
            walk.reduce_vars = vars.iter().map(|(k, v)| (k.clone(), v.clone())).collect();
        }
        if let Some(ValueView::Str(sym)) = amap.get(wk::match_sym_variant()).map(Value::view) {
            walk.sym = Some(Symbol::intern(&sym));
        }
        Some(walk)
    }

    /// Whether the actions object answers a method for the node: its
    /// `:sym<...>` variant's (returned as the second element) or the rule's
    /// own.
    fn node_action_method(
        &mut self,
        actions_class: Option<Symbol>,
        rule_name: &str,
        sym: Option<Symbol>,
    ) -> (bool, Option<String>) {
        let class_name = actions_class.map(|cn| cn.resolve());
        let sym_method = sym.map(|sym| {
            self.variant_action_method_name(class_name.as_deref(), rule_name, &sym.resolve())
        });
        let Some(class) = actions_class else {
            // Not a class we can ask: always try the call.
            return (true, sym_method);
        };
        let sym_hit = sym_method
            .as_deref()
            .is_some_and(|name| self.actions_answer_method_of(class, Symbol::intern(name)));
        let rule_hit =
            !rule_name.is_empty() && self.actions_answer_method_of(class, Symbol::intern(rule_name));
        (sym_hit || rule_hit, sym_method)
    }

    fn invoke_grammar_actions_unmarked(
        &mut self,
        match_obj: Value,
        actions: &mut Value,
        rule_name: &str,
    ) -> Result<Value, RuntimeError> {
        // Already dispatched at its reduction (ADR-7576): its subtree was too.
        if match_obj.match_node_reduced() {
            return Ok(match_obj);
        }
        let node = match match_obj.match_action_walk_node() {
            Some(node) => node,
            None => match Self::eager_action_walk_node(&match_obj) {
                Some(found) => found,
                None => return Ok(match_obj),
            },
        };
        let actions_class = Self::actions_class_sym(actions);
        let (has_own_action, sym_method_name) =
            self.node_action_method(actions_class, rule_name, node.sym);
        // A childless leaf whose rule has no action method — the overwhelming
        // majority of nodes in a leaf-heavy parse (one per matched character in
        // a quoted-scalar run): nothing below can have any effect for it.
        if !has_own_action && node.has_no_children() {
            return Ok(match_obj);
        }

        // Re-install this match's own `:my $*x` bindings before descending into
        // children. A parent-owned binding (for example `:my %*PLAYED = ();`
        // in `deal`) must remain live while child actions accumulate into it;
        // the child walk restores only bindings owned by the child itself.
        let mut saved_reduce_vars: Vec<(String, Option<Value>)> = Vec::new();
        for (k, v) in &node.reduce_vars {
            for env_key in Self::grammar_dynvar_env_keys(k) {
                if !saved_reduce_vars.iter().any(|(saved_key, _)| saved_key == &env_key) {
                    saved_reduce_vars.push((env_key.clone(), self.env.get(&env_key).cloned()));
                }
                self.env.insert(env_key, v.clone());
            }
        }
        if let Err(error) = self.walk_action_children(&node, actions) {
            self.restore_action_reduce_vars(&saved_reduce_vars);
            return Err(error);
        }
        // Interior node with no action method of its own: the children (and
        // silent captures) above are already dispatched, and none of the env
        // ceremony below can matter — no method will read `$/`/`$<x>`/`$0…`,
        // and `make` cannot run.
        if !has_own_action {
            self.restore_action_reduce_vars(&saved_reduce_vars);
            return Ok(match_obj);
        }
        let method_result = self.call_node_action(
            &match_obj,
            &node,
            actions,
            rule_name,
            sym_method_name.as_deref(),
        );
        self.restore_action_reduce_vars(&saved_reduce_vars);
        match method_result {
            Ok(_) => {}
            // No action method for this rule: silently skip. Only a miss on
            // the action method ITSELF counts -- a `No such method` raised
            // inside an action's body (`(1,2)».made`) is a real error and
            // propagates out of `.parse`, as in rakudo (#10703).
            Err(e)
                if e.is_method_not_found_for(rule_name)
                    || sym_method_name
                        .as_deref()
                        .is_some_and(|sym| e.is_method_not_found_for(sym)) => {}
            Err(e) => return Err(e),
        }
        // `make` already wrote `.made` into this node in place when the
        // action ran it on `$/`; a `make` that reached `action_made` some
        // other way (an action whose `$/` is not this node) lands here.
        if let Some(ast) = self.regex_state.action_made.take() {
            match_obj.set_match_ast_in_place(ast);
        }
        Ok(match_obj)
    }

    /// Dispatch the children of `node` that have not run their actions yet,
    /// in reduction order (bottom-up within each child).
    fn walk_action_children(
        &mut self,
        node: &ActionWalkNode,
        actions: &mut Value,
    ) -> Result<(), RuntimeError> {
        if node.has_named_hash && !(node.named_children.is_empty() && node.silent.is_empty()) {
            // A quantified named capture is stored as one Array under its
            // capture name, but grammar actions run at each cursor's reduce
            // point. Sorting the named-capture slots first therefore grouped
            // all `<line>` actions together ahead of an interleaved
            // `<section>` or `<include>` action. Stateful actions (including
            // parsers that maintain scope or interpolation variables) observe
            // the wrong order. The children are flattened, so sibling capture
            // types are interleaved by source position.
            let mut children: Vec<(Option<Symbol>, Value)> = node
                .named_children
                .iter()
                .map(|(name, child)| (Some(*name), child.clone()))
                .collect();
            // A silent subrule still reduces at its position in the pattern.
            // It has no named slot, but its action must run among the named
            // captures rather than after all of them.
            children.extend(node.silent.iter().map(|item| (None, item.clone())));
            // A zero-width hidden subrule and the following capture can share
            // a start position. The earlier reduction has the earlier end;
            // both seam reads leave lazy Match children unmaterialized.
            children.sort_by_key(|(_, v)| (v.match_to().unwrap_or(0), v.match_from().unwrap_or(0)));
            // A non-suppressing alias (`<x=rule>`) files ONE capture node under
            // both `x` and `rule`, so the same cursor appears twice among these
            // siblings. Rakudo dispatches an action per reduced cursor, not per
            // capture slot; dispatching per slot fired the whole subtree's
            // actions twice per alias level, compounding to 2^depth (256x on
            // `benchmarks/bench-yaml-parse.raku`). Dispatch once per node; the
            // `.made` it writes is the node's, so both slots read it.
            let mut seen: Vec<usize> = Vec::new();
            for (child_name, child_match) in children {
                let Some(child_name) = child_name else {
                    let dispatch_name = Self::get_action_name(&child_match).unwrap_or_default();
                    self.invoke_grammar_actions(child_match, actions, &dispatch_name)?;
                    continue;
                };
                let dispatch_name =
                    Self::get_action_name(&child_match).unwrap_or_else(|| child_name.resolve());
                self.invoke_grammar_actions_once(&mut seen, child_match, actions, &dispatch_name)?;
            }
        }

        // Recurse into positional `( )` group captures the same way. A group
        // itself has no action method (dispatch name "" unless the capture
        // carries an aliased `action_name`), but the named subrules matched
        // INSIDE it do — DBDish::Pg's tokenizer is `( <normal> | <placeholder>
        // )*` with per-token actions — and their `.made` lands on the Match
        // objects reachable via `$0[…]`.
        for item in &node.positional_children {
            let dispatch_name = Self::get_action_name(item).unwrap_or_default();
            self.invoke_grammar_actions(item.clone(), actions, &dispatch_name)?;
        }

        // A node without a named-capture hash still needs to dispatch its
        // hidden subrules, in source order. Nodes with that hash handled them
        // above, interleaved with the captured children.
        if !node.has_named_hash && !node.silent.is_empty() {
            let mut items = node.silent.clone();
            items.sort_by_key(|m| m.match_from().unwrap_or(0));
            for item in items {
                let dispatch_name = Self::get_action_name(&item).unwrap_or_default();
                self.invoke_grammar_actions(item, actions, &dispatch_name)?;
            }
        }
        Ok(())
    }

    /// The action method the node dispatches to, when it reads its captures
    /// only through a `$/` it binds itself: a compiled user method with a
    /// positional `$/` parameter, unwrapped. Its body and every closure in it
    /// then resolve `$/`, `$<name>` and `$0` to that parameter, and a method
    /// has its own `$_`, so nothing in it can observe the env ceremony of
    /// [`Self::call_node_action`] (#18207). Returns the method's name and its
    /// resolution, so the call does not resolve it again.
    // Cost: O(p), p = the method's parameters, plus one memoized method
    // resolution.
    fn action_binding_own_match_var(
        &mut self,
        actions: &Value,
        match_obj: &Value,
        rule_name: &str,
        sym_method_name: Option<&str>,
    ) -> Option<(Symbol, Symbol, std::sync::Arc<crate::runtime::MethodDef>)> {
        let class = Self::actions_class_sym(actions)?;
        if self.has_any_wrap_chains() {
            return None;
        }
        let method = match sym_method_name {
            Some(sym) if self.actions_answer_method_of(class, Symbol::intern(sym)) => sym,
            _ => rule_name,
        };
        let method_sym = Symbol::intern(method);
        let args = std::slice::from_ref(match_obj);
        let (owner, def) = self
            .resolve_method_cached(class.as_str(), method, class, method_sym, args, actions)
            .ok()??;
        let binds = !def.is_multi
            && def.compiled_code.is_some()
            && def
                .param_defs
                .iter()
                .any(|param| !param.named && param.name == "/");
        binds.then_some((method_sym, owner, def))
    }

    /// Call the node's own action method with `$/`, `$_` and the node's
    /// `$<name>`/`$0..` installed, and the enclosing ones put back after.
    /// An action that binds its own `$/` cannot observe any of that, so it is
    /// called directly.
    fn call_node_action(
        &mut self,
        match_obj: &Value,
        node: &ActionWalkNode,
        actions: &mut Value,
        rule_name: &str,
        sym_method_name: Option<&str>,
    ) -> Result<Value, RuntimeError> {
        self.env.remove_sym(wk::made());
        self.regex_state.action_made = None;
        if let Some((method, owner, def)) =
            self.action_binding_own_match_var(actions, match_obj, rule_name, sym_method_name)
        {
            return self.call_action_method_resolved(actions, method, owner, &def, match_obj);
        }
        self.env.insert_sym(wk::match_var(), match_obj.clone());
        // Hide the parent's named and positional captures from this action,
        // and install the node's own so `$<a>` and `$0` work inside it.
        let saved_named_captures = self.take_action_named_captures();
        for (k, v) in node.named_values() {
            self.env
                .insert_sym(super::methods_grammar_action_env::angle_key_sym(k), v);
        }
        let saved_positional = self.take_action_positional_captures();
        for (i, v) in node.positional_values().into_iter().enumerate() {
            self.env
                .insert_sym(super::methods_grammar_action_env::positional_key_sym(i), v);
        }
        // Also set $_ to the match (for `.make:` syntax)
        let saved_topic = self.env.get_sym(wk::topic()).cloned();
        self.env.insert_sym(wk::topic(), match_obj.clone());

        let (_, method_result) =
            self.call_grammar_action_method(actions, rule_name, sym_method_name, match_obj);

        match saved_topic {
            Some(old_topic) => {
                self.env.insert_sym(wk::topic(), old_topic);
            }
            None => {
                self.env.remove_sym(wk::topic());
            }
        }
        self.restore_action_named_captures(saved_named_captures);
        self.restore_action_positional_captures(saved_positional);
        method_result
    }
}
