use super::*;

/// (pre_phasers, enter_phasers, success_queue, failure_queue, post_phasers, body_main)
///
/// `body_main` borrows the input when the block holds no phaser that is
/// split off, which is the common case (a module mainline is hundreds of
/// statements, and copying them on every load was a measurable share of it).
pub(super) type SplitPhasers<'a> = (
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    Vec<Stmt>,
    std::borrow::Cow<'a, [Stmt]>,
);

impl Interpreter {
    // Cost: O(n) to scan, n = statements; the body is only copied when a
    // phaser is split off.
    /// Whether `stmt` is a block phaser [`Self::split_block_phasers`] takes
    /// out of the body.
    fn splits_off(stmt: &Stmt) -> bool {
        matches!(
            stmt,
            Stmt::Phaser {
                kind: PhaserKind::Pre
                    | PhaserKind::Enter
                    | PhaserKind::Post
                    | PhaserKind::Leave
                    | PhaserKind::Keep
                    | PhaserKind::Undo,
                ..
            }
        )
    }

    /// Whether [`Self::split_block_phasers`] would split anything off `stmts`.
    // Cost: O(n), n = top-level statements.
    pub(super) fn block_has_split_phasers(stmts: &[Stmt]) -> bool {
        stmts.iter().any(Self::splits_off)
    }

    /// Move the bare top-level `our` declarations (`our @log;`, no initializer)
    /// out of `body`, in source order, so they can run ahead of the block's
    /// ENTER phasers. rakudo installs an `our` symbol when the unit is
    /// compiled, so an ENTER sees the same package container the later
    /// declaration (which only re-loads it) gives the body.
    // Cost: O(n), n = top-level statements.
    pub(crate) fn take_bare_our_decls(body: &mut Vec<Stmt>) -> Vec<Stmt> {
        let is_bare_our = |stmt: &Stmt| {
            matches!(
                stmt,
                Stmt::VarDecl { is_our: true, expr, has_initializer: false, .. }
                    if crate::compiler::Compiler::is_synthesized_decl_default(expr)
            )
        };
        if !body.iter().any(is_bare_our) {
            return Vec::new();
        }
        let (decls, rest) = std::mem::take(body).into_iter().partition(is_bare_our);
        *body = rest;
        decls
    }

    pub(super) fn split_block_phasers<'a>(&self, stmts: &'a [Stmt]) -> SplitPhasers<'a> {
        if !Self::block_has_split_phasers(stmts) {
            return (
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                Vec::new(),
                std::borrow::Cow::Borrowed(stmts),
            );
        }
        let mut pre_ph = Vec::new();
        let mut enter_ph = Vec::new();
        let mut body_main = Vec::new();
        let mut success_queue = Vec::new();
        let mut failure_queue = Vec::new();
        let mut post_ph = Vec::new();
        for stmt in stmts {
            if let Stmt::Phaser { kind, body, .. } = stmt {
                match kind {
                    // PRE/POST keep their original `Stmt::Phaser` node so the
                    // captured `condition` source text survives — every other
                    // kind is reduced to its body, which is all its consumers
                    // need. `phaser_body_and_condition` unwraps them again.
                    PhaserKind::Pre => pre_ph.push(stmt.clone()),
                    PhaserKind::Enter => enter_ph.push(Stmt::Block(body.clone())),
                    PhaserKind::Post | PhaserKind::Leave | PhaserKind::Keep | PhaserKind::Undo => {}
                    _ => body_main.push(stmt.clone()),
                }
            } else {
                body_main.push(stmt.clone());
            }
        }
        // POST phasers in reverse source order
        for stmt in stmts.iter().rev() {
            if let Stmt::Phaser { kind, body, .. } = stmt {
                match kind {
                    PhaserKind::Leave => {
                        success_queue.push(Stmt::Block(body.clone()));
                        failure_queue.push(Stmt::Block(body.clone()));
                    }
                    PhaserKind::Keep => success_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Undo => failure_queue.push(Stmt::Block(body.clone())),
                    PhaserKind::Post => post_ph.push(stmt.clone()),
                    _ => {}
                }
            }
        }
        (
            pre_ph,
            enter_ph,
            success_queue,
            failure_queue,
            post_ph,
            std::borrow::Cow::Owned(body_main),
        )
    }

    /// Unwrap a phaser statement as `split_block_phasers` hands it back: the
    /// body to evaluate, plus a `PRE`/`POST` phaser's verbatim condition source
    /// text (the empty string when there is none — an old AST, or a body that
    /// arrived as a bare `Stmt::Block`).
    pub(super) fn phaser_body_and_condition(stmt: &Stmt) -> (&[Stmt], &str) {
        match stmt {
            Stmt::Phaser {
                body, condition, ..
            } => (
                body,
                condition.as_ref().map(Symbol::as_str).unwrap_or_default(),
            ),
            Stmt::Block(body) => (body, ""),
            other => (std::slice::from_ref(other), ""),
        }
    }

    /// A `^lookup`/`^find_method`/`.can` result for a *multi* method is a
    /// dispatcher-shaped Sub: its env carries `__mutsu_lookup_class` /
    /// `__mutsu_lookup_method` but no `__mutsu_lookup_candidate_idx` (a
    /// single resolved candidate carries the idx). The Sub's params/body are
    /// only the first candidate's, so invoking it as a value must not bind
    /// that signature — Raku re-dispatches among the whole candidate family
    /// with the first argument as the invocant. Returns the method name when
    /// `data` has that dispatcher shape.
    pub(crate) fn sub_multi_method_dispatcher_name(data: &crate::value::SubData) -> Option<String> {
        // Symbol-keyed throughout: this runs on EVERY indirect call
        // (`$code(...)`, `.&meth`) on the way to the compiled closure path, and
        // the string-keyed `Env::get` re-interned each literal per call -- three
        // thread-local string hashes for a probe that misses for every ordinary
        // block (#8302).
        //
        // A WhateverCode's callable type is `WhateverCode`, never the `Method`
        // a dispatcher value carries; its compiled code says so without the
        // env probes and string compare below (#13286).
        if data
            .compiled_code
            .as_ref()
            .is_some_and(|cc| cc.is_whatever_code)
        {
            return None;
        }
        if data
            .env
            .get_sym(crate::symbol::well_known::lookup_candidate_idx())
            .is_some()
        {
            return None;
        }
        match data
            .env
            .get_sym(crate::symbol::well_known::callable_type())
            .map(Value::view)
        {
            Some(ValueView::Str(t)) if t.as_str() == "Method" || t.as_str() == "Submethod" => {}
            _ => return None,
        }
        data.env
            .get_sym(crate::symbol::well_known::lookup_class())?;
        match data
            .env
            .get_sym(crate::symbol::well_known::lookup_method())
            .map(Value::view)
        {
            Some(ValueView::Str(m)) => Some(m.to_string()),
            _ => None,
        }
    }

    /// A role-mixed callable (`&foo but R1`, `&foo.^mixin(R1)`) wraps a
    /// Sub/Routine/WeakSub in a `ValueView::Mixin`; every call site that
    /// needs to actually invoke a callable, or decide whether a value IS
    /// one, must look through that wrapper the same way `mixin_iteration_target`
    /// unwraps a Mixin-wrapped collection for `.map`/`.grep` iteration.
    /// Without this, `(1,2,3).map(&double but R1)` either failed a
    /// callable pre-check or died with "Callable expected" once past it
    /// (see `todo/tickets/map-rejects-role-mixed-sub-as-callable.md`).
    pub(crate) fn unwrap_callable_mixin(func: Value) -> Value {
        if let ValueView::Mixin(inner, _) = func.view()
            && matches!(
                inner.view(),
                ValueView::Sub(_) | ValueView::Routine { .. } | ValueView::WeakSub(_)
            )
        {
            inner.as_ref().clone()
        } else {
            func
        }
    }

    /// Whether `captured` -- the candidates a multi dispatcher code value
    /// carries -- are exactly the ones the family `name` resolves to right now.
    /// A compiled candidate is the live one only when the bodies match too: a
    /// later block's own `multi q(Int)` has the same package and name as the
    /// one a closure captured from an earlier block, and is not its candidate.
    // Cost: O(c), c = candidates in the family.
    pub(crate) fn captured_candidates_are_live(&self, name: &str, captured: &[Value]) -> bool {
        let live = self.resolve_all_multi_candidates(name);
        live.len() == captured.len()
            && live.iter().zip(captured).all(|(live, captured)| {
                matches!(
                    captured.view(),
                    ValueView::Sub(data)
                        if data.package == live.package
                            && data.name == live.name
                            && match (&data.compiled_routine, live.compiled_fn()) {
                                (Some(c), Some(l)) => c.fingerprint == l.fingerprint,
                                _ => true,
                            }
                )
            })
    }

    /// Whether every candidate in `captured` is still one of the candidates
    /// the family `name` resolves to right now: the family only GREW since
    /// the dispatcher was captured (a `multi` declared after
    /// `&f.wrap(...)` or `my &g = &f`, which rakudo sees at compile time,
    /// #15925). The same identity test as
    /// [`Self::captured_candidates_are_live`], applied per candidate.
    // Cost: O(c * k), c = live candidates, k = captured candidates.
    pub(crate) fn captured_candidates_are_live_subset(
        &self,
        name: &str,
        captured: &[Value],
    ) -> bool {
        let live = self.resolve_all_multi_candidates(name);
        !captured.is_empty()
            && captured.iter().all(|captured| {
                let ValueView::Sub(data) = captured.view() else {
                    return false;
                };
                live.iter().any(|live| {
                    data.package == live.package
                        && data.name == live.name
                        && match (&data.compiled_routine, live.compiled_fn()) {
                            (Some(c), Some(l)) => c.fingerprint == l.fingerprint,
                            _ => true,
                        }
                })
            })
    }

    /// Whether `v` is a multi dispatcher code value of the family `name`.
    // Cost: O(1).
    pub(crate) fn is_family_dispatcher_of(v: &Value, name: &str) -> bool {
        let ValueView::Sub(data) = v.view() else {
            return false;
        };
        matches!(
            data.env.get("__mutsu_multi_dispatch_name").map(Value::view),
            Some(ValueView::Str(family)) if family.as_ref() == name
        )
    }

    /// A multi dispatcher code value of the family `name` that calling would
    /// send straight back into resolution of `name` (`&trait_mod:<is>`
    /// exported by `sub EXPORT`): either its captured candidates are the live
    /// ones, or a by-name call is already running through a dispatcher of that
    /// family. A by-name call that found no candidate to bind must not retry
    /// through it: the retry resolves the same family again, forever.
    // Cost: O(c), c = candidates in the family.
    pub(crate) fn is_live_family_dispatcher(&self, v: &Value, name: &str) -> bool {
        if !Self::is_family_dispatcher_of(v, name) {
            return false;
        }
        let ValueView::Sub(data) = v.view() else {
            return false;
        };
        if self
            .dispatch
            .family_dispatchers_in_flight
            .contains(&Symbol::intern(name))
        {
            return true;
        }
        data.env
            .get("__mutsu_multi_dispatch_candidates")
            .cloned()
            .and_then(Value::into_array)
            .is_some_and(|(candidates, _)| self.captured_candidates_are_live(name, &candidates))
    }
    /// A captured candidate's env is a clone of the one live when `&name` was
    /// evaluated, so it has no `&name`: a by-name call in its body (`b($n-1)`)
    /// cannot reach its own family once the declaring scope is gone. Bind the
    /// dispatcher being called under `&name` for this call (the Subs are
    /// immutable once built, so a standing back-reference is not an option).
    // Cost: O(e), e = size of the candidate's captured env (only when it lacks `&name`).
    fn bind_family_dispatcher(candidate: &Value, family: Option<&str>, dispatcher: &Value) -> Value {
        let (Some(family), ValueView::Sub(cand)) = (family, candidate.view()) else {
            return candidate.clone();
        };
        let amp = format!("&{family}");
        if cand.env.contains_key(&amp) {
            return candidate.clone();
        }
        let mut next: crate::value::SubData = (**cand).clone();
        next.env.insert(amp, dispatcher.clone());
        Value::sub_value(crate::gc::Gc::new(next))
    }

    /// A Junction argument that no candidate of a captured family takes as a
    /// whole is autothreaded, one call of the family per eigenstate -- as a
    /// by-name call of a registry multi threads it. `None` when no argument
    /// is a Junction, or some candidate accepts it (`Mu`, `Junction`, a
    /// slurpy).
    // Cost: O(c * p) to decide, c = candidates, p = parameters; then one
    // dispatch per eigenstate of the threaded Junction.
    fn autothread_captured_candidates(
        &mut self,
        func: &Value,
        candidates: &[Value],
        family_name: Option<&str>,
        args: &[Value],
    ) -> Result<Option<Value>, RuntimeError> {
        let junction_indices: Vec<usize> = args
            .iter()
            .enumerate()
            .filter(|(_, v)| Self::extract_junction_from_arg(v).is_some())
            .map(|(i, _)| i)
            .collect();
        if junction_indices.is_empty() {
            return Ok(None);
        }
        let mut seen_candidate = false;
        for candidate in candidates {
            let ValueView::Sub(data) = candidate.view() else {
                continue;
            };
            seen_candidate = true;
            if self
                .autothread_indices_for_params(args, &junction_indices, &data.param_defs)
                .is_empty()
            {
                return Ok(None);
            }
        }
        if !seen_candidate {
            return Ok(None);
        }
        let idx = self.pick_autothread_junction_index(args, &junction_indices);
        let Some((kind, values, pair_key)) = Self::extract_junction_from_arg(&args[idx]) else {
            return Ok(None);
        };
        let mut results = Vec::with_capacity(values.len());
        for eigenstate in values.iter() {
            let mut threaded = args.to_vec();
            threaded[idx] = match &pair_key {
                Some(key) => Value::pair(key.clone(), eigenstate.clone()),
                None => eigenstate.clone(),
            };
            results.push(self.call_captured_candidates(func, candidates, family_name, threaded)?);
        }
        Ok(Some(Value::junction(kind, results)))
    }

    /// Dispatch `call_args` over the candidates `candidates` of the
    /// captured-candidate dispatcher `func` (family `family_name`), ranked as
    /// a by-name call ranks them (#12474), without resolving the family by
    /// name: the `{*}` of a proto the dispatcher ran (ADR-8751 stage 2), or a
    /// call made where the family is out of scope.
    // Cost: O(c * p), c = candidates, p = parameters bound per candidate.
    pub(crate) fn call_captured_candidates(
        &mut self,
        func: &Value,
        candidates: &[Value],
        family_name: Option<&str>,
        call_args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        if let Some(threaded) =
            self.autothread_captured_candidates(func, candidates, family_name, &call_args)?
        {
            return Ok(threaded);
        }
        if let Some((candidate, remaining)) = self.pick_captured_multi_candidate(
            family_name.unwrap_or(""),
            candidates,
            &call_args,
        )? {
            // An imported increment candidate joins the native increment's
            // core candidates and takes the call only when it out-ranks them,
            // as a registry candidate does (ADR-8751 stage 2).
            if let Some(family) = family_name
                && let Some(op) =
                    crate::runtime::native_increment_dispatch::IncrementOp::from_routine_name(
                        family,
                    )
                && let [arg] = call_args.as_slice()
                && let ValueView::Sub(data) = candidate.view()
                && let Some(def) = self
                    .module
                    .lexical_operator_candidate_defs
                    .get(&data.id)
                    .cloned()
                && self.core_increment_wins_over(&def, &call_args)
            {
                return self.run_core_increment(op, arg, None);
            }
            let candidate = Self::bind_family_dispatcher(&candidate, family_name, func);
            return self.call_captured_multi_winner(
                family_name.unwrap_or(""),
                candidate,
                remaining,
                call_args,
                func.clone(),
            );
        }
        // Slurpy catch-all
        for candidate in candidates {
            if let ValueView::Sub(cand_data) = candidate.view()
                && cand_data.param_defs.iter().any(|pd| pd.slurpy)
            {
                let candidate = Self::bind_family_dispatcher(candidate, family_name, func);
                    return self.call_sub_value(candidate, call_args);
            }
        }
        // User candidates of a built-in operator add to it: a call none of
        // them accepts is the operator's own (ADR-8751 stage 2).
        if let Some(family) = family_name
            && let ValueView::Sub(data) = func.view()
            && data
                .env
                .get_sym(crate::symbol::wk::multi_dispatch_builtin_fallback())
                .is_some()
        {
            return self.call_builtin_operator(family, &call_args);
        }
        // None of the captured candidates accepts the call. For a family
        // that is X::Multi::NoMatch, not the first candidate's own
        // binding error: a caller that treats "no candidate" as "try
        // something else" (a trait application through an imported
        // `&trait_mod:<is>`) tells the two apart by it.
        if candidates.len() > 1
            && let Some(family) = family_name
        {
            return Err(self.multi_no_match_error(family, &call_args));
        }
        if let Some(candidate) = candidates.first() {
            let candidate = Self::bind_family_dispatcher(candidate, family_name, func);
                    return self.call_sub_value(candidate, call_args);
        }
        Err(self.multi_no_match_error(family_name.unwrap_or(""), &call_args))
    }

    /// The winner among the candidates `candidates` of the captured-candidate
    /// dispatcher `func` (family `family`) for `args`, called as a multi
    /// candidate (a `callsame` in it reaches the next one, and past the last
    /// the built-in operator), or `None` when none accepts them: the user
    /// candidates of a built-in operator a block-scoped `use` imported, which
    /// decline to the operator's own body (ADR-8751 stage 2).
    // Cost: O(c * p) to rank, c = candidates, p = parameters bound per
    // candidate, plus the winner's call.
    pub(crate) fn try_captured_candidates(
        &mut self,
        func: &Value,
        candidates: &[Value],
        family: &str,
        args: Vec<Value>,
    ) -> Result<Option<Value>, RuntimeError> {
        let Some((candidate, remaining)) =
            self.pick_captured_multi_candidate(family, candidates, &args)?
        else {
            return Ok(None);
        };
        // The winner joins the operator's core candidates and takes the call
        // only when it out-narrows them, as a registry candidate does
        // (ADR-0071).
        if let [left, right] = args.as_slice()
            && let ValueView::Sub(data) = candidate.view()
            && let Some(def) = self
                .module
                .lexical_operator_candidate_defs
                .get(&data.id)
                .cloned()
        {
            match self.core_infix_verdict(family, &def, left, right) {
                super::native_infix_dispatch::CoreInfixVerdict::Core => return Ok(None),
                super::native_infix_dispatch::CoreInfixVerdict::Tie(core) => {
                    return Err(self.core_infix_tie_error(family, &def, core, left, right));
                }
                super::native_infix_dispatch::CoreInfixVerdict::User => {}
            }
        }
        let candidate = Self::bind_family_dispatcher(&candidate, Some(family), func);
        self.call_captured_multi_winner(family, candidate, remaining, args, func.clone())
            .map(Some)
    }

    /// The built-in operator `name` (`infix:<+>`, `prefix:<->`) itself,
    /// without the user candidates a by-name call would also consider: the
    /// fallback of a dispatcher over imported candidates of the operator,
    /// whose own `&name` binding a by-name call would find again.
    // Cost: the operator's own cost.
    // Cost: O(1) plus the operator's own work.
    fn call_builtin_operator(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        // The built-in operator's own evaluation (it may compile the operator
        // as an expression) must not dispatch back to the user candidates that
        // fell back to it.
        self.module.builtin_op_fallback_depth += 1;
        let result = self.call_builtin_operator_inner(name, args);
        self.module.builtin_op_fallback_depth -= 1;
        result
    }

    fn call_builtin_operator_inner(
        &mut self,
        name: &str,
        args: &[Value],
    ) -> Result<Value, RuntimeError> {
        if let Some(op) = name.strip_prefix("infix:<").and_then(|s| s.strip_suffix('>')) {
            return self.call_infix_routine(Self::normalize_unicode_infix(op), args);
        }
        if let Some(op) = crate::runtime::native_increment_dispatch::IncrementOp::from_routine_name(name)
            && let [arg] = args
        {
            return self.run_core_increment(op, arg, None);
        }
        if let Some(op) = name.strip_prefix("prefix:<").and_then(|s| s.strip_suffix('>'))
            && let [arg] = args
            && let Some(result) = self.core_prefix_op(op, arg)
        {
            return result;
        }
        if let Some(op) = name.strip_prefix("postfix:<").and_then(|s| s.strip_suffix('>'))
            && let [arg] = args
            && let Some(result) = self.core_postfix_op(op, arg)
        {
            return result;
        }
        match name {
            "postcircumfix:<[ ]>" => return self.builtin_postcircumfix_subscript(args, true),
            "postcircumfix:<{ }>" => return self.builtin_postcircumfix_subscript(args, false),
            _ => {}
        }
        Err(self.multi_no_match_error(name, args))
    }

    /// The `{*}` of a proto a captured-candidate dispatcher ran
    /// ([`Self::push_proto_dispatch_frame_over`]): `args` go to that
    /// dispatcher's own candidates (ADR-8751 stage 2).
    // Cost: O(c * p), as [`Self::call_captured_candidates`].
    pub(crate) fn call_proto_frame_dispatcher(
        &mut self,
        dispatcher: &Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        let ValueView::Sub(data) = dispatcher.view() else {
            return self.call_sub_value(dispatcher.clone(), args);
        };
        let candidates: Vec<Value> = data
            .env
            .get_sym(crate::symbol::wk::multi_dispatch_candidates())
            .cloned()
            .and_then(Value::into_array)
            .map(|(items, _)| items.iter().cloned().collect())
            .unwrap_or_default();
        let family = match data.env.get("__mutsu_multi_dispatch_name").map(Value::view) {
            Some(ValueView::Str(name)) => Some(name.to_string()),
            _ => None,
        };
        self.call_captured_candidates(dispatcher, &candidates, family.as_deref(), args)
    }

    pub(crate) fn call_sub_value(
        &mut self,
        func: Value,
        args: Vec<Value>,
    ) -> Result<Value, RuntimeError> {
        // ADR-0058: a slurpy parameter FLATTENS a `Seq` argument
        // (`{ [+] @_ } o *.map(* * 2)` sums the mapped elements, not the Seq),
        // and the binder reads those elements through pure code. Pull a
        // still-deferred `.map`/`.grep` argument here rather than teaching
        // every binding shape about it. A `MapGrep` source is always finite
        // (its items were materialized at the `.map` call), so this cannot
        // hang; it costs raku's argument-position laziness, which mutsu never
        // had for `.map` anyway.
        self.reify_map_grep_seq_args(&args)?;
        let func = Self::unwrap_callable_mixin(func);
        // A native/accessor Method object runs its own candidate, not
        // whatever the invocant's class overrides it with (#10344).
        if let Some(result) = self.try_call_bound_method_object(&func, &args) {
            return result;
        }
        // A `Method`/`Submethod` object (`.^find_method`, `.^method_table`)
        // used as a plain callable -- e.g. as a `.wrap` wrapper, which the
        // wrap chain invokes through here -- runs the callable it carries,
        // the same one `CALL-ME` invokes. A multi dispatcher carries none and
        // is left to the paths below.
        let method_callable = match func.view() {
            ValueView::Instance {
                class_name,
                attributes,
                ..
            } if matches!(class_name.as_str(), "Method" | "Submethod") => {
                attributes.as_map().get("__mutsu_method_callable").cloned()
            }
            _ => None,
        };
        let func = method_callable.unwrap_or(func);
        // Upgrade WeakSub to Sub transparently
        let func = match func.view() {
            ValueView::WeakSub(weak) => match weak.upgrade() {
                Some(strong) => Value::sub_value(strong),
                None => return Err(RuntimeError::new("Callable has been freed")),
            },
            _ => func,
        };
        if let ValueView::Routine {
            package,
            name,
            is_regex,
            ..
        } = func.view()
        {
            // A token/rule method value called with a cursor (`$meth($c)`,
            // e.g. from a custom grammar HOW's `find_method` wrapper) runs
            // the token at the cursor position and returns a Match.
            if is_regex
                && let Some(res) = self.try_call_token_method_value(package, &name.resolve(), &args)
            {
                return res;
            }
            // A cursor-protocol method value (`Match.^lookup("!cursor_init")`,
            // #7883) is a METHOD, and `Match` is a builtin type with no
            // `registry().classes` entry -- so the `has_class` method-dispatch
            // fallback further down never fires for it and the name fell
            // through to "Unknown function". Re-dispatch it on its first
            // argument, which is the type object the cursor belongs to.
            if super::regex::regex_cursor::is_cursor_protocol_method(&name.resolve())
                && !args.is_empty()
            {
                let mut args = args;
                let invocant = args.remove(0);
                return self.call_method_with_values(invocant, &name.resolve(), args);
            }
            if !crate::qualified::is_global_package(package) {
                let fq = crate::qualified::qualified_text(package, name)
                    .as_str()
                    .to_string();
                if self.resolve_function(&fq).is_some() {
                    return self.call_function(&fq, args);
                }
            }
            // A native method object of a core type (`Map.^lookup('new')`,
            // `Map.^lookup('raku')`): it has no registry class, and a core
            // routine of the same name (`keys`) is not the method, so it
            // dispatches on its invocant here, ahead of the function lookup.
            // The native constructor / qualified native method is used, not
            // `invocant.name`: a subclass overriding it by calling this object
            // (`method new(|c) { &new(self, |c) }`) would re-enter itself.
            // The catalog probe is O(1); `is_builtin_type_method` walks the
            // type's ancestors, so it only runs for a package that is a core
            // type at all.
            if !args.is_empty()
                && !self.has_class(&package.resolve())
                && crate::builtin_types::catalog::builtin_type_info(&package.resolve()).is_some()
            {
                let pkg = package.resolve();
                let method = name.resolve();
                // A method of a core type with no catalog row (`Match.pos`,
                // `.from`, `.orig`) still dispatches on an invocant of that type.
                if self.is_builtin_type_method(&pkg, &method)
                    || self.type_matches_value(&pkg, &args[0])
                {
                    let mut args = args;
                    let invocant = args.remove(0);
                    // A role mixed into a native value (`my %h does R`, whose
                    // `R` overrides `AT-KEY`): the method object names the
                    // core type's own method, so it runs on the native base,
                    // not through the override that called it (WriteOnceHash's
                    // `my &AT-KEY := ::?CLASS.^find_method('AT-KEY')`).
                    if matches!(invocant.view(), ValueView::Mixin(..))
                        && let Some(result) = self.mixin_base_native_entry(&invocant, &method, &args)
                    {
                        return result;
                    }
                    return match invocant.view() {
                        ValueView::Package(_) if method == "new" => {
                            self.dispatch_native_new(invocant, args)
                        }
                        _ => self.call_method_with_values(invocant, &method, args),
                    };
                }
            }
            let name_str = name.resolve();
            if self.resolve_function(&name_str).is_some()
                || self.has_proto(&name_str)
                || self.has_multi_candidates(&name_str)
            {
                return self.call_function(&name_str, args);
            }
            // Method dispatch fallback for &?ROUTINE.dispatcher()(self, ...)
            // Only use this when the package is a known class.
            let pkg = package.resolve();
            if !args.is_empty()
                && !crate::qualified::is_global_package(crate::qualified::known_symbol(&pkg))
                && self.has_class(&pkg)
            {
                let invocant = args[0].clone();
                let method_args = args[1..].to_vec();
                return self.call_method_with_values(invocant, &name_str, method_args);
            }
            return self.call_function(&name_str, args);
        }
        if let ValueView::Junction { kind, values } = func.view() {
            let mut results = Vec::with_capacity(values.len());
            for callable in values.iter() {
                results.push(self.call_sub_value(callable.clone(), args.clone())?);
            }
            return Ok(Value::junction(kind, results));
        }
        if let ValueView::Sub(data) = func.view() {
            // The terminal of a wrapped grammar token/rule/regex is a
            // synthetic Sub created by `regex_token_method`. It must bypass
            // ordinary Sub binding and run the token against the cursor that
            // the wrapper received; `callsame` has already advanced to this
            // terminal through the normal wrap-dispatch frame.
            let token_terminal = data
                .env
                .get("__mutsu_token_method_wrapper_package")
                .and_then(|pkg| pkg.as_str())
                .zip(
                    data.env
                        .get("__mutsu_token_method_wrapper_name")
                        .and_then(|name| name.as_str()),
                )
                .map(|(pkg, name)| (pkg.to_string(), name.to_string()));
            if let Some((pkg, name)) = token_terminal {
                // A wrapped `.parse` start rule: re-run the whole parse.
                if let Some(method) = data
                    .env
                    .get("__mutsu_token_method_wrapper_parse_method")
                    .and_then(|m| m.as_str())
                    .map(str::to_string)
                {
                    let parse_args: Vec<Value> = match data
                        .env
                        .get("__mutsu_token_method_wrapper_parse_args")
                        .map(Value::view)
                    {
                        Some(ValueView::Array(items, _)) => items.iter().cloned().collect(),
                        _ => Vec::new(),
                    };
                    let parsed =
                        self.run_wrapped_start_rule_terminal(&pkg, &method, &parse_args)?;
                    return Ok(Self::token_wrap_result_or_failure(
                        &pkg,
                        args.first(),
                        parsed,
                    ));
                }
                let Some(cursor) = args.first() else {
                    return Ok(Value::NIL);
                };
                let Some((orig, pos, _)) = Self::cursor_call_position(cursor) else {
                    return Ok(Value::NIL);
                };
                // The cursor's own subject when it has one, so a wrapped
                // recursion runs every level against one target (#11404).
                let target = cursor
                    .match_target()
                    .unwrap_or_else(|| crate::value::regex_caps::MatchTarget::new(&orig));
                let matched = self.run_token_method_on_target(
                    crate::symbol::Symbol::intern(&pkg),
                    &name,
                    &args[1..],
                    target,
                    pos,
                )?;
                return Ok(Self::token_wrap_result_or_failure(
                    &pkg,
                    args.first(),
                    matched,
                ));
            }
            // Multi-method dispatcher Sub (`^find_method`/`.can` on a multi):
            // re-dispatch with args[0] as invocant instead of binding the
            // first candidate's signature (see sub_multi_method_dispatcher_name).
            if !args.is_empty()
                && let Some(meth) = Self::sub_multi_method_dispatcher_name(&data)
            {
                let mut args = args;
                let invocant = args.remove(0);
                return self.call_method_with_values(invocant, &meth, args);
            }
            // Check for wrap chain — if wrappers exist, dispatch through them.
            // A callsame/callwith invocation of the original (or an inner
            // wrapper) sets `wrap_skip_once` so exactly that call runs the sub
            // directly; a fresh named call — including a *recursive* one from
            // inside the original body — re-enters the chain like Raku does.
            let skip_chain_once = self.dispatch.wrap_skip_once.take() == Some(data.id);
            if !skip_chain_once
                && let Some(chain) = self.dispatch.wrap_chains.get(&data.id).cloned()
                && !chain.is_empty()
                // A nextcallee-returned wrappee is a direct code body: it
                // must not re-enter the wrapper chain.
                && !crate::runtime::code_do_attr::is_direct(&data)
            {
                let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
                self.test_pending_callsite_line = callsite_line;
                // Build remaining list: inner wrappers then original sub
                // chain is ordered inner-to-outer (last = outermost), so:
                // outermost is last, we call it; remaining = [inner..., original]
                let outermost = chain.last().unwrap().1.clone();
                let mut remaining: Vec<Value> = Vec::new();
                // Add wrappers from second-to-last down to first (inner order)
                for i in (0..chain.len() - 1).rev() {
                    remaining.push(chain[i].1.clone());
                }
                // Add the original sub at the end
                remaining.push(Value::sub_value(data.clone()));
                let frame = super::WrapDispatchFrame {
                    sub_id: data.id,
                    remaining,
                    args: sanitized_args.clone(),
                    arg_sources: self.pending_call_arg_sources().cloned(),
                    dispatch_token: 0,
                };
                self.push_wrap_dispatch_frame(frame);
                let result = self.call_method_wrapper(outermost, sanitized_args);
                self.pop_wrap_dispatch_frame();
                return result;
            }
            let (sanitized_args, callsite_line) = self.sanitize_call_args(&args);
            self.test_pending_callsite_line = callsite_line;
            let mut call_args = sanitized_args.clone();
            if !data.assumed_positional.is_empty() || !data.assumed_named.is_empty() {
                let mut positional = Vec::new();
                let mut named = data.assumed_named.clone();
                let mut incoming_positional = Vec::new();
                for arg in &sanitized_args {
                    if let ValueView::Pair(key, boxed) = arg.view() {
                        named.insert(key.clone(), boxed.clone());
                    } else {
                        incoming_positional.push(arg.clone());
                    }
                }
                // Fill bare `*` primers from incoming positional args in order.
                // Remaining positional args are appended after all fixed primers.
                let mut incoming_idx = 0usize;
                for assumed in &data.assumed_positional {
                    let is_placeholder = matches!(assumed.view(), ValueView::Whatever)
                        || matches!(assumed.view(), ValueView::Num(f) if f.is_infinite())
                        || matches!(assumed.view(), ValueView::Rat(_, 0));
                    if is_placeholder {
                        if incoming_idx < incoming_positional.len() {
                            positional.push(incoming_positional[incoming_idx].clone());
                            incoming_idx += 1;
                        }
                    } else {
                        positional.push(assumed.clone());
                    }
                }
                positional.extend(incoming_positional.into_iter().skip(incoming_idx));
                call_args = positional;
                for (key, value) in named {
                    call_args.push(Value::pair(key, value));
                }
            }
            // A `.assuming`-primed `Method` object: the merged arguments
            // (invocant first) go to the object's own `CALL-ME`.
            if let Some(callee) = data
                .env
                .get_sym(crate::symbol::wk::assuming_callee_marker())
                .cloned()
            {
                return self.try_compiled_method_or_interpret(callee, "CALL-ME", call_args);
            }
            // A CORE routine value (`&CORE::name`) runs the built-in, never a
            // same-named declaration of the program.
            if let Some(ValueView::Str(core_name)) = data
                .env
                .get(crate::meta_ns::CORE_ROUTINE_KEY)
                .map(Value::view)
            {
                return self.call_core_routine(&core_name, call_args);
            }
            // Routine wrapper from .assuming() on a multi-dispatch sub
            if let Some(ValueView::Str(routine_name)) =
                data.env.get("__mutsu_routine_name").map(Value::view)
            {
                return self.call_function(&routine_name, call_args);
            }
            // Multi-dispatch dispatcher: captured multi candidates from resolve_code_var
            if let Some((candidates_arc, _)) = data
                .env
                .get("__mutsu_multi_dispatch_candidates")
                .cloned()
                .and_then(Value::into_array)
            {
                let candidates = (*candidates_arc).clone();
                let func_for_family = func.clone();
                let family_name = match data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
                {
                    Some(ValueView::Str(n)) => Some(n.to_string()),
                    _ => None,
                };
                // First try to dispatch via the function table (if still in scope).
                // A proto or multi-candidate name alone is not enough: resolving
                // either here can find the caller's declaration and discard the
                // concrete candidates captured by this code value. A live multi
                // is safe when its candidate identities are exactly the captured
                // set; this keeps nextsame/callsame working for a same-scope code
                // value while avoiding a same-named imported dispatcher.
                if let Some(ValueView::Str(name)) =
                    data.env.get("__mutsu_multi_dispatch_name").map(Value::view)
                {
                    let captured_match = self.captured_candidates_are_live(&name, &candidates)
                        || self.captured_candidates_are_live_subset(&name, &candidates);
                    // A live single routine of that name is the same family
                    // only when it is one of the captured candidates: a
                    // same-named routine imported later into the caller
                    // (`my $ok = do { use Test; &ok }; use Monad::Result
                    // :subs`) must not take the call from them.
                    let live_single_is_captured =
                        self.resolve_function(&name).is_some_and(|live| {
                            candidates.iter().any(|captured| {
                                matches!(
                                    captured.view(),
                                    ValueView::Sub(data)
                                        if data.package == live.package
                                            && data.name == live.name
                                            // The same routine, not another
                                            // family's candidate spelled the
                                            // same (`multi f` of the importing
                                            // unit vs. an imported `multi f`).
                                            && match (&data.compiled_routine, live.compiled_fn()) {
                                                (Some(c), Some(l)) => c.fingerprint == l.fingerprint,
                                                _ => true,
                                            }
                                )
                            })
                        });
                    // A same-named `&name` the caller's scope holds that is no
                    // candidate of this family (a later import of a single
                    // routine of the name) takes the by-name call from it.
                    let amp_shadows = self
                        .env
                        .get(&format!("&{}", name.as_str()))
                        .is_some_and(|bound| {
                            matches!(bound.view(), ValueView::Sub(d)
                                if !d.env.contains_key("__mutsu_multi_dispatch_candidates")
                                    && !candidates.iter().any(|c| matches!(c.view(),
                                        ValueView::Sub(cd) if cd.id == d.id)))
                        });
                    if (live_single_is_captured || captured_match) && !amp_shadows {
                        return self.call_user_family_by_name(&name, call_args);
                    }
                    // The family is not reachable by its bare name from here
                    // (a module's `our &op = &concat` called from the
                    // importer), but its declaring package still names it:
                    // dispatch through the qualified family so the ordinary
                    // narrowness ranking applies, not declaration order.
                    if let Some(pkg) = candidates.first().and_then(|c| match c.view() {
                        ValueView::Sub(d) => Some(d.package),
                        _ => None,
                    }) && !crate::qualified::is_global_package(pkg)
                    {
                        let qname = crate::qualified::qualified(pkg, Symbol::intern(&name));
                        let all_declared_there = candidates.iter().all(|c| {
                            matches!(c.view(), ValueView::Sub(d) if d.package == pkg)
                        });
                        if all_declared_there && self.has_proto(qname.as_str()) {
                            return self.call_user_family_by_name(qname.as_str(), call_args);
                        }
                    }
                    // A compunit-scoped family (#11004) that the calling unit
                    // cannot see by name, such as a module invoking a `&sha256`
                    // its importer passed in. Dispatch it from the unit that
                    // declared the captured candidates. There the name is that
                    // family, with its proto, ranking and defaults intact.
                    if self.operator_has_import_scope(&name)
                        && let Some(unit) = candidates.first().and_then(|c| match c.view() {
                            ValueView::Sub(d) => {
                                Some(self.unit_of_source(d.source_file.as_deref()))
                            }
                            _ => None,
                        })
                    {
                        let saved_unit = std::mem::replace(&mut self.current_unit, unit);
                        // Only when the name really is that family there; a
                        // dispatcher whose candidates no name reaches keeps
                        // the captured-candidate walk below. Another family
                        // of the same name does not count: an imported
                        // family's dispatcher called after its block exited
                        // must not reach the importing unit's own `multi`.
                        if self.captured_candidates_are_live(&name, &candidates) {
                            // By name, as the dispatch above does: a family
                            // spelled like a core routine (`unlink`) must not
                            // reach the built-in arm of `call_function`.
                            let result = self.call_user_family_by_name(&name, call_args);
                            self.current_unit = saved_unit;
                            return result;
                        }
                        self.current_unit = saved_unit;
                    }
                }
                // A proto that does more than dispatch runs first; its `{*}`
                // comes back to the captured candidates (ADR-8751 stage 2).
                if let Some(proto) = data.env.get_sym(crate::symbol::wk::multi_dispatch_proto()).cloned()
                {
                    self.push_proto_dispatch_frame_over(
                        family_name.clone().unwrap_or_default(),
                        call_args.clone(),
                        func_for_family,
                    );
                    let result = self.call_sub_value(proto, call_args);
                    self.pop_proto_dispatch_frame();
                    return result;
                }
                if !candidates.is_empty() {
                    return self.call_captured_candidates(
                        &func_for_family,
                        &candidates,
                        family_name.as_deref(),
                        call_args,
                    );
                }
            }
            // The identity function `[∘]` yields for an empty operand list.
            // Rakudo's is a one-positional block, so it rejects a wrong arity
            // exactly as `-> $x { $x }` would.
            if data
                .env
                .contains_key(crate::runtime::utils::IDENTITY_CALLABLE_MARKER)
            {
                let mut positionals = call_args.into_iter().filter(|a| {
                    !matches!(a.view(), ValueView::Pair(..) | ValueView::ValuePair(..))
                });
                let first = positionals.next();
                let extra = positionals.count();
                return match (first, extra) {
                    (Some(v), 0) => Ok(v),
                    (None, _) => Err(RuntimeError::new(
                        "Too few positionals passed; expected 1 argument but got 0",
                    )),
                    (Some(_), n) => Err(RuntimeError::new(format!(
                        "Too many positionals passed; expected 1 argument but got {}",
                        n + 1
                    ))),
                };
            }
            if let (Some(left), Some(right)) = (
                data.env.get("__mutsu_compose_left").cloned(),
                data.env.get("__mutsu_compose_right").cloned(),
            ) {
                let right_result = self.call_sub_value(right, call_args)?;
                // ADR-0058: `composed_result_to_args` spreads a `Seq` result
                // into the left callable's arguments by reading its elements
                // through pure code, so a still-deferred `.map`/`.grep` result
                // (`{ [+] @_ } o *.map(* * 2)`) has to run its callback first.
                self.reify_map_grep_seq(&right_result)?;
                let (left_params, left_param_defs) = self.callable_signature(&left);
                let left_variadic = left_param_defs.iter().any(|pd| pd.slurpy && !pd.named);
                let left_expects_single = match left.view() {
                    ValueView::Sub(left_data) if left_params.is_empty() => {
                        let (uses_positional, _) =
                            crate::method_signature_shared::auto_signature_uses(&left_data.body);
                        !uses_positional
                    }
                    _ => !left_variadic && left_params.len() == 1,
                };
                let left_args = Self::composed_result_to_args(right_result, left_expects_single);
                return self.call_sub_value(left, left_args);
            }
            // Reject arguments for blocks with explicitly empty signatures
            // (e.g. `-> { ... }` which takes 0 parameters).
            if data.empty_sig && !call_args.is_empty() {
                return Err(Self::reject_args_for_empty_sig(&call_args));
            }
            // A code object built from a registry routine carries that routine's
            // compiled body (ADR-0019 C6c); run it as bytecode instead of
            // executing the AST copy the declaration left in `data.body`
            // (ADR-0019 C6d-4). This is the same fork `vm_call_on_value` takes
            // for the non-wrap dispatch of these code objects — the leg that
            // lands here is the wrap chain's direct-run of the original sub
            // (and the other carriers that call a routine value through the
            // interpreter entry). Blocks and closures keep the carrier below:
            // they carry `compiled_code`, never `compiled_routine`.
            //
            // Scalar `is rw`/`is raw` routines run compiled too: the binder
            // aliases such a param to a shared `ContainerRef` cell chained to
            // the caller's variable, so the wrap-chain relay no longer depends
            // on the carrier's same-name env merge (the gate that used to sit
            // here; see news/2026-08/rw-params-bind-shared-cells.md).
            if data.compiled_routine.is_some()
                && let Some(cf) = data.compiled_routine.clone()
            {
                // Prefer the routine's own nested-sub table over an empty one
                // (ADR-0019 C6e-3c): e.g. `_is-eqv`'s multi dispatch calling
                // `test-eqv`, a `sub` nested in its own body, needs this to
                // resolve `test-eqv`'s compiled routine key.
                let empty_fns = CompiledFns::default();
                let fns = cf.compiled_fns.as_deref().unwrap_or(&empty_fns);
                // Carrier parity: a binding failure here must surface raw
                // (X::TypeCheck::Binding), not reclassified as a compile-time
                // X::TypeCheck::Argument. One-shot; consumed at the callee's
                // entry — see suppress_binding_error_enhance.
                self.dispatch.suppress_binding_error_enhance = true;
                return self.call_compiled_closure(&data, &cf.code, call_args, fns);
            }
            // A `supply` block body keeps the carrier below, which hands it its
            // emitter and the panic->quit boundary (see `call_supply_callback`).
            if let Some(cc) = data.compiled_code.clone() {
                let empty_fns = CompiledFns::default();
                let fns = data.compiled_fns.as_deref().unwrap_or(&empty_fns);
                return self.call_compiled_closure(&data, &cc, call_args, fns);
            }
            // Every code object carries compiled bytecode: a routine its
            // `compiled_routine`, a block or closure its `compiled_code`
            // (ADR-0055 §8.3, #14572). One without either has no body to run.
            return Err(RuntimeError::new(
                "Code object has no compiled body",
            ));
        }
        Err(RuntimeError::new("Callable expected"))
    }
}
