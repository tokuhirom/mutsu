use super::*;

impl Interpreter {
    pub(super) fn force_lazy_list(&mut self, list: &LazyList) -> Result<Vec<Value>, RuntimeError> {
        // A lazy map/grep pipeline is rooted at an infinite source. It can still
        // terminate when the callback runs `last`; attempt a bounded force and
        // return the result if the pipe became `done`, otherwise throw
        // X::Cannot::Lazy (genuinely infinite). Mirrors `force_lazy_list_vm`.
        if list.lazy_pipe.is_some() {
            const EAGER_FORCE_CAP: usize = 1_000_000;
            let forced = self.force_lazy_pipe(list, EAGER_FORCE_CAP)?;
            let done = list
                .lazy_pipe
                .as_ref()
                .map(|p| p.lock().unwrap().done)
                .unwrap_or(true);
            if done {
                return Ok(forced);
            }
            return Err(RuntimeError::typed_msg(
                "X::Cannot::Lazy",
                "Cannot coerce an infinite lazy list to a strict list",
            ));
        }
        // A lazy `IO::CatHandle.lines`/`.handles` list is finite: pull from the
        // live cat until exhausted (cache always starts non-empty, so this must
        // run before the cache short-circuit below).
        if list.cat_pull.is_some() {
            return self.force_cat_pull(list, usize::MAX);
        }
        // A lazy `WALK(method)()` candidate-invocation list (ADR-0019 E7 step
        // 7, `todo/deep/adr0019-e5-e7-entry-routing.md`): pull every remaining
        // MRO-level candidate. Its cache starts non-empty too
        // (`LazyList::new_cached(Vec::new())`), so this must run before the
        // cache short-circuit below for the same reason as `cat_pull` above --
        // without it, `.gist`/`.Str`/`say`/list-coercion on a fresh
        // `$x.WALK(...)()` (before anything else has pulled it) read the
        // still-empty initial cache and silently printed/produced `()`.
        if list.walk_pending.is_some() {
            return self.force_walk_pending(list, usize::MAX);
        }
        // A closure sequence with a concrete endpoint is externally finite,
        // but its generator is retained so construction does not eagerly walk
        // to a distant endpoint.  A strict consumer must drive that generator
        // before consulting the seed cache.
        if list.has_finite_closure_endpoint() {
            return self.extend_closure_sequence(list, usize::MAX);
        }
        if let Some(cached) = list.cache.lock().unwrap().clone() {
            return Ok(cached);
        }
        // Lazy map: evaluate the stored callback over the stored items now.
        //
        // `.map` returns a lazy Seq in Raku, so the callback runs only when the
        // Seq is forced. By deferring evaluation to this point we get the
        // correct out-of-dynamic-scope behaviour for `return` inside the
        // callback for free: if the lexically enclosing routine has already
        // exited (it is no longer on the dynamic call stack), the existing
        // CX::Return propagation machinery (calls.rs / the top-level VM loop)
        // converts the signal into `X::ControlFlow::Return` with
        // out-of-dynamic-scope set. Delegating to `eval_map_over_items` keeps
        // full fidelity with eager map: block arity > 1, Slip flattening,
        // LAST/NEXT phasers, and composed callbacks all behave identically.
        if let (Some(map_items), Some(map_func)) = (
            list.env.get("__mutsu_lazy_map_items"),
            list.env.get("__mutsu_lazy_map_func"),
        ) {
            let items_list = crate::runtime::value_to_list(map_items);
            let func = map_func.clone();
            let result = self.eval_map_over_items(Some(func), items_list)?;
            let result_items = crate::runtime::value_to_list(&result);
            *list.cache.lock().unwrap() = Some(result_items.clone());
            return Ok(result_items);
        }
        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        self.gather_take_limits.push(None);
        // The gather body is a lexical block: a `sub` declared inside it shadows
        // any same-named routine from an enclosing (or sibling) scope rather than
        // redeclaring it. Without this depth bump the routine registration would
        // see `&name` left in env by an earlier sibling block and raise a bogus
        // X::Redeclaration. The VM force path never hits this because it dispatches
        // the body's subs out of the body's own `compiled_fns`.
        self.push_block_scope_depth();
        // The body's `samewith` names the routine the gather was WRITTEN in —
        // see `push_captured_samewith_context`.
        let pushed_samewith = self.push_captured_samewith_context(&list.env);
        let run_res = self.run_block(&list.body);
        self.pop_captured_samewith_context(pushed_samewith);
        self.pop_block_scope_depth();
        let items = self.gather_items.pop().unwrap_or_default();
        self.gather_take_limits.pop();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
            self.gather_take_limits.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert_sym(*k, v.clone());
        }
        self.env = merged_env;
        run_res?;
        *list.cache.lock().unwrap() = Some(items.clone());
        Ok(items)
    }

    pub(crate) fn force_lazy_list_prefix_bridge(
        &mut self,
        list: &crate::value::LazyList,
        needed_len: usize,
    ) -> Result<Vec<Value>, RuntimeError> {
        if needed_len == 0 {
            return Ok(Vec::new());
        }
        if let Some(cached) = list.cache.lock().unwrap().clone()
            && cached.len() >= needed_len
        {
            return Ok(cached[..needed_len].to_vec());
        }

        let saved_env = self.env.clone();
        let saved_len = self.gather_items.len();
        self.env = list.env.clone();
        self.gather_items.push(Vec::new());
        self.gather_take_limits.push(Some(needed_len));
        self.gather_suspend_pending = false;
        // See `push_captured_samewith_context`.
        let pushed_samewith = self.push_captured_samewith_context(&list.env);
        let run_res = self.run_block(&list.body);
        self.pop_captured_samewith_context(pushed_samewith);
        // Clear the deferred-suspension flag on exit — see the matching clear
        // in force_lazy_list_vm_n_inner.
        self.gather_suspend_pending = false;
        let mut items = self.gather_items.pop().unwrap_or_default();
        self.gather_take_limits.pop();
        while self.gather_items.len() > saved_len {
            self.gather_items.pop();
            self.gather_take_limits.pop();
        }
        let mut merged_env = saved_env;
        for (k, v) in self.env.iter() {
            merged_env.insert_sym(*k, v.clone());
        }
        self.env = merged_env;

        match run_res {
            Ok(()) => {
                *list.cache.lock().unwrap() = Some(items.clone());
            }
            Err(err) if err.message == Self::LAZY_GATHER_TAKE_LIMIT_SIGNAL => {}
            Err(err) => return Err(err),
        }
        if items.len() > needed_len {
            items.truncate(needed_len);
        }
        Ok(items)
    }

    pub(crate) fn force_lazy_list_bridge(
        &mut self,
        list: &crate::value::LazyList,
    ) -> Result<Vec<Value>, RuntimeError> {
        self.force_lazy_list(list)
    }

    /// Force a `LazyIoLines` value into an eager array by reading all remaining
    /// records from the file handle. When `words` is true the records are
    /// whitespace-delimited words; otherwise they are lines.
    pub(crate) fn force_lazy_io_lines(
        &mut self,
        handle: &Value,
        words: bool,
    ) -> Result<Value, RuntimeError> {
        let mut items = Vec::new();
        if words {
            while let Some(word) = self.read_word_from_handle_value(handle)? {
                items.push(Value::str(word));
            }
        } else {
            while let Some(line) = self.read_line_from_handle_value(handle)? {
                items.push(Value::str(line));
            }
        }
        Ok(Value::array(items))
    }
}
