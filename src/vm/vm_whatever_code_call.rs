//! Calling a `WhateverCode` (or any `Callable`) that stands in a subscript or a
//! `Buf` offset — `@a[*-1]`, `@a[*-3..*-1]`, `$b.subbuf(*-2)`.
//!
//! Such a closure is called with the container's element count bound to every
//! one of its parameters (`*-4 .. *-2` curries two). It is a closure like any
//! other, so it runs its own pre-compiled bytecode (`SubData::compiled_code`);
//! issue #10118 is the history of the sites that instead re-compiled its AST
//! `body` on every access (one `Compiler::compile` per subscript).

use super::*;

impl Interpreter {
    /// Call the subscript closure `data` with the element count `len` bound to
    /// each of its parameters, and return what it yields (`Nil` if it dies,
    /// which is what every subscript site did with a failed evaluation).
    ///
    /// Every subscript / delete / assign / `Buf` offset site that resolves a
    /// `Callable` index goes through here.
    // Cost: O(p + b), p = the closure's parameter count, b = the cost of one
    // run of its compiled body. No compilation on the path that has bytecode.
    pub(crate) fn call_subscript_code(
        &mut self,
        data: &crate::gc::Gc<crate::value::SubData>,
        len: i64,
    ) -> Value {
        let args = vec![Value::int(len); data.params.len()];
        let empty_fns = CompiledFns::default();
        if let Some(cc) = data.compiled_code.clone() {
            let fns = data.compiled_fns.as_deref().unwrap_or(&empty_fns);
            return self
                .call_compiled_closure(data, &cc, args, fns)
                .unwrap_or(Value::NIL);
        }
        // A code object built from a registry routine carries that routine's
        // own bytecode (ADR-0019 C6c), the same way `vm_call_map_block` runs it.
        if let Some(cf) = data.compiled_routine.clone() {
            let fns = cf.compiled_fns.as_deref().unwrap_or(&empty_fns);
            return self
                .call_compiled_closure(data, &cf.code, args, fns)
                .unwrap_or(Value::NIL);
        }
        // TODO: compile to bytecode. A `Sub` with neither bytecode form has no
        // compiled body to run, so its AST is evaluated in its captured
        // environment with the parameters bound to `len`. The compiler attaches
        // `compiled_code` to every closure it emits (including WhateverCode), so
        // this is reached only by a Sub value constructed without one.
        let mut sub_env = data.env.clone();
        for p in data.params.iter() {
            sub_env.insert(p.to_string(), Value::int(len));
        }
        let saved_env = std::mem::replace(self.env_mut(), sub_env);
        let result = self.eval_block_value(&data.body).unwrap_or(Value::NIL);
        *self.env_mut() = saved_env;
        result
    }

    /// `$buf.subbuf($from, $len)` / `.subbuf-rw(...)` with a `Callable` offset
    /// (`*-2`) or end (`*-1`): the replacement argument list with each Callable
    /// resolved to the `Int` the pure cascade (`builtins::methods_narg`) reads,
    /// or `None` when there is nothing to resolve.
    ///
    /// A Callable `$from` is called with the element count and yields the start
    /// offset; a Callable second argument yields an inclusive END index, so it
    /// becomes the length `end - from + 1` (clamped at 0).
    // Cost: O(n + b), n = the argument count, b = one run of each Callable.
    pub(crate) fn resolve_subbuf_callable_args(
        &mut self,
        target: &Value,
        method_name: &str,
        args: &[Value],
    ) -> Option<Vec<Value>> {
        if !matches!(method_name, "subbuf" | "subbuf-rw")
            || !args.iter().any(|a| matches!(a.view(), ValueView::Sub(_)))
            || !crate::builtins::is_buf_like(target)
        {
            return None;
        }
        let len = crate::builtins::buf_get_int_items(target)?.len();
        let mut resolved = args.to_vec();
        if let Some(first) = resolved.first_mut()
            && let ValueView::Sub(data) = first.view()
        {
            let start = self.call_subscript_code(&data, len as i64);
            *first = Value::int(crate::runtime::utils::to_int(&start));
        }
        if let Some(second) = resolved.get(1)
            && let ValueView::Sub(data) = second.view()
        {
            let start = crate::builtins::resolve_buf_index(&resolved[0], len);
            let end_idx = self.call_subscript_code(&data, len as i64);
            let sub_len = crate::runtime::utils::to_int(&end_idx) - start + 1;
            resolved[1] = Value::int(sub_len.max(0));
        }
        Some(resolved)
    }
}
