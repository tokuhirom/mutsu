use super::*;
use crate::value::RuntimeError;

/// Returns true if the value may have a custom `.gist`/`.Str` method that
/// requires interpreter method dispatch.  For all other (primitive) types
/// we can use the fast `gist_value()` / `to_string_value()` paths directly.
fn needs_method_dispatch(v: &Value) -> bool {
    match v {
        Value::Instance { .. }
        | Value::CustomType { .. }
        | Value::CustomTypeInstance(_)
        | Value::Mixin(..)
        | Value::Proxy { .. }
        | Value::Junction { .. } => true,
        // Type objects may carry a user-defined `method gist`/`method Str`
        // (callable on the type object itself), so route them through method
        // dispatch; `render_gist_value`/`render_str_value` fall back to the
        // default `(TypeName)` rendering when no such method exists.
        Value::Package(..) => true,
        // A LazyList (gather/take, infinite sequence, lazy map/grep pipeline)
        // must be rendered via `.gist`/`.Str` method dispatch: an eager gather
        // is forced to its elements, while a genuinely lazy/infinite one
        // renders as raku's placeholder (`(...)` / `...`). The pure
        // `gist_value`/`to_str_context` fast paths would print the bare type
        // name "LazyList" instead.
        Value::LazyList(..) => true,
        // A collection whose gist embeds an element's gist must be rendered via
        // method dispatch when any element needs it (e.g. an instance/type-object
        // with a custom `method gist`), so the per-element gist is honored.
        Value::Array(items, _) => items.iter().any(element_needs_method_dispatch),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            items.iter().any(element_needs_method_dispatch)
        }
        Value::Hash(map) => map.values().any(element_needs_method_dispatch),
        Value::Pair(_, val) => element_needs_method_dispatch(val),
        Value::ValuePair(k, val) => {
            element_needs_method_dispatch(k) || element_needs_method_dispatch(val)
        }
        _ => false,
    }
}

/// Whether a *collection element* must be rendered via method dispatch. Unlike
/// the top-level check, a Mixin element is excluded: a Mixin wrapping a
/// List/Array renders via its inner value, and dispatching `.gist` on it would
/// add a spurious paren layer (regressing e.g. `(@list but Role).gist`).
fn element_needs_method_dispatch(v: &Value) -> bool {
    match v {
        Value::Instance { .. }
        | Value::CustomType { .. }
        | Value::CustomTypeInstance(_)
        | Value::Package(..) => true,
        Value::Array(items, _) => items.iter().any(element_needs_method_dispatch),
        Value::Seq(items) | Value::HyperSeq(items) | Value::RaceSeq(items) | Value::Slip(items) => {
            items.iter().any(element_needs_method_dispatch)
        }
        Value::Hash(map) => map.values().any(element_needs_method_dispatch),
        Value::Pair(_, val) => element_needs_method_dispatch(val),
        Value::ValuePair(k, val) => {
            element_needs_method_dispatch(k) || element_needs_method_dispatch(val)
        }
        _ => false,
    }
}

/// Check if a value is a Rat/FatRat/BigRat with zero denominator and throw
/// X::Numeric::DivideByZero if so (Raku defers the error until the value is used).
fn check_rat_divide_by_zero(v: &Value) -> Result<(), RuntimeError> {
    match v {
        Value::Rat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::FatRat(n, d) if *d == 0 => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::Int(*n),
        ))),
        Value::BigRat(n, d) if d.is_zero() => Err(RuntimeError::numeric_divide_by_zero_with(Some(
            Value::from_bigint((**n).clone()),
        ))),
        _ => Ok(()),
    }
}

impl Interpreter {
    pub(super) fn exec_make_array_op(&mut self, n: u32, is_real_array: bool) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            // Force lazy IO lines into eager arrays
            let val = if matches!(&val, Value::LazyIoLines { .. }) {
                match self.force_if_lazy_io_lines(val) {
                    Ok(v) => v,
                    Err(_) => continue,
                }
            } else if let Value::LazyList(ll) = &val
                && ll.coroutine.is_some()
                && ll.sequence_spec.is_none()
                && ll.scan_spec.is_none()
                && ll.lazy_pipe.is_none()
                && !matches!(
                    ll.env.get("__mutsu_preserve_lazy_on_array_assign"),
                    Some(Value::Bool(true))
                )
            {
                // Array literals (`[...]`) are eager: a gather/take LazyList must
                // run now so its side effects happen (e.g. `take shift @array`
                // mutating an outer array a surrounding `while` loops on) and its
                // elements materialize. Only a plain gather (coroutine, no
                // sequence/scan/pipe spec) is forced — an infinite `...` sequence
                // or a lazy map/grep pipe must stay lazy (forcing a sequence_spec
                // would truncate it to its finite cache). A `lazy`-marked list is
                // likewise left lazy; so is one that fails a strict force.
                match self.force_lazy_list_vm(ll) {
                    Ok(items) => Value::Seq(std::sync::Arc::new(items)),
                    Err(_) => val,
                }
            } else {
                val
            };
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                Value::Array(items, kind) if kind.is_itemized() => {
                    elems.push(Value::Array(items, kind))
                }
                // Scalar-wrapped values (.item / $()) are never flattened.
                Value::Scalar(inner) => elems.push(Value::Scalar(inner)),
                // Set/Bag/Mix are Iterable but NOT Positional, so they do not
                // flatten in list context (unlike a List/Array/Seq/Range, and
                // unlike a Hash, which does flatten to its pairs). A single
                // `[set(1,2)]` therefore keeps the Set whole as one element.
                // (Matches raku and `flat(set(...))`; `value_to_list` would
                // otherwise decompose it into its key/True pairs.)
                v @ (Value::Set(..) | Value::Bag(..) | Value::Mix(..)) => elems.push(v),
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                other if is_real_array && n == 1 => elems.extend(runtime::value_to_list(&other)),
                other => elems.push(other),
            }
        }
        if is_real_array {
            self.stack.push(Value::real_array(elems));
        } else {
            self.stack.push(Value::array(elems));
        }
    }

    /// Like `exec_make_array_op` with `is_real_array=true` but never flattens
    /// single elements. Used for bracket arrays with trailing comma (`[x,]`)
    /// and for `[$scalar]` / `[$%h]` to prevent hash/array flattening.
    pub(super) fn exec_make_array_no_flatten_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            match val {
                Value::Slip(items) => elems.extend(items.iter().cloned()),
                // Nil in list context contributes nothing (same as value_to_list).
                Value::Nil => {}
                other => elems.push(other),
            }
        }
        self.stack.push(Value::real_array(elems));
    }

    pub(super) fn exec_make_hash_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n * 2;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for pair in items.chunks(2) {
            let key = Value::hash_key_encode(&pair[0]);
            let val = pair[1].clone();
            map.insert(key, val);
        }
        self.stack.push(Value::hash(map));
    }

    /// Build a Hash from N Pair values on the stack (from `%(k=>v, ...)` syntax).
    pub(super) fn exec_make_hash_from_pairs_op(&mut self, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        let mut map = HashMap::new();
        for item in items {
            match item {
                Value::Pair(k, v) => {
                    map.insert(k, *v);
                }
                Value::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), *v);
                }
                _ => {
                    // Non-pair values: use stringified value as key mapped to True
                    map.insert(item.to_string_value(), Value::Bool(true));
                }
            }
        }
        self.stack.push(Value::hash(map));
    }

    /// Box the local scalar variable `name` into a shared `ContainerRef` cell and
    /// return that cell, so a Capture positional built from `\($name)` aliases the
    /// variable's container (`$c[0]++` writes through to `$name`). If the variable
    /// is not a local slot in this frame (or is already a cell), fall back to the
    /// captured `inner` value / existing cell. Mirrors `box_captured_lexicals`.
    pub(super) fn capture_var_cell(
        &mut self,
        code: &CompiledCode,
        name: &str,
        inner: Value,
    ) -> Value {
        if inner.is_container_ref() {
            return inner;
        }
        let Some(idx) = code.locals.iter().rposition(|n| n == name) else {
            return inner;
        };
        if self.locals[idx].is_container_ref() {
            return self.locals[idx].clone();
        }
        // Only box a plain scalar container; reference/type values are not
        // re-containerized (mirrors the box-on-capture guard).
        if matches!(
            self.locals[idx],
            Value::Package(_)
                | Value::Array(..)
                | Value::Hash(..)
                | Value::Sub(..)
                | Value::Instance { .. }
                | Value::Proxy { .. }
        ) {
            return self.locals[idx].clone();
        }
        let cell = self.locals[idx].clone().into_container_ref();
        self.locals[idx] = cell.clone();
        // The captured local is now a shared `ContainerRef`. It MUST also reach
        // env unconditionally: a later interpreter-side mutation (e.g. `$pair.value
        // = X` writing through the cell) triggers an env->locals resync that would
        // otherwise overwrite the local with a stale by-value env copy, breaking
        // the alias. `flush_local_to_env` only flushes "simple" locals, so set env
        // directly here.
        let sym = code.locals_sym.get(idx).copied();
        self.set_env_with_main_alias_sym(name, sym, cell.clone());
        cell
    }

    pub(super) fn exec_make_capture_op(&mut self, code: &CompiledCode, n: u32) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut positional = Vec::new();
        let mut named = HashMap::new();
        for val in raw {
            // A `WrapVarRef`-tagged scalar variable positional (`\($a)`): capture
            // the variable's *container* so `$c[0]` aliases `$a` and `$c[0]++`
            // writes through. Box the named local into a shared `ContainerRef`
            // cell (same scope as `$c`, so sharing the slot's cell suffices) and
            // store that cell as the positional element.
            if let Value::Capture {
                positional: p,
                named: nm,
            } = &val
                && p.is_empty()
                && let Some(Value::Str(source_name)) = nm.get("__mutsu_varref_name")
                && let Some(inner) = nm.get("__mutsu_varref_value")
            {
                let source_name = source_name.to_string();
                let inner = inner.clone();
                positional.push(self.capture_var_cell(code, &source_name, inner));
                continue;
            }
            match val {
                Value::Pair(k, v) => {
                    // A named scalar-var element (`\(:$a)`): the value is a
                    // WrapVarRef-tagged capture — box the named local so `$c<a>`
                    // aliases `$a` and `$c<a>++` writes through.
                    if let Value::Capture {
                        positional: p,
                        named: nm,
                    } = v.as_ref()
                        && p.is_empty()
                        && let Some(Value::Str(source_name)) = nm.get("__mutsu_varref_name")
                        && let Some(inner) = nm.get("__mutsu_varref_value")
                    {
                        let source_name = source_name.to_string();
                        let inner = inner.clone();
                        let cell = self.capture_var_cell(code, &source_name, inner);
                        named.insert(k, cell);
                    } else {
                        named.insert(k, *v);
                    }
                }
                Value::Capture {
                    positional: p,
                    named: n,
                } => {
                    // Flatten inner capture (from |capture slip)
                    positional.extend(*p);
                    named.extend(*n);
                }
                Value::Slip(items) => {
                    for item in items.iter() {
                        match item {
                            Value::Pair(k, v) => {
                                named.insert(k.clone(), *v.clone());
                            }
                            other => positional.push(other.clone()),
                        }
                    }
                }
                other => positional.push(other),
            }
        }
        self.stack.push(Value::capture(positional, named));
    }

    /// Flatten top-level `Slip` arguments into the surrounding argument list.
    /// A `|(...)` slip passed to a list operator (say/put/print/note) spreads its
    /// elements as individual arguments, exactly like the parenthesized-call path.
    fn flatten_slip_args(values: Vec<Value>) -> Vec<Value> {
        if !values.iter().any(|v| matches!(v, Value::Slip(_))) {
            return values;
        }
        let mut out = Vec::with_capacity(values.len());
        for v in values {
            match v {
                Value::Slip(items) => out.extend(items.iter().cloned()),
                other => out.push(other),
            }
        }
        out
    }

    pub(super) fn exec_say_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        let mut parts = Vec::new();
        for v in &values {
            let v = loan_env!(self, auto_fetch_proxy(v))?;
            check_rat_divide_by_zero(&v)?;
            // Resolve bound-element sentinels inside arrays before gist
            let v = self.resolve_bound_array_elements(v);
            if needs_method_dispatch(&v) {
                parts.push(loan_env!(self, render_gist_value(&v)));
            } else {
                parts.push(runtime::gist_value(&v));
            }
        }
        let line = parts.join("");
        loan_env!(self, write_to_named_handle("$*OUT", &line, true))?;
        Ok(())
    }

    pub(super) fn exec_note_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let content = if n == 0 {
            "Noted".to_string()
        } else {
            let start = self.stack.len() - n;
            let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
            let mut parts = Vec::new();
            for v in &values {
                if needs_method_dispatch(v) {
                    parts.push(loan_env!(self, render_gist_value(v)));
                } else {
                    parts.push(runtime::gist_value(v));
                }
            }
            parts.join("")
        };
        loan_env!(self, write_to_named_handle("$*ERR", &content, true))?;
        Ok(())
    }

    pub(super) fn exec_put_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        // A lone Junction argument autothreads: each eigenstate is put on its
        // own line (`put 1|2` => "1\n2\n").
        if values.len() == 1 && matches!(&values[0], Value::Junction { .. }) {
            let v = loan_env!(self, auto_fetch_proxy(&values[0]))?;
            check_rat_divide_by_zero(&v)?;
            let mut lines = Vec::new();
            self.collect_put_lines(&v, &mut lines)?;
            for line in &lines {
                loan_env!(self, write_to_named_handle("$*OUT", line, true))?;
            }
            return Ok(());
        }
        // Otherwise concatenate every argument's `.Str` into a single line plus a
        // trailing newline (`put 1, 2, 3` => "123\n"), like `print` with a newline.
        let mut content = String::new();
        for v in &values {
            let v = loan_env!(self, auto_fetch_proxy(v))?;
            check_rat_divide_by_zero(&v)?;
            if needs_method_dispatch(&v) {
                content.push_str(&loan_env!(self, render_str_value(&v)));
            } else {
                content.push_str(&v.to_str_context());
            }
        }
        loan_env!(self, write_to_named_handle("$*OUT", &content, true))?;
        Ok(())
    }

    pub(super) fn exec_print_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let values: Vec<Value> = Self::flatten_slip_args(self.stack.drain(start..).collect());
        let mut content = String::new();
        for v in &values {
            check_rat_divide_by_zero(v)?;
            // For Junctions, thread: call .Str on each element recursively
            self.collect_str_threaded(v, &mut content)?;
        }
        loan_env!(self, write_to_named_handle("$*OUT", &content, false))?;
        Ok(())
    }

    /// Recursively collect put lines from a value, threading through Junctions.
    fn collect_put_lines(
        &mut self,
        v: &Value,
        lines: &mut Vec<String>,
    ) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_put_lines(elem, lines)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                lines.push(loan_env!(self, render_str_value(v)));
            }
            _ => {
                lines.push(v.to_str_context());
            }
        }
        Ok(())
    }

    /// Recursively collect .Str output from a value, threading through Junctions.
    fn collect_str_threaded(&mut self, v: &Value, out: &mut String) -> Result<(), RuntimeError> {
        match v {
            Value::Junction { values, .. } => {
                for elem in values.iter() {
                    self.collect_str_threaded(elem, out)?;
                }
            }
            _ if needs_method_dispatch(v) => {
                out.push_str(&loan_env!(self, render_str_value(v)));
            }
            _ => {
                out.push_str(&v.to_str_context());
            }
        }
        Ok(())
    }
}

impl Interpreter {
    /// Fast path for @arr.push(val) — directly appends to the array Arc.
    pub(super) fn exec_array_push_op(
        &mut self,
        code: &CompiledCode,
        target_name_idx: u32,
        value_source_idx: Option<u32>,
    ) -> Result<(), RuntimeError> {
        let target_name = Self::const_str(code, target_name_idx);
        // A lazy `@`-array (infinite source) cannot be pushed to: there is no
        // end to append after. raku throws `X::Cannot::Lazy`
        // ("Cannot push to a lazy list onto a Array"). (L2)
        if let Some(Value::LazyList(ll)) = self.env().get(target_name)
            && ll.in_array_context()
            && ll.is_genuinely_lazy()
        {
            let _ = self.stack.pop();
            return Err(RuntimeError::cannot_lazy_with_action("push to", "Array"));
        }
        // Shared (threaded) context: route an Array push through the atomic
        // shared store so concurrent `@a.push` from multiple threads serialize
        // under the shared_vars write lock instead of clobbering each other's
        // stale local snapshots (lost update). Non-Array targets keep the
        // interpreter fallback.
        if self.shared_vars_active {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            // Only a plain lexical `@name` (not an attribute `@!x`/`@.x` or other
            // twigil'd form) has a single shared identity across threads, so only
            // it may funnel into the name-keyed atomic shared store.
            if matches!(target, Value::Array(..) | Value::Nil)
                && Self::is_plain_lexical_array_name(target_name)
            {
                let items = match val {
                    Value::Slip(items) => items.to_vec(),
                    other => vec![other],
                };
                let result = self.shared_array_extend(target_name, items, false);
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        // TODO: compile to bytecode — shaped-array push, blocked-by: shaped
        // dimension metadata check in Interpreter. See ledger §1.
        // Check for shaped arrays — must fall back to interpreter
        // (push is illegal on fixed-dimension arrays)
        if let Some(Value::Array(_, kind)) = self.env().get(target_name)
            && *kind == crate::value::ArrayKind::Shaped
        {
            let val = self.stack.pop().unwrap_or(Value::Nil);
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }
        let mut val = self.stack.pop().unwrap_or(Value::Nil);

        // Reference push (`@a.push(@b)` / `@a.push(%h)`): Raku's non-flattening
        // `**@` slurpy stores the container itself, so later mutations of the
        // source (`@b.push(4)`, `@b[0]=v`, `@b=(...)`) must propagate to the
        // stored element. Share a `ContainerRef` cell between the source variable
        // and the pushed element (the same mechanism as a whole-container `:=`
        // bind). Reuse the source's existing cell if it already has one.
        if let Some(src_idx) = value_source_idx
            && matches!(val, Value::Array(..) | Value::Hash(..))
        {
            let src_name = Self::const_str(code, src_idx).to_string();
            let existing_cell = self
                .get_env_with_main_alias(&src_name)
                .or_else(|| {
                    self.find_local_slot(code, &src_name)
                        .map(|s| self.locals[s].clone())
                })
                .and_then(|v| match v {
                    Value::ContainerRef(cell) => Some(cell),
                    _ => None,
                });
            let cell = existing_cell.unwrap_or_else(|| {
                let cell = std::sync::Arc::new(std::sync::Mutex::new(val.clone()));
                let cell_val = Value::ContainerRef(cell.clone());
                self.set_env_with_main_alias(&src_name, cell_val.clone());
                self.update_local_if_exists(code, &src_name, &cell_val);
                cell
            });
            val = Value::ContainerRef(cell);
        }

        // Empty (empty Slip) means nothing to push -- return the array as-is.
        if let Value::Slip(ref items) = val
            && items.is_empty()
        {
            let result = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            self.stack.push(result);
            return Ok(());
        }

        // TODO: compile to bytecode — non-simple-target push, blocked-by:
        // first-class container identity Phase 2 (closure-captured ContainerRef
        // arrays). See ledger §1.
        // Check the target exists as a simple Array in env.
        // If not (e.g., captured closure var, or non-Array), fall back to interpreter.
        let is_simple_array = self
            .env()
            .get(target_name)
            .is_some_and(|v| matches!(v, Value::Array(..)));
        if !is_simple_array {
            let target = self.env().get(target_name).cloned().unwrap_or(Value::Nil);
            // Phase 2 Stage 2: a `:=`-cell-bound variable (`@x[0] := @b` /
            // `%h<k> := @b`) or a Slice 2a `=`-array-shared scalar (`$n = @z`)
            // holds a shared `ContainerRef` cell. Mutate the array INSIDE the
            // cell with COW (`Arc::make_mut`) — mirroring the simple-array path
            // below — so a copy made out of this cell (`my @copy = @z`), which
            // shares the inner Arc, is detached rather than mutated in place.
            // The shared cell itself keeps every alias coherent.
            if let Value::ContainerRef(cell) = target {
                let mut guard = cell.lock().unwrap();
                if let Value::Array(arr, kind) = &mut *guard {
                    let kind = *kind;
                    let items = Arc::make_mut(arr);
                    match &val {
                        Value::Slip(slip_items) => items.extend(slip_items.iter().cloned()),
                        _ => items.push(val),
                    }
                    let result = Value::Array(arr.clone(), kind);
                    drop(guard);
                    self.stack.push(result);
                    self.env_dirty = true;
                    return Ok(());
                }
                // Non-array inner (e.g. Hash): generic clone-and-write-back.
                let inner = guard.clone();
                drop(guard);
                let result = loan_env!(self, call_method_with_values(inner, "push", vec![val]))?;
                *cell.lock().unwrap() = result.clone();
                self.stack.push(result);
                self.env_dirty = true;
                return Ok(());
            }
            let result = loan_env!(self, call_method_with_values(target, "push", vec![val]))?;
            self.stack.push(result);
            self.env_dirty = true;
            return Ok(());
        }

        // Check type constraint on the array variable.
        // For Slip values, check each element individually.
        if let Some(type_name) = self
            .var_type_constraint_fast(target_name)
            .map(|s| s.to_string())
        {
            let items_to_check: Vec<&Value> = match &val {
                Value::Slip(items) => items.iter().collect(),
                other => vec![other],
            };
            for item in items_to_check {
                if !self.type_matches_value(&type_name, item) {
                    return Err(RuntimeError::typecheck_assignment(
                        &type_name,
                        item,
                        Some(target_name),
                    ));
                }
            }
        }

        // Find the local slot and drop it to allow in-place mutation
        let local_slot = self.find_local_slot(code, target_name);
        if let Some(slot) = local_slot {
            self.locals[slot] = Value::Nil;
        }

        let result = if let Some(Value::Array(arr, kind)) = self.env_mut().get_mut(target_name) {
            let items = Arc::make_mut(arr);
            match &val {
                Value::Slip(slip_items) => items.extend(slip_items.iter().cloned()),
                _ => items.push(val),
            }
            Value::Array(arr.clone(), *kind)
        } else {
            // Auto-vivify: create new array
            let arr = match &val {
                Value::Slip(slip_items) => Value::real_array(slip_items.to_vec()),
                _ => Value::real_array(vec![val]),
            };
            self.env_mut().insert(target_name.to_string(), arr.clone());
            arr
        };

        // Restore local slot
        if let Some(slot) = local_slot {
            self.locals[slot] = result.clone();
        }

        self.stack.push(result);
        // Slice 6.3 step 2: no env_dirty mark. This native push path mutates only
        // `target_name` in env and has already reverse-write-through'd the result
        // into its local slot just above, so the caller's slot is coherent — a
        // pull would be redundant. (The interpreter-fallback push branches above,
        // for shared/shaped/non-simple-array targets, keep their conservative mark.)
        Ok(())
    }
}
