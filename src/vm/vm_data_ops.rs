use super::*;

impl Interpreter {
    pub(super) fn exec_make_array_op(&mut self, n: u32, is_real_array: bool) {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            // Force lazy IO lines into eager arrays
            let val = if matches!(val.view(), ValueView::LazyIoLines { .. }) {
                match self.force_if_lazy_io_lines(val) {
                    Ok(v) => v,
                    Err(_) => continue,
                }
            } else if let ValueView::LazyList(ll) = val.view()
                && ll.coroutine.is_some()
                && ll.sequence_spec.is_none()
                && ll.scan_spec.is_none()
                && ll.lazy_pipe.is_none()
                && !matches!(
                    ll.env
                        .get("__mutsu_preserve_lazy_on_array_assign")
                        .map(Value::view),
                    Some(ValueView::Bool(true))
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
                match self.force_lazy_list_vm(&ll) {
                    Ok(items) => Value::seq(items),
                    Err(_) => val,
                }
            } else {
                val
            };
            match val.view() {
                ValueView::Slip(items) => elems.extend(items.iter().cloned()),
                ValueView::Array(_, kind) if kind.is_itemized() => elems.push(val),
                // Scalar-wrapped values (.item / $()) are never flattened.
                ValueView::Scalar(_) => elems.push(val),
                // Set/Bag/Mix are Iterable but NOT Positional, so they do not
                // flatten in list context (unlike a List/Array/Seq/Range, and
                // unlike a Hash, which does flatten to its pairs). A single
                // `[set(1,2)]` therefore keeps the Set whole as one element.
                // (Matches raku and `flat(set(...))`; `value_to_list` would
                // otherwise decompose it into its key/True pairs.)
                ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..) => elems.push(val),
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                _ if is_real_array && n == 1 => elems.extend(runtime::value_to_list(&val)),
                _ => elems.push(val),
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
            match val.view() {
                ValueView::Slip(items) => elems.extend(items.iter().cloned()),
                // Nil in list context contributes nothing (same as value_to_list).
                ValueView::Nil => {}
                _ => elems.push(val),
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
            match item.view() {
                ValueView::Pair(k, v) => {
                    map.insert(k.clone(), v.clone());
                }
                ValueView::ValuePair(k, v) => {
                    map.insert(k.to_string_value(), v.clone());
                }
                _ => {
                    // Non-pair values: use stringified value as key mapped to True
                    map.insert(item.to_string_value(), Value::TRUE);
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
            self.locals[idx].view(),
            ValueView::Package(_)
                | ValueView::Array(..)
                | ValueView::Hash(..)
                | ValueView::Sub(..)
                | ValueView::Instance { .. }
                | ValueView::Proxy { .. }
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
            if let ValueView::Capture {
                positional: p,
                named: nm,
            } = val.view()
                && p.is_empty()
                && let Some(ValueView::Str(source_name)) =
                    nm.get("__mutsu_varref_name").map(Value::view)
                && let Some(inner) = nm.get("__mutsu_varref_value")
            {
                let source_name = source_name.to_string();
                let inner = inner.clone();
                positional.push(self.capture_var_cell(code, &source_name, inner));
                continue;
            }
            match val.view() {
                ValueView::Pair(k, v) => {
                    // A named scalar-var element (`\(:$a)`): the value is a
                    // WrapVarRef-tagged capture — box the named local so `$c<a>`
                    // aliases `$a` and `$c<a>++` writes through.
                    if let ValueView::Capture {
                        positional: p,
                        named: nm,
                    } = v.view()
                        && p.is_empty()
                        && let Some(ValueView::Str(source_name)) =
                            nm.get("__mutsu_varref_name").map(Value::view)
                        && let Some(inner) = nm.get("__mutsu_varref_value")
                    {
                        let source_name = source_name.to_string();
                        let inner = inner.clone();
                        let cell = self.capture_var_cell(code, &source_name, inner);
                        named.insert(k.clone(), cell);
                    } else {
                        named.insert(k.clone(), v.clone());
                    }
                }
                ValueView::Capture {
                    positional: p,
                    named: n,
                } => {
                    // Flatten inner capture (from |capture slip)
                    positional.extend(p.iter().cloned());
                    named.extend(n.iter().map(|(k, v)| (k.clone(), v.clone())));
                }
                ValueView::Slip(items) => {
                    for item in items.iter() {
                        match item.view() {
                            ValueView::Pair(k, v) => {
                                named.insert(k.clone(), v.clone());
                            }
                            _ => positional.push(item.clone()),
                        }
                    }
                }
                _ => positional.push(val),
            }
        }
        self.stack.push(Value::capture(positional, named));
    }
}
