use super::*;

impl Interpreter {
    /// ADR-0049 (slices 1-2): a real `Array`/`Hash` element is a `Scalar`
    /// container, and a `Scalar` cannot hold `Nil` -- storing `Nil` into one
    /// restores the *owning container's own* default (`is default(...)` ->
    /// native element zero -> declared element type object -> `Any`, exactly
    /// `Interpreter::typed_container_default`). Call this once per container
    /// right after it is fully built (and, for a typed/`.new` construction,
    /// after `tag_container_metadata` has attached its type info), so a
    /// nested literal decays inside-out (`[[Nil]] eqv [[Any]]` — ADR-0049
    /// S1.5): the inner array's own construction op already ran this hook
    /// before the outer one sees it.
    ///
    /// A no-op for anything `typed_container_default` cannot decay -- `List`,
    /// `Seq`, and other non-container values, whose elements are not
    /// `Scalar`s and legitimately keep a stored `Nil` (ADR-0049 S1.4 I1-I3).
    pub(crate) fn decay_nil_container_elements(&mut self, mut value: Value) -> Value {
        let default = self.typed_container_default(&value);
        if default.is_nil() {
            return value;
        }
        let decayed_array = value
            .with_array_mut(|items, _kind| {
                let data = crate::gc::Gc::make_mut(items);
                for item in data.items_mut() {
                    if item.is_nil() {
                        *item = default.clone();
                    }
                }
            })
            .is_some();
        if !decayed_array {
            value.with_hash_mut(|items| {
                let data = crate::gc::Gc::make_mut(items);
                for v in data.map.values_mut() {
                    if v.is_nil() {
                        *v = default.clone();
                    }
                }
            });
        }
        value
    }

    pub(super) fn exec_make_array_op(
        &mut self,
        code: &CompiledCode,
        n: u32,
        is_real_array: bool,
    ) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            // A `WrapVarRef`-tagged scalar variable element of a List (`($a, $b)`):
            // store the variable's shared `ContainerRef` cell so the List aliases
            // `$a`'s container and a later mutation is visible when the List is
            // read. A ContainerRef is a scalar item, so it never flattens. (Only
            // Lists carry this tag -- bracket arrays `[...]` decontainerize.)
            if let ValueView::VarRef {
                name: source_name,
                value: inner,
                ..
            } = val.view()
            {
                let source_name = source_name.resolve();
                let inner = inner.clone();
                let slot_hint = val.varref_slot();
                elems.push(self.capture_var_cell_inner(code, &source_name, inner, true, slot_hint));
                continue;
            }
            // Reify a not-yet-read Seq source (deferred Iterator or
            // IO::Handle.lines — ADR-0034) into an eager array element.
            let val = if let ValueView::Seq(body) = val.view()
                && body.needs_touch()
            {
                let body = std::sync::Arc::clone(&body);
                match self.reify_seq_body(&body) {
                    Ok(_) => val.clone(),
                    Err(_) => continue,
                }
            } else if let ValueView::LazyList(ll) = val.view()
                && ((ll.coroutine.is_some()
                    && ll.sequence_spec.is_none()
                    && ll.scan_spec.is_none()
                    && ll.lazy_pipe.is_none())
                    || ll.has_finite_closure_endpoint())
                && !matches!(
                    ll.env
                        .get("__mutsu_preserve_lazy_on_array_assign")
                        .map(Value::view),
                    Some(ValueView::Bool(true))
                )
            {
                // Array literals (`[...]`) are eager: a finite gather/take or
                // finite-endpoint closure sequence must run now so its elements
                // materialize. Unbounded `... *` sequences and lazy pipelines
                // stay lazy; a `lazy`-marked list stays lazy too.
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
                // Buf/Blob are Positional but NOT Iterable, so the one-arg
                // flatten rule keeps them whole too: `[$str.encode]` is one
                // element, even though `.list`/`.rotor`/`for` yield the bytes.
                ValueView::Instance { attributes, .. }
                    if crate::value::value_buf::has_buf_elems(&attributes) =>
                {
                    elems.push(val)
                }
                // A single infinite *integer* range (`[1..Inf]`, `[1..*]`,
                // `[^Inf]`, `[0..^*]`) keeps the `[...]` array lazy: build the
                // same reify-on-demand lazy array `my @a = 1..Inf` produces, so
                // `.is-lazy` is True and `.elems` throws `X::Cannot::Lazy`
                // instead of materializing a MAX_RANGE_EXPAND-capped finite
                // prefix. (n == 1 guarantees this is the whole array.)
                _ if is_real_array
                    && n == 1
                    && let Some(lazy) = runtime::utils::infinite_int_range_to_lazy_array(&val) =>
                {
                    self.stack.push(lazy);
                    return Ok(());
                }
                // A single genuinely-*infinite* lazy list (an infinite `...`
                // sequence like `[1,2,3...*]` / `[-Inf...Inf]`, or a lazy map/grep
                // pipe over an infinite source) keeps the `[...]` array lazy,
                // exactly as `my @a = 1,2,3...*` does: `.is-lazy` is True and
                // `.elems` throws `X::Cannot::Lazy` instead of eagerly materializing
                // (which would hang or truncate to the seed cache). Mirrors the
                // infinite-int-range arm above for the sequence/pipe case. A merely
                // `lazy`-marked *finite* list (`[lazy 1, 2]`) is deliberately NOT
                // preserved here — it still materializes so whole-array reads like
                // `cmp` don't mis-read the seed cache (roast S03-operators/cmp.t
                // "lazy array comparisons").
                ValueView::LazyList(ll) if is_real_array && n == 1 && ll.is_lazy_infinite() => {
                    self.stack.push(Value::lazy_list(crate::gc::Gc::new(
                        ll.with_array_context(),
                    )));
                    return Ok(());
                }
                // In bracket-array literals (`[...]`), a single element is in
                // list context and should flatten one level (e.g. `[2..6]`,
                // `[@a]`, `[(1,2,3)]`), while multi-element forms keep each
                // element itemized (e.g. `[(1,2),(3,4)]`).
                _ if is_real_array && n == 1 => {
                    // A single user Iterable instance reifies through its own
                    // `iterator` method, exactly like `my @a = $iterable`
                    // (`[ $csv.error_diag ]` lists CSV::Diag's six fields —
                    // Text::CSV t/80_diag.t). Multi-element forms keep the
                    // instance whole, matching raku. (A Buf/Blob instance has
                    // no user `iterator`, so it takes its dedicated arm above.)
                    if matches!(val.view(), ValueView::Instance { .. })
                        && let Some(items) = self.try_iterable_instance_items(&val)?
                    {
                        elems.extend(items);
                    } else {
                        elems.extend(runtime::value_to_list(&val));
                    }
                }
                _ => elems.push(val),
            }
        }
        // `use fatal`: a list/array literal must not silently embed an
        // unhandled Failure produced by one of its elements -- explode here,
        // before the composite becomes a stored value.
        self.explode_if_fatal_failure_in_composite(&elems)?;
        let result = if is_real_array {
            Value::real_array(elems)
        } else {
            Value::array(elems)
        };
        // ADR-0049: decay a stored `Nil` element to the array's own default.
        // A no-op for the `List` (non-real) branch above, since
        // `typed_container_default` returns `Nil` (meaning "no decay") for a
        // non-real-array container.
        let result = self.decay_nil_container_elements(result);
        // ADR-0040 slice 2: a real `[...]` literal's elements are `Scalar`
        // containers, so aggregates itemize; a `(...)` List literal's are not
        // (§1.6), which `itemize_real_array_elements` discriminates by kind.
        // Applied AFTER the per-element one-arg/Slip flattening decision
        // above, so arity is untouched (§2 part 3).
        let result = runtime::utils::itemize_real_array_elements(result);
        self.stack.push(result);
        Ok(())
    }

    /// Like `exec_make_array_op` with `is_real_array=true` but never flattens
    /// single elements. Used for bracket arrays with trailing comma (`[x,]`)
    /// and for `[$scalar]` / `[$%h]` to prevent hash/array flattening.
    pub(super) fn exec_make_array_no_flatten_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let raw: Vec<Value> = self.stack.drain(start..).collect();
        let mut elems = Vec::with_capacity(raw.len());
        for val in raw {
            match val.view() {
                ValueView::Slip(items) => elems.extend(items.iter().cloned()),
                // ADR-0049 slice 1: a bare `Nil` is a real element here, not a
                // List-context no-op -- it decays to the array's own default
                // below (`[Nil,].elems` is `1`, not a silently dropped `0`).
                _ => elems.push(val),
            }
        }
        // `use fatal`: see the comment in `exec_make_array_op`.
        self.explode_if_fatal_failure_in_composite(&elems)?;
        let result = Value::real_array(elems);
        let result = self.decay_nil_container_elements(result);
        // ADR-0040 slice 2: see `exec_make_array_op`.
        let result = runtime::utils::itemize_real_array_elements(result);
        self.stack.push(result);
        Ok(())
    }

    pub(super) fn exec_make_hash_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n * 2;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        // `use fatal`: see the comment in `exec_make_array_op`. A hash
        // literal's key/value pairs are pushed flat (key, val, key, val, ...),
        // so scanning the whole `items` slice checks both sides.
        self.explode_if_fatal_failure_in_composite(&items)?;
        let mut map = HashMap::new();
        for pair in items.chunks(2) {
            // A bare type-object key (`%(Int, 1)`) stringifies to "" with the
            // Rakudo "uninitialized value in string context" warning (or a user
            // `.Str`/`.Stringy`), matching the `my %h = (Int, 1)` list path.
            let key = if matches!(pair[0].view(), ValueView::Package(_)) {
                self.coerce_type_object_hash_key(&pair[0])?
            } else {
                Value::hash_key_encode(&pair[0])
            };
            // ADR-0040 slice 2: a `%(...)` literal's values are `Scalar`
            // containers, so an aggregate value itemizes on the way in.
            let val = pair[1].clone().itemize_for_element_store();
            map.insert(key, val);
        }
        let result = self.decay_nil_container_elements(Value::hash(map));
        self.stack.push(result);
        Ok(())
    }

    /// Build a Hash from N Pair values on the stack (from `%(k=>v, ...)` syntax).
    pub(super) fn exec_make_hash_from_pairs_op(&mut self, n: u32) -> Result<(), RuntimeError> {
        let n = n as usize;
        let start = self.stack.len() - n;
        let items: Vec<Value> = self.stack.drain(start..).collect();
        // `use fatal`: see the comment in `exec_make_array_op`. Elements here
        // are `Pair`/`ValuePair` values, so the composite check descends one
        // level into the pair's value.
        self.explode_if_fatal_failure_in_composite(&items)?;
        let mut map = HashMap::new();
        for item in items {
            match item.view() {
                // ADR-0040 slice 2: a `%(...)` literal's values are `Scalar`
                // containers, so an aggregate value itemizes on the way in.
                ValueView::Pair(k, v) => {
                    map.insert(k.clone(), v.clone().itemize_for_element_store());
                }
                // A Junction key (`%( "a"|"b" => 1 )`) threads: it stores the value
                // under each member key (`%h<a> == %h<b> == 1`), not under the
                // junction's stringification. Matches Rakudo.
                ValueView::ValuePair(k, v) => {
                    let v = v.clone().itemize_for_element_store();
                    for kk in crate::runtime::utils::hash_pair_keys(k) {
                        map.insert(kk.to_string_value(), v.clone());
                    }
                }
                _ => {
                    // Non-pair values: use stringified value as key mapped to True
                    map.insert(item.to_string_value(), Value::TRUE);
                }
            }
        }
        let result = self.decay_nil_container_elements(Value::hash(map));
        self.stack.push(result);
        Ok(())
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
        slot_hint: Option<u32>,
    ) -> Value {
        self.capture_var_cell_inner(code, name, inner, false, slot_hint)
    }

    /// Like `capture_var_cell`, but when `box_type_objects` is set a plain type
    /// object (an uninitialized `my $a` holds `Any`) is also boxed into a fresh
    /// `ContainerRef` cell. This is required for List container aliasing: four
    /// distinct uninitialized `my` scalars must be four distinct containers
    /// (`$a, $b X!=:= $c, $d` is all-True), which only holds if each gets its own
    /// cell rather than falling back to the shared `Any` type object.
    pub(super) fn capture_var_cell_inner(
        &mut self,
        code: &CompiledCode,
        name: &str,
        inner: Value,
        box_type_objects: bool,
        slot_hint: Option<u32>,
    ) -> Value {
        if inner.is_container_ref() {
            return inner;
        }
        // An `is raw`/`is rw` PARAMETER's own local slot may already hold the
        // caller's real shared cell: `bind_function_args_values`'s
        // `rw_shared_cell_key` mechanism boxes it there at call time so the
        // param, the caller's variable, and any relayed alias observe one
        // container (todo/tickets/is-raw-param-container-identity.md). That
        // bind ALSO registers a `__mutsu_sigilless_alias::` entry (for a
        // possible later `:=` through the param) pointing at the CALLER's
        // source name -- which is never a local of THIS frame. Consulting the
        // alias root below before checking the untouched slot would discard
        // `slot_hint` (belonging to the param's own name) and search for the
        // caller's name instead, find no local, and fall through to boxing a
        // brand-new, disconnected cell. Check the original name's own slot
        // first: if it is already a container ref, it unambiguously IS the
        // right cell to reuse, regardless of what any alias entry says.
        if let Some(hint) = slot_hint
            && hint != u32::MAX
            && code.locals.get(hint as usize).map(String::as_str) == Some(name)
            && self.locals[hint as usize].is_container_ref()
        {
            return self.locals[hint as usize].clone();
        }
        // A `:=`-bound scalar shares its binding root's container, so it must box
        // into the SAME cell (`$c := $b; $a, $b X=:= $c, $d` has exactly one True
        // pair — `$b =:= $c`). The bind is tracked by name
        // (`__mutsu_sigilless_alias::`), so resolve the root and box its slot;
        // both `($a,$b)` (boxing `b`) and `($c,$d)` (boxing `c`→root `b`) then
        // share `b`'s cell regardless of construction order. Unbound names
        // resolve to themselves at the cost of one env lookup.
        let root = self.resolve_alias_root(name);
        let use_hint = root == name;
        let name: &str = root.as_str();
        // Prefer the compile-time-resolved slot from the `WrapVarRef` site:
        // shadow slots give several `code.locals` entries the same name, and
        // the by-name `rposition` fallback would pick the LAST one — an inner
        // shadow's (possibly dead) slot — then poison env[name] with a cell
        // holding that slot's stale value (the CSV::Table comment-strip
        // state-sync bug, t/list-alias-shadowed-name.t). `Some(u32::MAX)`
        // means the compiler proved the name is NOT a local of this frame
        // (an env-based loop param, a captured outer) — do not guess a slot
        // by name there either, for the same shadow-collision reason. The
        // hint is dropped when `:=` aliasing redirected the name to a
        // different root variable, and the legacy by-name search remains for
        // VarRefs built without compiler slot info (`slot_hint: None`).
        let idx = match slot_hint {
            Some(hint)
                if use_hint && code.locals.get(hint as usize).map(String::as_str) == Some(name) =>
            {
                Some(hint as usize)
            }
            Some(hint) if use_hint && hint == u32::MAX => None,
            _ => code.locals.iter().rposition(|n| n == name),
        };
        let Some(idx) = idx else {
            // The named scalar is not a local of this frame (a captured/outer
            // variable read through the closure env), so there is no slot to box
            // into a shared cell. For List aliasing (`box_type_objects`) the
            // element must still stay a single *itemized* item: a `$`-scalar
            // holding an aggregate does NOT flatten in list/hash context
            // (`my $h = %x; my %c = ($h,)` dies "Odd number", `@a = ($h,)` is one
            // element). Without a cell the write-through alias is unavailable for
            // a captured variable anyway, so itemize the value to preserve the
            // non-flatten semantics (mirrors the old compiler `Itemize` path).
            if box_type_objects {
                return Self::itemize_value(inner);
            }
            return inner;
        };
        if self.locals[idx].is_container_ref() {
            return self.locals[idx].clone();
        }
        // Only box a plain scalar container; genuine reference values are not
        // re-containerized (mirrors the box-on-capture guard). A bare type object
        // (`Any`) is boxed only for List aliasing (`box_type_objects`), so that
        // distinct uninitialized scalars stay distinct containers.
        let is_reference = matches!(
            self.locals[idx].view(),
            ValueView::Array(..)
                | ValueView::Hash(..)
                | ValueView::Sub(..)
                | ValueView::Instance { .. }
                | ValueView::Proxy { .. }
        );
        let is_type_object = matches!(self.locals[idx].view(), ValueView::Package(_));
        if is_reference || (is_type_object && !box_type_objects) {
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
            if let ValueView::VarRef {
                name: source_name,
                value: inner,
                ..
            } = val.view()
            {
                let source_name = source_name.resolve();
                let inner = inner.clone();
                let slot_hint = val.varref_slot();
                positional.push(self.capture_var_cell(code, &source_name, inner, slot_hint));
                continue;
            }
            match val.view() {
                ValueView::Pair(k, v) => {
                    // A named scalar-var element (`\(:$a)`): the value is a
                    // WrapVarRef-tagged capture — box the named local so `$c<a>`
                    // aliases `$a` and `$c<a>++` writes through.
                    if let ValueView::VarRef {
                        name: source_name,
                        value: inner,
                        ..
                    } = v.view()
                    {
                        let source_name = source_name.resolve();
                        let inner = inner.clone();
                        let slot_hint = v.varref_slot();
                        let cell = self.capture_var_cell(code, &source_name, inner, slot_hint);
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
