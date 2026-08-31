use super::*;

impl Interpreter {
    /// Env key marking a variable as a genuine bound array SLICE (`@slice :=
    /// @array[1,2]`, §4 BLOCKERS.md test 15) — set only at the exact bind
    /// moment that produces an array of per-element `ContainerRef` cells, and
    /// cleared on every redeclaration of the same name. See the write-through
    /// checks in `vm_var_assign_local.rs` and this file's non-bind path for
    /// why "elements are cells" alone is not a safe trigger.
    pub(crate) fn bound_array_slice_marker_key(name: &str) -> String {
        runtime::bound_array_slice_key(name)
    }

    /// If `name` is a raw `\target` bound to a multi-dim slice lvalue (marked at
    /// bind time by `is_multidim_slice_cells`) whose current value `holder` is a
    /// non-empty list of `ContainerRef` cells, distribute `rhs` element-wise
    /// through the cells (mutating the real nested container) and return the
    /// value to push as the assignment result. Returns `None` when this is not
    /// such a binding, so the caller falls back to normal assignment.
    ///
    /// The bind-time marker — NOT "all elements are cells" — is the trigger:
    /// `.grep`'s rw-topic promotion can also leave a cell-list in an unrelated
    /// scalar (`$x = $x.grep(...)`, roast S03-operators/assign.t) that must keep
    /// plain replace semantics.
    pub(crate) fn distribute_bound_multidim_slice(
        &mut self,
        name: &str,
        holder: &Value,
        rhs: &Value,
    ) -> Option<Result<(), RuntimeError>> {
        if name.starts_with('@') || name.starts_with('%') || name.starts_with('&') {
            return None;
        }
        // No `__mutsu_bound_array_slice::*` marker was ever created (the
        // common program): skip the per-assignment `format!` + env probe.
        if !crate::env::bound_array_slice_possible() {
            return None;
        }
        if !matches!(
            self.env()
                .get(&Self::bound_array_slice_marker_key(name))
                .map(Value::view),
            Some(ValueView::Bool(true))
        ) {
            return None;
        }
        let cells: Vec<Value> = match holder.view() {
            ValueView::Array(items, ..)
                if !items.is_empty() && items.iter().all(Value::is_container_ref) =>
            {
                items.iter().cloned().collect()
            }
            _ => return None,
        };
        let rhs_vals = match self.assignment_rhs_values(rhs) {
            Ok(v) => v,
            Err(e) => return Some(Err(e)),
        };
        for (i, cell_val) in cells.iter().enumerate() {
            let v = rhs_vals
                .get(i)
                .cloned()
                .unwrap_or_else(|| Value::package(crate::symbol::Symbol::intern("Any")));
            if let ValueView::ContainerRef(cell) = cell_val.view() {
                cell.lock().unwrap().clone_from(&v);
            }
        }
        Some(Ok(()))
    }

    /// `:=` bind of a Positional value to an `@`-sigiled target: preserves the
    /// container type (e.g. List stays List) instead of copying into a fresh
    /// Array. Shared by the SetLocal path (`my @x := ...`) and the SetGlobal
    /// path (`@!attr := ...`, `our @x := ...`).
    ///
    /// Type-check: only Positional values can be bound to @-sigiled vars.
    /// A single-index terminal element bind (`@x := @array[2]`) may arrive
    /// here as a `ContainerRef` cell (promoted so a later whole-array
    /// assignment writes through the source element) — decont it first so the
    /// Positional check inspects the actual bound value, not the cell wrapper.
    pub(crate) fn bind_positional_value(
        &mut self,
        name: &str,
        raw_popped: &Value,
    ) -> Result<Value, RuntimeError> {
        let decontained_popped = raw_popped.deref_container();
        // `but`/`does` wraps the value in a `Mixin` view, which none of the
        // arms below matched — so `my @p := <a b> but R` failed the bind even
        // though `$p ~~ Positional` (a different oracle) answered True. A
        // composition can only ADD to what the wrapped value does, so a mixin
        // is Positional exactly when its inner value is, or when one of the
        // composed roles is/does `Positional`.
        let mut mixin_composes_positional = false;
        let decontained_popped = match decontained_popped.view() {
            ValueView::Mixin(inner, mixins) => {
                mixin_composes_positional = mixins.keys().any(|k| {
                    k.strip_prefix("__mutsu_role__")
                        .is_some_and(|role| role == "Positional")
                });
                inner.as_ref().clone()
            }
            _ => decontained_popped,
        };
        let is_positional = mixin_composes_positional
            || match decontained_popped.view() {
                ValueView::Array(..)
                | ValueView::LazyList(_)
                | ValueView::Seq(_)
                | ValueView::Slip(_)
                | ValueView::Range(..)
                | ValueView::RangeExcl(..)
                | ValueView::RangeExclStart(..)
                | ValueView::RangeExclBoth(..)
                | ValueView::GenericRange { .. }
                | ValueView::Uni { .. }
                | ValueView::Nil => true,
                // A Positional TYPE OBJECT binds too (`my @x := Positional[Dog]`,
                // JSON::Unmarshal's attribute-type flow): raku accepts it and
                // `.of` reads the parametric element type.
                ValueView::Package(name) => {
                    let n = name.resolve();
                    let base = n.split('[').next().unwrap_or(&n);
                    matches!(
                        base,
                        "Positional" | "Array" | "List" | "Seq" | "Slip" | "Range" | "Buf" | "Blob"
                    ) || self
                        .class_composed_roles(base)
                        .is_some_and(|roles| roles.iter().any(|r| r == "Positional"))
                }
                // Instance objects are Positional only if they implement
                // the Positional role (or Array subclass etc.), but not
                // Failure or arbitrary classes.
                ValueView::Instance {
                    class_name,
                    attributes,
                    ..
                } => {
                    let cn = class_name.resolve();
                    matches!(
                        cn.as_str(),
                        "Array"
                            | "List"
                            | "Slip"
                            | "Seq"
                            | "Range"
                            | "Buf"
                            | "Blob"
                            | "utf8"
                            | "buf8"
                            | "buf16"
                            | "buf32"
                    ) || self
                        .class_composed_roles(&cn)
                        .is_some_and(|roles| roles.iter().any(|r| r == "Positional"))
                        || attributes.contains_key("__mutsu_array_storage")
                }
                _ => false,
            };
        if !is_positional {
            let got_type = crate::runtime::utils::value_type_name(raw_popped);
            let mut attrs = std::collections::HashMap::new();
            attrs.insert("got".to_string(), raw_popped.clone());
            attrs.insert(
                "expected".to_string(),
                Value::package(crate::symbol::Symbol::intern("Positional")),
            );
            attrs.insert("symbol".to_string(), Value::str_from(name));
            attrs.insert(
                "message".to_string(),
                Value::str(format!(
                    "Type check failed in binding; expected Positional but got {}",
                    got_type
                )),
            );
            let ex = Value::make_instance(
                crate::symbol::Symbol::intern("X::TypeCheck::Binding"),
                attrs,
            );
            let mut err = RuntimeError::new(format!(
                "Type check failed in binding; expected Positional but got {}",
                got_type
            ));
            err.exception = Some(Box::new(ex));
            return Err(err);
        }
        Ok(match raw_popped.view() {
            // `:=` binds the container itself rather than copying values
            // into a fresh Array, so — unlike plain `=` assignment, which
            // must stay conservative about mutation support (see
            // docs/lazy-arrays.md L2) — ANY genuinely-lazy list (gather
            // coroutine, infinite sequence/closure spec, lazy pipe, scan)
            // can be bound without forcing. A *plain* (non-`lazy`-marked)
            // gather is `.is-lazy` False (`is_genuinely_lazy()` alone
            // would miss it) but binding must still not force it — that
            // is the whole point of `:=` vs `=` (t/gather-lazy.t tests
            // 1/3) — so `coroutine.is_some()` is checked too. Tag it as
            // living in `@` array context so gist/`.WHAT` render
            // `[...]`/`Array` rather than `(...)`/`Seq`.
            ValueView::LazyList(list) if list.coroutine.is_some() || list.is_genuinely_lazy() => {
                Value::lazy_list(crate::gc::Gc::new(list.with_array_context()))
            }
            ValueView::LazyList(list) => Value::real_array(self.force_lazy_list_vm(&list)?),
            ValueView::Seq(body) if body.is_io_lines_source() => {
                let body = std::sync::Arc::clone(&body);
                let pulled = self.reify_seq_body(&body)?;
                Value::real_array(pulled)
            }
            // `@a := $x` strips the Scalar container: the @-var binds the
            // Array itself (same backing data, plain kind), not the item.
            ValueView::Array(items, kind) if kind.is_itemized() => {
                Value::array_with_kind(items.clone(), kind.decontainerize())
            }
            _ => raw_popped.clone(),
        })
    }

    pub(super) fn exec_set_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        // A re-assignment to a tied container (`my %h is Foo; %h = ...`, Foo
        // `does Associative`/`Positional`) routes through the class's `STORE`,
        // preserving the tied instance, instead of overwriting the slot with a
        // plain Hash/Array. Gated on `!vardecl_context` so a fresh declaration
        // (`my %h = ...`) is untouched.
        if !self.vardecl_context.get()
            && let Some(()) = self.maybe_tied_store_reassign(code, idx as usize)?
        {
            return Ok(());
        }
        // Named-sub declaration boxing decision: a scalar local that a
        // directly-nested *named sub* writes (`needs_cell_named_sub`) becomes a
        // shared `ContainerRef` cell so the named sub's by-name env writes and the
        // owner's slot reads alias one cell, carrying cross-call accumulation
        // (`via(); via()`) through the cell instead of the `env_dirty` blanket
        // reconcile (see docs/captured-outer-cell-sharing.md).
        //
        // Boxing and the blanket reconcile are MUTUALLY EXCLUSIVE coherence
        // mechanisms — exactly one is active. Gating boxing on
        // `blanket_reconcile_disabled()` keeps the DEFAULT (shipping) build
        // byte-identical to before (reconcile carries coherence, no boxing), while
        // the `MUTSU_NO_BLANKET_RECONCILE` build activates boxing. This is what lets
        // the slice land safely: with the reconcile on, cell-sharing would
        // propagate a captured-outer write that the reconcile happens to drop
        // through some carriers (e.g. `lives-ok { ... }`), and that extra
        // propagation surfaces unrelated latent bugs (a missing `&foo = &foo`
        // self-reference type-check, etc.). Those are fixed as the surface is
        // migrated; until then boxing is exercised only under the toggle, where the
        // pins prove it carries the named-sub accumulation surface.
        //
        // `vardecl_context` is consumed inside the inner handler, so snapshot it
        // here. Gated on a non-empty `needs_cell_named_sub`, so ordinary code (e.g.
        // `fib`) pays nothing. Such a captured local is `needs_env_sync`
        // (non-simple), so the boxing must wrap BOTH the fast and slow inner paths
        // — hence it lives here, not inside one branch. Deliberately keyed on
        // `needs_cell_named_sub`, NOT the closure-driven `needs_cell_locals`:
        // closures are boxed precisely at their creation op, so reusing that set
        // here would over-box unrelated same-named locals (same-named `my` locals
        // share one slot) and break e.g. `let`-restore in a sibling block.
        let box_decl = self.vardecl_context.get()
            && (!code.needs_cell_named_sub.is_empty()
                || !code.needs_cell_ref_capture_slots.is_empty());
        // An `our sub` declared in a bare block captures this local but outlives the
        // block (it lives in the package registry, with no closure env). Box the
        // local AND persist the cell so a call after the block reads the live value.
        let box_decl_our =
            self.vardecl_context.get() && !code.needs_cell_escaping_our_sub.is_empty();
        // A DECLARATION must not reverse-sync its slot out to an `our` package
        // variable: a `my`/`state` is a fresh lexical (and same-named locals
        // share one slot, so a mainline `my $x` reaches the slot an in-file
        // `module M { our $x }` linked), while an `our` declaration publishes
        // itself explicitly right after this store. `vardecl_context` is
        // consumed inside the inner handler, so snapshot it here.
        let is_vardecl = self.vardecl_context.get();
        // Container-descriptor naming (`@kh.VAR.name`): a plain `my @x`/`my %h`
        // declaration stamps the variable name into the fresh container below,
        // after the store. A `:=` bind keeps the bound container's original
        // name (rakudo first-name-wins), so detect the bind shape up front —
        // the flags are consumed inside the inner handler.
        let stamp_decl_name = self.vardecl_context.get()
            && !self.bind_context.get()
            && !self.scalar_bind_context.get()
            && !matches!(
                self.stack.last().map(Value::view),
                Some(ValueView::VarRef { .. })
            )
            && code.locals.get(idx as usize).is_some_and(|n| {
                (n.starts_with('@') || n.starts_with('%'))
                    && !n[1..].starts_with(['!', '.'])
                    && !n.contains("__ANON")
            });
        let r = self.exec_set_local_op_inner(code, idx);
        // The store that ends a declaration's in-flight window: from here the
        // slot holds the new binding, so a spawn may unmask the name again (see
        // `thread_decl_in_flight`). Only ever non-empty in threaded programs.
        if !self.thread_decl_in_flight.is_empty()
            && let Some(name) = code.locals.get(idx as usize)
        {
            self.thread_decl_in_flight.remove(name);
        }
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            if stamp_decl_name {
                let name = code.locals[idx as usize].clone();
                if let Some(val) = self.locals.get_mut(idx as usize) {
                    val.stamp_descriptor_name(&name);
                }
            }
            self.sync_our_package_var_from_local(code, idx as usize, is_vardecl);
            self.mirror_attr_local_to_cell(code, idx as usize);
            if box_decl
                && (code
                    .locals_sym
                    .get(idx as usize)
                    .is_some_and(|sym| code.needs_cell_named_sub.contains(sym))
                    || code.needs_cell_ref_capture_slots.contains(&idx))
            {
                self.box_decl_local_cell(code, idx as usize);
            }
            if box_decl_our
                && let Some(sym) = code.locals_sym.get(idx as usize)
                && code.needs_cell_escaping_our_sub.contains(sym)
            {
                // Box the captured local into a shared cell so the escaped `our` sub
                // and the owner alias one cell. The PERSIST into
                // `escaped_our_lexical_cells` is deferred to the sub's source-order
                // `RegisterSub` (see `exec_register_sub_op`), NOT done here — same-
                // named `my` locals share one slot, so an unrelated sibling-block
                // `my $a` reaches this same site and must not pollute the persisted
                // map with its value.
                self.box_decl_local_cell(code, idx as usize);
            }
        }
        r
    }

    pub(crate) fn exec_set_local_op_inner(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let idx = idx as usize;
        // A whole `%`/`@` (re)assignment or rebind replaces the container, breaking
        // every `:=`-bound element — drop the read-only-element markers so a later
        // `%h<k> = v` is writable again (`%h<i> := 137; %h = (...)`).
        if crate::env::elem_index_meta_possible() {
            let name = &code.locals[idx];
            if name.starts_with('%') || name.starts_with('@') {
                let name = name.clone();
                self.clear_all_ro_index(&name);
            }
        }
        let raw_popped = self.stack.pop().unwrap_or(Value::NIL);
        // Check if we're trying to write to a private instance attribute (!attr)
        // when self is a type object (not an instance). Raku says this should die.
        let local_name = &code.locals[idx];
        if local_name.starts_with('!')
            && local_name.len() > 1
            && let Some(self_val) = self.get_env_with_main_alias("self")
            // Read through a ContainerRef: `$outer := self` rewrites the frame's
            // `self` into the bind's shared cell, but it still holds the instance.
            && !self_val.with_deref(|v| {
                matches!(v.view(), ValueView::Instance { .. } | ValueView::Mixin(..))
            })
        {
            // self is a type object - determine class name for error message
            let class_name = match self_val.view() {
                ValueView::Package(sym) => sym.to_string(),
                _ => crate::value::what_type_name(&self_val),
            };
            return Err(RuntimeError::new(format!(
                "Cannot look up attributes in a {} type object. Did you forget a '.new'?",
                class_name
            )));
        }
        // The compiler's own resolution of a `:=` bind source (see
        // `bind_source_is_own_frame_lexical`), kept before the wrapper is
        // stripped.
        let bind_source_slot = raw_popped.varref_slot();
        let (mut raw_popped, bind_source) = Self::extract_varref_binding(raw_popped);
        let is_bind = self.bind_context.get() || bind_source.is_some();
        let is_rebind = self.rebind_context.get();
        let is_constant = self.constant_context.get();
        let has_explicit_initializer = self.explicit_initializer_context.get();
        let is_vardecl = self.vardecl_context.get();
        let is_shaped_decl = self.shaped_decl_context;
        let scalar_bind = self.scalar_bind_context.get();
        let param_raw_bind = self.param_raw_bind_context.get();
        let array_share = self.array_share_context.get();
        self.bind_context.set(false);
        self.scalar_bind_context.set(false);
        self.param_raw_bind_context.set(false);
        self.rebind_context.set(false);
        self.constant_context.set(false);
        self.array_share_context.set(false);
        self.explicit_initializer_context.set(false);
        self.vardecl_context.set(false);
        self.shaped_decl_context = false;
        // A `Seq.new($iterator)` stores its iterator deferred (empty backing vec).
        // Assigning it to an `@`/`%` container reifies the Seq (raku list
        // semantics), so pull every element from the iterator first — the array/
        // hash coercion below (`value_to_list`/`build_hash_from_items`) reads the
        // items directly and would otherwise store nothing. A `$` scalar target
        // keeps the Seq lazy, and a `:=` bind must not consume it, so both are
        // excluded.
        if !is_bind
            && !is_rebind
            && let ValueView::Seq(body) = raw_popped.view()
        {
            let name = &code.locals[idx];
            if name.starts_with('@') || name.starts_with('%') {
                if body.needs_touch() {
                    let body = std::sync::Arc::clone(&body);
                    self.reify_seq_body(&body)?;
                    // `body` is shared by `raw_popped` (both alias the same
                    // `Arc<SeqBody>`), so the in-place reify above already
                    // updated it — no rebuild needed.
                }
            } else if name.starts_with('$') {
                // A plain `$s = SEQ` (or `my $s = SEQ`) assignment itemizes
                // the Seq into a Scalar container (raku container
                // semantics): from this point on a later "discarded in sink
                // context" read of this same value — even a bare `$s;`, or
                // this value flowing back out through a routine/closure
                // return whose caller discards it — must NOT force it
                // (`$s;` after `my $s = (gather die)[];` only warns "Useless
                // use of $s in sink context", it does not run the gather;
                // measured against raku). See `SeqBody::mark_itemized`.
                body.mark_itemized();
            }
        }
        // The `gather`/`take` coroutine machinery still represents a lazy
        // sequence as `ValueView::LazyList` (docs/adr/0034's `Seq`/`SeqBody`
        // split has not yet subsumed it) rather than `ValueView::Seq`, so it
        // needs the identical itemization exemption via its own value-level
        // `itemized` flag (LazyList has no shared mutable core to flip in
        // place the way `SeqBody::mark_itemized` does — the flag travels by
        // replacing the stored value with a tagged clone, same pattern as
        // `.cache`'s `with_cached_no_sink`).
        if !is_bind
            && !is_rebind
            && let ValueView::LazyList(list) = raw_popped.view()
            && !list.is_itemized()
        {
            let name = &code.locals[idx];
            if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
                raw_popped = Value::lazy_list(crate::gc::Gc::new(list.with_itemized()));
            }
        }
        // Slice 2a/2b: `$scalar = @arr` / `$scalar = %hash` / chained `$r = $q`
        // promotes the source container to a shared `ContainerRef` cell (raku
        // reference semantics). Handled before the decont marker and fast/slow
        // split because the scalar is itemized (NOT a `:=` decont alias) and needs
        // replace-on-reassign semantics distinct from `:=` write-through. Only
        // shares when the source value deref's to an Array/Hash, so a plain
        // `$x = $y` (scalar source) stays a copy.
        let array_share_source = self.array_share_source.take();
        if array_share
            && let Some(src) = array_share_source
            && raw_popped
                .with_deref(|v| matches!(v.view(), ValueView::Array(..) | ValueView::Hash(_)))
        {
            let name = &code.locals[idx];
            if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
                return self.array_share_assign(code, idx, raw_popped, src);
            }
        }
        // Plain `=` assignment stores a VALUE. A bare `ContainerRef` reaching an
        // assignment target is an lvalue return that nothing decontainerized on
        // the way (`my $c = f()` where `f` is `is rw` / ends in `return-rw`):
        // storing the cell itself would silently alias the routine's source, so
        // `$c = 99` would write it. Raku copies there — only `:=` (and rw
        // parameter binding, which never reaches this op) keeps the container.
        //
        // Every other producer of a `ContainerRef` already deconts at its read
        // chokepoint (`GetLocal`'s `into_deref`, `resolve_array_entry`, ...), so
        // this only ever fires for a value that came straight off a call. The
        // `array_share` promotion above has already returned, and `:=`/rebind/
        // `constant`/raw-param binds are excluded by the guard.
        if !is_bind
            && !is_rebind
            && !is_constant
            && !param_raw_bind
            && !scalar_bind
            && raw_popped.is_container_ref()
        {
            raw_popped = raw_popped.deref_container();
        }
        // Record/clear the decontainerize marker for `$` scalars. A scalar bound
        // (`:=`) to a Positional is not a Scalar container, so `@a = $bound` must
        // flatten (the `ItemizeVar` opcode reads this marker). Plain assignment
        // re-containerizes, clearing any stale marker. Done before the fast/slow
        // split so both paths are covered. `raw_popped` is only borrowed.
        {
            let name = &code.locals[idx];
            self.update_bound_decont_marker(name, scalar_bind || is_bind, &raw_popped);
        }
        // A scalar `:=` bound to a plain immutable VALUE with no named source
        // (`my $r := $obj.ro-attr` — a non-rw accessor result) is bound to that
        // value itself, not a container: a later `$r = v` is "Cannot assign to
        // an immutable value" in raku. Literal binds (`my $x := 5`) get this
        // from the parser's MarkReadonly too; this covers the runtime-only
        // cases.
        // Deliberately an ALLOWLIST of pure immutable scalar kinds: anything
        // container-like or writable-through (ContainerRef, Proxy STORE,
        // HashEntryRef deferred binds, `is raw` results, ...) must stay
        // writable, and an overlooked kind here turns into a hard runtime
        // error, so the conservative direction is to mark less.
        //
        // Only the *decision* is made here; the marking itself is applied
        // AFTER the declaration bookkeeping below, which now unconditionally
        // clears any stale readonly marking left by an earlier same-named
        // binding (see the `unmark_readonly` call in the `is_vardecl` block).
        let bind_marks_immutable = scalar_bind
            && bind_source.is_none()
            && matches!(
                raw_popped.view(),
                ValueView::Int(_)
                    | ValueView::BigInt(_)
                    | ValueView::Num(_)
                    | ValueView::Str(_)
                    | ValueView::Bool(_)
                    | ValueView::Rat(..)
                    | ValueView::Complex(..)
            );
        // A sigilless `\target` bound to a multi-dim slice lvalue distributes a
        // plain whole-value reassignment (`target = values`, e.g. as a sub's
        // bare-statement return value) element-wise through its cells — the
        // `SetLocal` counterpart of the same write-through in
        // `exec_assign_expr_local_op_inner` / `exec_assign_expr_op_inner`.
        // Excluded for a `:=` bind / declaration, which replaces the binding.
        if !is_bind
            && !is_vardecl
            && !is_rebind
            && !scalar_bind
            // Gate before the String/holder clones: no slice marker was ever
            // created in the common program.
            && crate::env::bound_array_slice_possible()
        {
            let name = code.locals[idx].clone();
            let holder = self.locals[idx].clone();
            if let Some(res) = self.distribute_bound_multidim_slice(&name, &holder, &raw_popped) {
                res?;
                self.stack.push(raw_popped);
                return Ok(());
            }
        }
        // A redeclaration (`my @a` in a new scope) must not inherit the
        // `is default(...)` trait from an earlier same-named variable,
        // since var_defaults is keyed only by name. Drop any stale entry;
        // if the current decl has its own `is default(...)` trait, the
        // trait op will re-set it immediately after.
        if is_vardecl {
            let name = &code.locals[idx];
            self.clear_var_default(name);
            // Clear the deleted-index tracker left over from a previous
            // same-named variable in an outer scope. (Pre-interned key: this
            // whole block runs on every declaration, so a loop-body `my $t`
            // pays it once per iteration.) Skipped outright when no such key has
            // ever been created -- the program never `:delete`d an index, so
            // there is nothing to clear.
            if crate::env::elem_index_meta_possible()
                && let Some(sym) = code.deleted_index_sym(idx)
            {
                self.env_mut().remove_sym(sym);
            }
            // Clear any sigilless-readonly flag inherited from an outer
            // scope (e.g. a for-loop `\result` shouldn't block `my $result`
            // in a called sub). Same latch: the `__mutsu_sigilless_readonly::*`
            // key only exists once the program creates a sigilless/`:=` alias.
            if crate::env::closure_meta_keys_possible()
                && let Some(sym) = code.readonly_sym(idx)
            {
                self.env_mut().remove_sym(sym);
            }
            // A fresh `my $x = ...` declaration (plain `=`, not `:=`) drops the
            // FORWARD sigilless `:=` alias (`alias::x = Y`) a PRIOR same-name
            // binding left, so `$x = v` no longer propagates to `Y`. Without
            // this, `my $a = h; $a := h` inside a loop leaks the NEXT iteration's
            // `my $a = ...` back into the loop variable `h` (the loop var is a
            // sigilless alias target). Only the FORWARD alias is dropped: reverse
            // aliases (`alias::Z = x`, another lexical bound TO `$x`) and
            // `local_bind_pairs` are preserved because a same-scope
            // redeclaration (`my $x = 2; my $y := $x; my $x = 3`) is the SAME
            // variable in raku, so `$y` must still track `$x`. `:=` (re)binds run
            // their own alias bookkeeping and are excluded.
            if !is_bind
                && !scalar_bind
                && crate::env::closure_meta_keys_possible()
                && let Some(sym) = code.alias_sym(idx)
            {
                self.env_mut().remove_sym(sym);
            }
            // Clear any readonly flag inherited from an earlier binding of this
            // bare name — a caller/outer-scope readonly param
            // (`sub f(Str $x){...}` vs. a callee's own `my $x`), or a previous
            // `my $x := <literal>` whose scope has already been left.
            // `readonly_vars` is keyed by bare name and is NOT cleared on
            // function entry or block exit, so without this a *fresh*
            // declaration inherits a marking that no longer describes it.
            //
            // This runs for `:=` declarations too. A declaration is the point
            // at which the name's readonly state is (re)established from
            // scratch: the bind's own marking is applied right AFTER this block
            // (`bind_marks_immutable` below) and by the trait/`MarkVarReadonly`
            // ops the parser now emits after the declaration, so clearing here
            // can no longer erase THIS declaration's own marking — only a stale
            // one. Strip the sigil to match the bare key form used by
            // `check_readonly_for_modify`.
            // Nothing is readonly yet: skip the sigil-strip + `Symbol::intern` of
            // the bare name (this runs on every declaration).
            if !self.no_readonly_vars() {
                let bare = name.trim_start_matches(['$', '@', '%', '&']);
                self.unmark_readonly(bare);
            }
            // A self-recursive closure declaration (`my $f = -> $n { $f($n-1) }`)
            // boxed this very local into a cell while evaluating its own
            // initializer, and the closure captured that cell. Both clears below
            // would orphan it — leaving `$f` as `Any` inside the closure — so keep
            // the cell and let the store below write THROUGH it. See
            // `CompiledCode::self_capture_decl_locals`.
            let self_captured_decl = !code.self_capture_decl_locals.is_empty()
                && code
                    .locals_sym
                    .get(idx)
                    .is_some_and(|sym| code.self_capture_decl_locals.contains(sym));
            // Replace stale ContainerRef in env with Nil so a new `my $var`
            // doesn't inherit a binding from an earlier scope. Keep the key
            // so saved frame propagation can still find it. Probed through the
            // pre-interned local Symbol (a by-name `get` would re-intern on every
            // declaration).
            if !self_captured_decl
                && let Some(sym) = code.locals_sym.get(idx).copied()
                && matches!(
                    self.env().get_sym(sym).map(Value::view),
                    Some(ValueView::ContainerRef(_))
                )
            {
                // A package `our` scalar's cell is published under the BARE env
                // key too (`OpCode::DeclareOurScalar`), so a consumer that
                // loads the module and then declares its own same-named `my`
                // lands here. That cell is not an enclosing frame's lexical —
                // it belongs to the package, and is reachable through its
                // qualified key and the `our` store regardless — so the key is
                // REMOVED rather than left holding `Nil`. Leaving a `Nil`
                // behind poisons the name: it is a live positive env hit that
                // the env<->locals reconciliation can pull back over this
                // fresh binding, so `my $s = 'mine'` read as `Nil` after the
                // next call into the module. Every other cell keeps the
                // existing `Nil` overwrite, which preserves the key for saved
                // frame propagation.
                if self.our_scalar_cell_names.contains(&code.locals[idx]) {
                    self.env_mut().remove_sym(sym);
                } else {
                    self.env_mut().insert_sym(sym, Value::NIL);
                }
            }
            // Per-iteration freshness for box-on-capture (lever C Slice 2): if a
            // previous iteration's closure boxed this loop-body `my` into a
            // ContainerRef (now sitting in the slot), the redeclaration is a
            // *fresh binding* — clear the stale cell so the assignment below
            // writes a new plain value instead of writing *through* the old Arc
            // (which would corrupt the prior iteration's captured closure).
            if !self_captured_decl && matches!(self.locals[idx].view(), ValueView::ContainerRef(_))
            {
                self.locals[idx] = Value::NIL;
            }
            // Clear a stale bound-array-slice marker (§4 BLOCKERS.md test 15)
            // left over from an earlier same-named variable — a fresh
            // declaration (bind or not) must not inherit "this array's
            // elements are write-through cells" from a prior scope/iteration.
            // Re-set below, in the `is_bind` arm, only if THIS declaration is
            // genuinely a bound array slice. Skipped entirely when no such
            // marker was ever created (the common program).
            if crate::env::bound_array_slice_possible()
                && let Some(sym) = code.bound_slice_sym(idx)
            {
                self.env_mut().remove_sym(sym);
            }
        }

        // Apply the `:=`-to-immutable-literal marking decided above, now that
        // the declaration bookkeeping has cleared any stale marking for this
        // bare name. A `$` name bound straight to a literal has no container of
        // its own, so rakudo's assignment error is X::AdHoc "Cannot assign to
        // an immutable value".
        if bind_marks_immutable {
            let bare = code.locals[idx]
                .trim_start_matches(['$', '@', '%', '&'])
                .to_string();
            self.mark_readonly_with(&bare, crate::ast::ReadonlyKind::Immutable);
        }

        // Lazily convert pending alias bind names into local_bind_pairs.
        self.resolve_pending_alias_binds(code);

        let name = &code.locals[idx];
        // Bound array SLICE write-through (`@slice := @array[1,2]; @slice =
        // ...;` as a statement): the local's OWN elements are shared
        // `ContainerRef` cells (from the bind-time slice promotion, §4
        // BLOCKERS.md test 15). A whole-array assignment writes through each
        // cell instead of replacing the slot, bounded by the slice's own
        // (fixed) arity — extra RHS values are discarded, matching raku's
        // bound-slice semantics. See the mirror-image handling in
        // `vm_var_assign_local.rs`'s expression-context assignment.
        if !is_bind
            && !is_rebind
            && name.starts_with('@')
            && matches!(
                self.env()
                    .get(&Self::bound_array_slice_marker_key(name))
                    .map(Value::view),
                Some(ValueView::Bool(true))
            )
            && let ValueView::Array(items, ..) = self.locals[idx].view()
            && items.iter().any(Value::is_container_ref)
        {
            let cells: Vec<Value> = items.iter().cloned().collect();
            let rhs_vals = self.assignment_rhs_values(&raw_popped)?;
            for (i, cell_val) in cells.iter().enumerate() {
                if let ValueView::ContainerRef(cell) = cell_val.view() {
                    let v = rhs_vals
                        .get(i)
                        .cloned()
                        .unwrap_or_else(|| Value::package(Symbol::intern("Any")));
                    *cell.lock().unwrap() = v;
                }
            }
            return Ok(());
        }
        // Capture the old hash Arc before assignment for circular reference fixup.
        let old_hash_arc = if name.starts_with('%') {
            if let ValueView::Hash(arc) = self.locals[idx].view() {
                Some(crate::gc::Gc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        // Capture the old array Arc before assignment for circular reference fixup.
        let old_array_arc = if name.starts_with('@') {
            if let ValueView::Array(arc, _) = self.locals[idx].view() {
                Some(crate::gc::Gc::as_ptr(&arc) as usize)
            } else {
                None
            }
        } else {
            None
        };
        // Container identity (§3, BLOCKERS.md splice.t): a plain whole-array/hash
        // *assignment* (`@a = ...`, not `my @a = ...` and not `:=`) mutates the
        // EXISTING container in place, so any other holder of the same backing
        // `Gc` (e.g. `@a` captured by value into a list `(0, @a)` / a `\param`
        // capture) observes the update — matching Raku's stable container
        // identity. We snapshot the live backing `Gc` here and, after all the
        // value-shaping/metadata logic has produced the final container, copy
        // its contents back into this original `Gc` rather than swapping the
        // slot to a fresh pointer. Only for real reassignment: a `my` decl
        // creates a fresh container each time (a captured reference from a
        // prior iteration must NOT see the new declaration's value).
        // Anonymous containers (`my %`, `my @`) all compile to the single reused
        // slot name `%__ANON_HASH__` / `@__ANON_ARRAY__`, so each successive
        // declaration is a DISTINCT logical variable that merely shares a slot.
        // In-place reassignment there would alias two unrelated anonymous
        // containers (e.g. `(my % = ...), (my % = ...)` in a list), so exclude
        // anon names and let them take the fresh-container (replace) path.
        let is_anon_container = name.contains("__ANON");
        let inplace_old_array: Option<crate::gc::Gc<crate::value::ArrayData>> =
            if name.starts_with('@') && !is_bind && !is_rebind && !is_vardecl && !is_anon_container
            {
                if let ValueView::Array(gc, _) = self.locals[idx].view() {
                    Some(gc.clone())
                } else {
                    None
                }
            } else {
                None
            };
        let inplace_old_hash: Option<crate::gc::Gc<crate::value::HashData>> =
            if name.starts_with('%') && !is_bind && !is_rebind && !is_vardecl && !is_anon_container
            {
                if let ValueView::Hash(gc) = self.locals[idx].view() {
                    Some(gc.clone())
                } else {
                    None
                }
            } else {
                None
            };
        let mut val = if name.starts_with('%') {
            if has_explicit_initializer
                && !is_constant
                && !is_bind
                && raw_popped.is_nil()
                && let Some(constraint) = loan_env!(self, var_type_constraint(name))
            {
                return Err(runtime::utils::type_check_assignment_typed_error(
                    name,
                    &constraint,
                    &Value::NIL,
                ));
            }
            // Prevent re-initialization of immutable containers (Mix, Set, Bag)
            if !is_vardecl
                && !is_bind
                && matches!(
                    self.locals[idx].view(),
                    ValueView::Mix(_, false) | ValueView::Set(_, false) | ValueView::Bag(_, false)
                )
            {
                let type_name = match self.locals[idx].view() {
                    ValueView::Mix(..) => "Mix",
                    ValueView::Set(..) => "Set",
                    ValueView::Bag(..) => "Bag",
                    _ => unreachable!(),
                };
                return Err(RuntimeError::assignment_ro_typename(
                    type_name,
                    &self.locals[idx].to_string_value(),
                ));
            }
            if is_bind {
                // `:=` binding preserves containers — skip coercion. But the
                // bound container must conform to a typed-hash variable's
                // declared value type: `my Int %h := <untyped hash>` dies
                // because the RHS is `Hash[Any,Any]`, not `Associative[Int]`.
                self.check_hash_bind_value_type(name, &raw_popped)?;
                raw_popped
            } else if is_constant {
                // `constant %x` preserves Associative containers (Hash, Bag, Set, Mix, Pair).
                // Non-Associative values (Lists) are coerced to Map.
                // Non-Associative Instance objects get .Map called for coercion.
                self.coerce_constant_hash_value(name, raw_popped)?
            } else {
                self.coerce_hash_var_value(name, raw_popped)?
            }
        } else if name.starts_with('@') {
            if (has_explicit_initializer || !is_vardecl)
                && !is_constant
                && !is_bind
                && raw_popped.is_nil()
                && let Some(constraint) = loan_env!(self, var_type_constraint(name))
            {
                // A bare Nil assigned to a typed `@` array reverts to the element
                // type's default. A definite (`:D`) element type has no default
                // type object, so it dies (`my Int:D @a = Nil`); a non-definite
                // element type produces the element type object
                // (`my Bool @a = Nil` -> `Array[Bool].new(Bool)`). Rewrite the
                // bare Nil into a single-element `[Nil]` so the element coercion
                // below turns it into that type object.
                if self.is_definite_constraint(&constraint) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        &Value::NIL,
                    ));
                }
                raw_popped = Value::real_array(vec![Value::NIL]);
            }
            // Native typed arrays cannot store lazy sequences — check before
            // eager evaluation so the error is raised even if the sequence is
            // infinite.
            if let Some(constraint) = loan_env!(self, var_type_constraint(name))
                && crate::runtime::native_types::is_native_array_element_type(&constraint)
            {
                let is_lazy_value = match raw_popped.view() {
                    ValueView::Array(_, kind) => kind.is_lazy(),
                    ValueView::LazyList(_) => true,
                    ValueView::Seq(body) => body.is_io_lines_source(),
                    _ => false,
                };
                if is_lazy_value {
                    let declared = format!("array[{}]", constraint);
                    return Err(RuntimeError::typed(
                        "X::Cannot::Lazy",
                        [
                            (
                                "message".to_string(),
                                Value::str(format!("Cannot store a lazy list onto a {}", declared)),
                            ),
                            ("action".to_string(), Value::str_from("store")),
                        ]
                        .into_iter()
                        .collect(),
                    ));
                }
                // An infinite Range (`-Inf..0e0`, `0e0..Inf`) cannot initialize a
                // native typed array. Detect it on the raw value *before*
                // materialization — `value_to_list` would otherwise expand a
                // left-infinite range to a capped finite list, hiding the
                // laziness. Mirrors `coerce_typed_array_elements`' per-item
                // check (action `initialize`, `array[$t]`).
                if raw_popped.is_range()
                    && crate::builtins::methods_0arg::is_value_lazy(&raw_popped)
                {
                    return Err(RuntimeError::typed(
                        "X::Cannot::Lazy",
                        [
                            (
                                "message".to_string(),
                                Value::str(format!(
                                    "Cannot initialize an array of {} with a lazy list",
                                    constraint
                                )),
                            ),
                            ("action".to_string(), Value::str_from("initialize")),
                            (
                                "what".to_string(),
                                Value::str(format!("array[{constraint}]")),
                            ),
                        ]
                        .into_iter()
                        .collect(),
                    ));
                }
            }
            let mut assigned = if is_constant {
                // `constant @x` stores a List, not an Array.
                // Explicit Arrays ([1,2,3]) are preserved.
                // Instance objects that do Positional are kept as-is.
                // Non-Positional objects have .cache called for coercion.
                match raw_popped.view() {
                    ValueView::Array(items, kind) if kind.is_real_array() => {
                        Value::array_with_kind(items.clone(), kind)
                    }
                    ValueView::Array(items, _) => {
                        Value::array_with_kind(items.clone(), crate::value::ArrayKind::List)
                    }
                    ValueView::Seq(body) if body.is_io_lines_source() => {
                        let body = std::sync::Arc::clone(&body);
                        let items = self.reify_seq_body(&body)?;
                        Value::array_with_kind(
                            crate::gc::Gc::new(crate::value::ArrayData::new(items)),
                            crate::value::ArrayKind::List,
                        )
                    }
                    ValueView::Seq(items) => Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(items.to_vec())),
                        crate::value::ArrayKind::List,
                    ),
                    // `CoerceToList` runs first for every `constant @x` and has
                    // already applied the constant-@ list semantics, including the
                    // decision to keep an unreifiable lazy list lazy (Digest::SHA2's
                    // `constant @primes = grep *.is-prime, 2 .. *` is infinite).
                    // Re-wrapping it here made `@primes[^8]` read `([...] Nil Nil …)`.
                    ValueView::LazyList(_) => raw_popped.clone(),
                    ValueView::Instance { class_name, .. } => {
                        // Check if Instance does Positional — if so, keep as-is.
                        let cn = class_name.resolve();
                        let does_positional = matches!(
                            cn.as_str(),
                            "Array"
                                | "List"
                                | "Slip"
                                | "Seq"
                                | "Range"
                                | "Buf"
                                | "Blob"
                                | "utf8"
                                | "buf8"
                                | "buf16"
                                | "buf32"
                        ) || self
                            .class_composed_roles(&cn)
                            .is_some_and(|roles| roles.iter().any(|r| r == "Positional"));
                        if does_positional {
                            raw_popped.clone()
                        } else {
                            // Call .cache on non-Positional to coerce.
                            // Skip native methods so user-defined .cache is called.
                            let cached = self.call_method_all_with_fallback(
                                &raw_popped,
                                "cache",
                                &[],
                                true,
                            )?;
                            let cached_val = cached.into_iter().next().unwrap_or(Value::NIL);
                            // Check that .cache returned a Positional
                            let is_pos = matches!(
                                cached_val.view(),
                                ValueView::Array(..)
                                    | ValueView::Seq(_)
                                    | ValueView::Slip(_)
                                    | ValueView::LazyList(_)
                            );
                            if !is_pos {
                                let got_type = crate::runtime::utils::value_type_name(&cached_val);
                                let mut attrs = std::collections::HashMap::new();
                                attrs.insert("got".to_string(), cached_val);
                                attrs.insert(
                                    "expected".to_string(),
                                    Value::package(crate::symbol::Symbol::intern("Positional")),
                                );
                                attrs.insert(
                                    "message".to_string(),
                                    Value::str(format!(
                                        "Type check failed in assignment to {}; expected Positional but got {}",
                                        name, got_type
                                    )),
                                );
                                let ex = Value::make_instance(
                                    crate::symbol::Symbol::intern("X::TypeCheck"),
                                    attrs,
                                );
                                let mut err = RuntimeError::new(format!(
                                    "Type check failed in assignment to {}; expected Positional but got {}",
                                    name, got_type
                                ));
                                err.exception = Some(Box::new(ex));
                                return Err(err);
                            }
                            // Coerce cached result to List
                            match cached_val.view() {
                                ValueView::Array(items, _) => Value::array_with_kind(
                                    items.clone(),
                                    crate::value::ArrayKind::List,
                                ),
                                ValueView::Seq(items) => Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(
                                        items.to_vec(),
                                    )),
                                    crate::value::ArrayKind::List,
                                ),
                                _ => Value::array_with_kind(
                                    crate::gc::Gc::new(crate::value::ArrayData::new(vec![
                                        cached_val.clone(),
                                    ])),
                                    crate::value::ArrayKind::List,
                                ),
                            }
                        }
                    }
                    _ => Value::array_with_kind(
                        crate::gc::Gc::new(crate::value::ArrayData::new(vec![raw_popped.clone()])),
                        crate::value::ArrayKind::List,
                    ),
                }
            } else if is_bind {
                self.bind_positional_value(name, &raw_popped)?
            } else if let Some(items) = self.try_iterable_instance_items(&raw_popped)? {
                // A user class that `does Iterable` with its own `iterator`
                // method populates the array via that iterator (raku:
                // `my @a = $iterable` reifies `.iterator` into the array),
                // rather than storing the object as a single element.
                runtime::coerce_to_array(Value::real_array(items))
            } else {
                match raw_popped.view() {
                    ValueView::LazyList(list) => {
                        // An unreifiable lazy list (infinite spec / infinite
                        // pipe / `lazy`-marked) survives `@`-assign as a
                        // reify-on-demand LazyList tagged with array context
                        // so gist/`.WHAT` render `[...]`/`Array` (L2b step 6 —
                        // docs/lazy-arrays.md; mutations reify a prefix via
                        // `reify_lazy_array_slot`). A plain finite gather (or
                        // a pipe over one) is `.is-lazy` False and
                        // materializes eagerly.
                        if list.preserve_lazy_on_array_assign() {
                            Value::lazy_list(crate::gc::Gc::new(list.with_array_context()))
                        } else {
                            let forced = self.force_lazy_list_vm(&list)?;
                            Value::real_array(self.decay_nil_vec_elements(forced))
                        }
                    }
                    // An `IO::Handle.lines`/`.words` Seq (ADR-0034's
                    // `SeqSource::IoLines`, formerly the separate
                    // `LazyIoLines` value) was already reified in place by
                    // the `@`/`%`-target reify above (this whole match is
                    // reached only via that same `!is_bind && !is_rebind`
                    // path), so it needs no special case here — it falls to
                    // the generic `_` arm below like any other Seq.
                    // An infinite integer range (`1..*`) stays a reify LazyList
                    // instead of being capped to a 100k `ArrayKind::Lazy` Array. (L2)
                    _ if runtime::utils::infinite_int_range_to_lazy_array(&raw_popped)
                        .is_some() =>
                    {
                        runtime::utils::infinite_int_range_to_lazy_array(&raw_popped).unwrap()
                    }
                    _ => {
                        // Resolve bound-element sentinels before coercing to
                        // array.  Assignment (not binding) creates new
                        // containers, so bound refs must be snapshotted.
                        let other = self.resolve_bound_array_elements(raw_popped.clone());
                        runtime::coerce_to_array(other)
                    }
                }
            };
            // An `@` (list-)assignment resets Nil elements to the target
            // container's own default (ADR-0049 slice 3: `Interpreter::
            // assign_store_nil_default`, which is typed_container_default-
            // based and consults the declared element type/`is default(...)`
            // when the container itself is not yet metadata-tagged) --
            // `my @a = (1,2)[1,2]` is `[2, Any]`, `my Int @a = 1, Nil, 3` is
            // `Array[Int].new(1, Int, 3)`. Binds keep the source values
            // untouched.
            if !is_bind {
                assigned = self.decay_nil_elements_for_var_assign(name, assigned);
                // ADR-0040 slice 2: a real `Array`'s elements are `Scalar`
                // containers. `coerce_to_array` already itemizes on its own
                // tail, but the arms above bypass it (the reified-LazyList
                // gather, the iterable-instance reify, the infinite-range
                // LazyList); hooking here covers the whole `@`-assign entry
                // with one call. A `:=` bind is deliberately excluded -- a
                // bound `List`'s elements are NOT containers (§1.6/row 24).
                assigned = Self::itemize_elements_for_var_assign(name, assigned);
            }
            // `@a = Nil` resets to the *outgoing container's* own
            // `is default(...)` -- see `array_assign_nil_container_default`.
            if !is_bind && raw_popped.is_nil() {
                let old = self.locals[idx].clone();
                assigned = self.array_assign_nil_container_default(name, &old, assigned);
            }
            // Mark a genuine bound array SLICE (`@slice := @array[1,2]`, §4
            // BLOCKERS.md test 15): its OWN elements are shared `ContainerRef`
            // cells from the bind-time slice promotion (`vm_var_index_ops.rs`).
            // This marker is the ONLY safe trigger for the write-through
            // behavior in `vm_var_assign_local.rs`/this file's non-bind path —
            // "elements are cells" alone is NOT a safe signal, since unrelated
            // machinery (e.g. `.grep`'s rw-topic element promotion) can also
            // leave cell elements in a plain array that must NOT get the
            // fixed-arity write-through treatment.
            if is_bind
                && name.starts_with('@')
                && let ValueView::Array(items, ..) = assigned.view()
                && items.iter().any(Value::is_container_ref)
            {
                self.env_mut()
                    .insert(Self::bound_array_slice_marker_key(name), Value::TRUE);
            }
            // Preserve shaped array property on re-assignment, but only if the
            // new value is NOT already a shaped array (e.g. from Array.new(:shape(...)))
            let assigned_has_own_shape = crate::runtime::utils::shaped_array_shape(&assigned)
                .is_some()
                || matches!(
                    assigned.view(),
                    ValueView::Array(_, crate::value::ArrayKind::Shaped)
                );
            // Read the LHS's current value THROUGH a shared `ContainerRef` cell:
            // a `$scalar = @arr` share (`MarkArrayShareSource`) or an rw/`\(...)`
            // argument capture (`WrapVarRef`) replaces this slot with the cell,
            // and a bare `shaped_array_shape` on the cell answers `None` — which
            // silently turned the next whole assignment into an unshaped,
            // truncated array (`my @a[4]; peek(@a); @a = "x","y"` gave 2 elems).
            let lhs_shape = self.locals[idx].with_deref(crate::runtime::utils::shaped_array_shape);
            if let Some(shape) = &lhs_shape
                && shape.len() == 1
                && !assigned_has_own_shape
            {
                let lhs_current = self.locals[idx].deref_container();
                let items = runtime::value_to_list(&assigned);
                let item_count = items.len();
                let mut shaped_items: Vec<Value> = items.into_iter().take(shape[0]).collect();
                if item_count < shape[0] {
                    // Pad with the element type's default (native arrays: int->0,
                    // num->0e0, str->""), not Nil, so clearing a shaped num array
                    // yields `0 0 0 0` rather than empty slots.
                    let default = self.typed_container_default(&lhs_current);
                    Self::autoviv_resize(&mut shaped_items, shape[0], default)?;
                }
                assigned = Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(shaped_items)),
                    crate::value::ArrayKind::Shaped,
                );
                crate::runtime::utils::mark_shaped_array(&assigned, Some(shape));
                // Preserve container type metadata
                if let Some(info) = self.container_type_metadata(&lhs_current) {
                    assigned = self.tag_container_metadata(assigned, info);
                }
            } else if !is_bind
                && !is_shaped_decl
                && (has_explicit_initializer || !is_vardecl)
                && lhs_shape.is_none()
                && assigned_has_own_shape
                && let ValueView::Array(items, _) = assigned.view()
            {
                // The LHS is a plain (non-shaped) `@` array but the RHS is a
                // shaped array. Raku `=` copies element VALUES and drops
                // shaped-ness: `my @u = @shaped` yields a growable unshaped
                // Array (`.shape` is `(*)`), so a later `@u = ...` is not
                // truncated to the original fixed dimension. Binding (`:=`)
                // keeps the shaped value, so this is guarded on `!is_bind`.
                // A *shaped declaration* (`my @a[5]`) has no RHS initializer —
                // its shaped value comes from the `[5]` spec, not a source array —
                // so `has_explicit_initializer || !is_vardecl` keeps that shape.
                let items = if items.shape.is_some() {
                    let mut data = (**items).clone();
                    data.shape = None;
                    crate::gc::Gc::new(data)
                } else {
                    items.clone()
                };
                assigned = Value::array_with_kind(items, crate::value::ArrayKind::Array);
            }
            let class_name = match self.locals[idx].view() {
                ValueView::Instance { class_name, .. } => Some(class_name),
                ValueView::Package(class_name) => Some(class_name),
                _ => None,
            };
            // When the old local slot holds a Buf/Blob (e.g. from a
            // previous loop iteration or redeclaration), VarDecl (`my @a`)
            // must be allowed to overwrite it. Plain assignments (`@a = ...`)
            // also come through SetLocal; for those, Blob is immutable and
            // Buf coerces through Buf.new to preserve the container type.
            if let Some(class_name) = class_name
                && !is_vardecl
            {
                let class = class_name.resolve();
                if class == "Blob" || class.starts_with("blob") || class.starts_with("Blob[") {
                    return Err(RuntimeError::assignment_ro(None));
                }
                if class == "Buf" || class.starts_with("buf") || class.starts_with("Buf[") {
                    let items = runtime::value_to_list(&assigned)
                        .into_iter()
                        .map(|v| Value::int(runtime::to_int(&v)))
                        .collect::<Vec<_>>();
                    assigned = self.try_compiled_method_or_interpret(
                        Value::package(class_name),
                        "new",
                        items,
                    )?;
                }
            }
            assigned
        } else {
            let v = Self::normalize_scalar_assignment_value(raw_popped);
            // Only a plain `=` assignment installs a Scalar container; binds
            // (`:=`), rebinds, and `constant` install the value itself.
            if is_bind
                || scalar_bind
                || param_raw_bind
                || is_rebind
                || is_constant
                || Self::is_identity_scalar_restore(&self.locals[idx], &v)
            {
                v
            } else {
                Self::itemize_scalar_store(name, v)
            }
        };
        if val.is_nil()
            && !self.locals[idx].is_nil()
            && let Some(def) = self.var_default(name)
        {
            val = def.clone();
        }
        // For array variables with `is default(X)`, replace Nil elements
        // with the default value (Raku container semantics).
        if name.starts_with('@')
            && let Some(def) = self.var_default(name).cloned()
            && let ValueView::Array(items, kind) = val.view()
        {
            let is_hole =
                |v: &Value| v.is_nil() || matches!(v.view(), ValueView::Package(n) if n == "Any");
            let has_holes = items.iter().any(is_hole);
            if has_holes {
                let replaced: Vec<Value> = items
                    .iter()
                    .map(|v| if is_hole(v) { def.clone() } else { v.clone() })
                    .collect();
                val = Value::array_with_kind(
                    crate::gc::Gc::new(crate::value::ArrayData::new(replaced)),
                    kind,
                );
            }
        }
        // Skip typed container coercion for `:=` binding — it would create
        // a new Arc and lose container identity (e.g. Map metadata).
        //
        // Also skip it for a `constant` declaration: `@`/`%` constants are
        // always untyped (a typed one is rejected as X::ParametricConstant at
        // compile time), so `coerce_typed_container_assignment` can only ever
        // rebuild the hash into a plain `Value::hash(...)` — dropping the
        // `declared_type: "Map"` metadata that `coerce_constant_hash_value`
        // just embedded. A `constant %h = <a b>` must render as `Map.new(...)`,
        // and the coercion above already produced the final value.
        if !is_bind && !is_constant && (name.starts_with('@') || name.starts_with('%')) {
            val = self.coerce_typed_container_assignment(name, val, has_explicit_initializer)?;
            // An attribute twigil's element type lives in the class registry,
            // which the name-keyed coercion above cannot see (`has Str @!c;
            // @!c = @x` as a statement lands here).
            let name = name.to_string();
            val = self.apply_attr_container_element_type(&name, val)?;
        }
        // Raku `@array = ...` / `%hash = ...` assigns INTO the existing container,
        // so its `is default(...)` element default survives the reassignment
        // (`my @a is default('N/A'); @a = Nil; @a[4]` still reads 'N/A'). The
        // freshly-built container carries no default of its own, and the Nil /
        // hole rebuilds above discard any grafted onto an intermediate value, so
        // regraft here — after all coercion/rebuilding — from the old container
        // (or the variable's registered default).
        if !is_bind
            && !is_vardecl
            && !is_constant
            && (name.starts_with('@') || name.starts_with('%'))
            && self.container_default(&val).is_none()
            && let Some(def) = self
                .container_default(&self.locals[idx])
                .or_else(|| self.var_default(name).cloned())
        {
            val = self.tag_container_default(val, def);
        }
        // A scalar attribute write from inside a method (`$!x = v` / `$.x = v`)
        // is compiled as an ordinary name assignment, so its declared type has
        // to be pulled from the class registry rather than the name-keyed
        // `var_type_constraints` map. Resolving it HERE — the single pre-store
        // choke point — gives the attribute the same treatment as any other
        // typed scalar (type check, `:D`, coercion, native wrapping) before the
        // slot write and before the mirror into `self`'s cell. Skipped for a
        // Nil assignment, which resets the attribute to its own type object via
        // `reset_nil_untyped_scalar` in the `else` arm below.
        let constraint = loan_env!(self, var_type_constraint(name)).or_else(|| {
            (!is_bind && !val.is_nil())
                .then(|| self.scalar_attr_type_constraint(name))
                .flatten()
        });
        if let Some(constraint) = constraint
            && !name.starts_with('%')
            && !name.starts_with('@')
        {
            if val.is_nil() && self.is_definite_constraint(&constraint) {
                if has_explicit_initializer {
                    let nominal =
                        loan_env!(self, nominal_type_object_name_for_constraint(&constraint));
                    let reset_value = Value::package(Symbol::intern(&nominal));
                    return Err(runtime::utils::definite_type_check_assignment_error(
                        name,
                        &constraint,
                        &reset_value,
                    ));
                }
                // A subset (named or anon-from-`where`) whose base is `:D` does not
                // require an initializer — only an explicit `:D` smiley on the
                // declared type does. Skip when no initializer is required.
                if self.constraint_requires_initializer(&constraint) {
                    // The constraint reached here already stored (the declaration
                    // applied any `use variables` pragma), so `implicit` cannot be
                    // recovered at this site — the TypeCheck opcode path reports it.
                    return Err(RuntimeError::missing_initializer(
                        &constraint,
                        "variable",
                        None,
                    ));
                }
            }
            // A Nil ASSIGNED to a typed scalar resets it to its type object
            // (`my Str $x = "a"; $x = Nil` leaves `$x === Str`). The reset must
            // happen in the STORE: a routine lexical's constraint is env-scoped
            // (`SetVarTypeScoped`), invisible to the read paths' global-map
            // probe — which deliberately leaves a genuinely-Nil `= Nil`
            // parameter default as Nil — so the stored value itself must carry
            // the type object. Binds keep the Nil (`:=` stores the value as
            // is), and an `is default(...)` value takes over the Nil read.
            if val.is_nil() && !is_bind && constraint != "Nil" && self.var_default(name).is_none() {
                val = self.typed_scalar_nil_seed_value(name, &constraint);
            } else {
                // A `:=` bind stores a `ContainerRef` cell (e.g. `my Offset $o := @a[$i]`
                // aliases the array element). The type constraint applies to the
                // *contained* value, so deref the cell before matching -- otherwise the
                // check inspects the container itself and reports the bogus "got Any".
                // Only the (rare) bind path derefs; the common assignment path borrows
                // `val` directly with no clone.
                let bind_derefed = is_bind.then(|| val.deref_container());
                let check_val = bind_derefed.as_ref().unwrap_or(&val);
                if !check_val.is_nil() && !self.type_matches_value(&constraint, check_val) {
                    return Err(runtime::utils::type_check_assignment_typed_error(
                        name,
                        &constraint,
                        check_val,
                    ));
                }
                if !val.is_nil() {
                    val = loan_env!(self, try_coerce_value_for_constraint(&constraint, val))?;
                }
                // Wrap native integer values on assignment (overflow wrapping)
                val = Self::wrap_native_int_by_constraint(&constraint, val)?;
            }
        } else if !is_bind && (!is_vardecl || has_explicit_initializer) {
            // Untyped scalar: assigning Nil resets it to the default type
            // object Any (`$x = Nil` / `my $x = Nil` leave `$x === Any`,
            // S02-types/nil.t 21). Only a declaration WITHOUT an explicit
            // initializer keeps the synthesized-Nil path — that default is
            // load-bearing across the compiler's shadow-slot/closure-cell
            // machinery (PLAN 8.5 step 3). The reset guard is a no-op for
            // `@`/`%` containers and internal temps.
            val = self.reset_nil_untyped_scalar(name, val);
        }
        // Only enforce sigilless-readonly when this is NOT a new variable
        // declaration (my $x = ...).  A `my` decl creates a fresh variable
        // that shadows the sigilless one, so it must not be blocked. Skipped
        // outright when no `__mutsu_sigilless_*` key has ever been created, so a
        // program with no sigilless/`:=` binding pays no `format!` per store.
        if !is_vardecl
            && crate::env::closure_meta_keys_possible()
            && let Some(readonly_sym) = code.readonly_sym(idx)
            && let Some(alias_sym) = code.alias_sym(idx)
            && matches!(
                self.env().get_sym(readonly_sym).map(Value::view),
                Some(ValueView::Bool(true))
            )
            && !matches!(
                self.env().get_sym(alias_sym).map(Value::view),
                Some(ValueView::Str(_))
            )
        {
            return Err(RuntimeError::assignment_ro(None));
        }
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key(name));
        }
        // When rebinding a variable (`$x := expr`), remove any existing
        // bind pairs where this slot was the source.  Rebinding replaces
        // the container, so previously bound targets must stop tracking.
        if is_rebind {
            self.local_bind_pairs.retain(|&(source, _)| source != idx);
            // Also remove env-based aliases that point TO this variable,
            // so GetLocal alias-following doesn't read the new value.
            let mut aliases_to_remove = Vec::new();
            let prefix = "__mutsu_sigilless_alias::";
            for (k, v) in self.env().iter() {
                if let Some(_var_name) = k.strip_prefix_str(prefix)
                    && let ValueView::Str(target) = v.view()
                    && target.as_str() == name
                {
                    aliases_to_remove.push(*k);
                }
            }
            for k in aliases_to_remove {
                self.env_mut().remove_sym(k);
            }
        }
        if let Some(source_name) = bind_source {
            let alias_key = runtime::sigilless_alias_key(name);
            let resolved_source = self.resolve_sigilless_alias_source_name(&source_name);
            // The write path walks `__mutsu_sigilless_alias::<name>` hop by hop, so
            // recording the *immediate* source keeps every intermediate name on the
            // path. Pre-resolving straight to the chain's root skips them, and an
            // `is rw` parameter is itself an alias of the caller's variable: in
            // `sub f($x is rw) { my $c := $x; $c = 3 }` the source `x` resolved to
            // the caller's `v`, a name with no slot in this frame, so neither `$x`
            // nor the caller was ever updated. Redirect only when the immediate
            // source really is a local here; otherwise keep the resolved root, as
            // before (the walk reaches it either way).
            let alias_target = if source_name != resolved_source
                && code.locals.iter().any(|n| n == &source_name)
            {
                source_name.clone()
            } else {
                resolved_source.clone()
            };
            self.env_mut()
                .insert(alias_key.clone(), Value::str(alias_target));
            // Binding aliases the source, so the target inherits its readonly-ness:
            // `sub f($ro) { my $c := $ro; $c = 3 }` is an assignment to a readonly
            // variable, exactly as `$ro = 3` would be. Writing an unconditional
            // `False` here made the alias writable and silently dropped the store.
            // (The SetGlobal bind path already propagates this.)
            let source_kind = self.readonly_kind(&resolved_source);
            self.env_mut().insert(
                runtime::sigilless_readonly_key(name),
                Value::truth(source_kind.is_some()),
            );
            if let Some(kind) = source_kind {
                self.mark_readonly_with(name, kind);
            }
            self.mark_sigilless_alias_seen();
            // Create a shared ContainerRef for cross-scope binding (source in
            // outer call frame) OR same-scope rebinding (`:=` on existing vars).
            // ContainerRef ensures bidirectional container sharing: writing to
            // either variable updates both, matching Raku's binding semantics.
            // The topic (`_`) and the arg pseudo-vars are a fresh per-call binding
            // on EVERY routine frame (see the `is_routine` reset in
            // `vm_call_named_inner.rs`), not a genuine lexical any one ancestor
            // frame "owns" — so `contains_key` is trivially true for almost every
            // frame on the stack. Treating that as "the source lives in an outer
            // frame" made a plain `my $ex := $_;` (as `Test.rakumod`'s `throws-like`
            // does inside its `CATCH { default { ... } }`) promote `_` itself to a
            // shared `ContainerRef` cell and splice that cell into EVERY ancestor
            // frame's own saved env below — permanently corrupting an unrelated
            // caller's topic with the bound value. A `:=` bind only needs to
            // capture `_`'s CURRENT referent in the target name; `_` itself must
            // never be promoted or written back into any ancestor frame.
            let is_percall_pseudo_var = matches!(resolved_source.as_str(), "_" | "@_" | "%_" | "!");
            let source_in_outer_frame = !is_percall_pseudo_var
                && self
                    .call_frames
                    .iter()
                    .any(|f| f.saved_env.contains_key(&resolved_source));
            let source_in_same_scope = code.locals.iter().any(|n| n == &resolved_source);
            // `my @a := @$n` deref-bind (Slice 2c): the parser conflates `@$n`
            // (deref of a scalar `$n` that holds an array by reference) with the
            // array variable `@n`. When no `@n`/`%n` container value exists at
            // runtime but a same-named scalar holds a shared `ContainerRef` cell
            // (a value-alias `my $n = @z` — Slice 2a), bind to THAT cell so `@a`
            // shares the very container `$n`/`@z` references, not a fresh copy.
            // Gate on the runtime value of `@n` (not its presence in the
            // function-wide `code.locals`, which a `my @n` in a *sibling* block
            // would spuriously satisfy via `source_in_same_scope`).
            let source_resolves_to_container = matches!(
                self.env().get(&resolved_source).map(Value::view),
                Some(ValueView::Array(..) | ValueView::Hash(..) | ValueView::ContainerRef(_))
            );
            let scalar_source: Option<String> = if source_resolves_to_container {
                None
            } else {
                resolved_source
                    .strip_prefix(['@', '%'])
                    .filter(|bare| {
                        matches!(
                            self.env().get(bare).map(Value::view),
                            Some(ValueView::ContainerRef(_))
                        )
                    })
                    .map(str::to_string)
            };
            let effective_source = scalar_source
                .clone()
                .unwrap_or_else(|| resolved_source.clone());
            // Frame-ownership gate for the ancestor-frame splice below. MUST be
            // read here, before either bind branch writes the shared container
            // into the env under the source's name — that write would make the
            // own-tier half of the test trivially true. See
            // `bind_source_is_own_frame_lexical`.
            let resolved_source_is_own_lexical = self.bind_source_is_own_frame_lexical(
                code,
                &source_name,
                &resolved_source,
                bind_source_slot,
            );
            let effective_source_is_own_lexical = if effective_source == resolved_source {
                resolved_source_is_own_lexical
            } else {
                self.bind_source_is_own_frame_lexical(
                    code,
                    &source_name,
                    &effective_source,
                    bind_source_slot,
                )
            };
            // For a deref-bind via a promoted scalar (`my @a := @$n` / `my %h :=
            // %$m`, Slice 2c), the sigilless alias set above points at the
            // sigil'd source (`@n`/`%m`) which has no container value — only the
            // bare scalar (`n`/`m`) holds the shared cell. Redirect the alias to
            // that effective source so a later element write (`@a[0]=`, `%h<k>=`,
            // which resolves the target *through* the alias) reaches the shared
            // cell rather than autovivifying a fresh, detached container.
            if scalar_source.is_some() {
                self.env_mut()
                    .insert(alias_key.clone(), Value::str(effective_source.clone()));
                self.mark_sigilless_alias_seen();
            }
            // Whole-container `:=` bind (`my @b := @a`, `my %h2 := %h`,
            // `my $ref := @a`): share a single `ContainerRef` cell between the
            // source and target so mutations through either alias (push,
            // element assignment) are observed by both, as in Raku. Without
            // this the two slots only share the inner `Arc`, and a COW mutation
            // (`.push`) detaches them. (Mirrors the element-bind cell pattern in
            // the index-assign path; the cell IS the alias so no
            // `local_bind_pairs` entry is recorded.)
            let val_is_container = matches!(
                val.view(),
                ValueView::Array(..) | ValueView::Hash(..) | ValueView::ContainerRef(_)
            );
            if val_is_container
                && (source_in_same_scope || source_in_outer_frame || scalar_source.is_some())
                && !name.starts_with('&')
            {
                // Reuse the source's existing cell if it already has one (so a
                // third alias `my @c := @b` joins the same cell); otherwise wrap
                // the bound value in a fresh cell. ADR-0024: also check the
                // mainline capture store — an intervening frame's own
                // identically-named local can shadow the source in the plain
                // `env` lookup right above (see `mainline_lexical_cell`).
                let cell = match val.view() {
                    ValueView::ContainerRef(arc) => arc.clone(),
                    _ => match self.env().get(&effective_source).map(Value::view) {
                        Some(ValueView::ContainerRef(arc)) => arc.clone(),
                        _ => self
                            .mainline_lexical_cell(&effective_source)
                            .unwrap_or_else(|| {
                                crate::gc::Gc::new(crate::value::ContainerCell::new(val.clone()))
                            }),
                    },
                };
                // A bound `@`/`%` variable adopts the *source* container's
                // declared element/key type, not its own (`my Int %a; my Cool
                // %b := %a` ⇒ `%b.of` is `Int`). Propagate the inner container's
                // embedded type metadata to this variable's constraint (same as
                // the typed-bind propagation at the end of the slow path, which
                // the early return below skips).
                if name.starts_with('@') || name.starts_with('%') {
                    let inner = cell.lock().unwrap().clone();
                    if let Some(info) = self.container_type_metadata(&inner)
                        && !info.value_type.is_empty()
                    {
                        let constraint_str = if name.starts_with('%') {
                            if let Some(ref kt) = info.key_type {
                                format!("{}{{{}}}", info.value_type, kt)
                            } else {
                                info.value_type.clone()
                            }
                        } else {
                            info.value_type.clone()
                        };
                        self.vm_set_var_type_constraint(name, Some(constraint_str));
                    } else {
                        // Untyped source: clear any declared constraint inherited
                        // from this variable's own declaration so it does not
                        // over-constrain the shared container.
                        self.vm_set_var_type_constraint(name, None);
                    }
                }
                let container = Value::container_ref(cell);
                self.locals[idx] = container.clone();
                if let Some(source_idx) = code.locals.iter().rposition(|n| n == &effective_source) {
                    self.locals[source_idx] = container.clone();
                    self.flush_local_to_env(code, source_idx);
                }
                self.set_env_with_main_alias(&effective_source, container.clone());
                // Propagate the shared cell into saved call frames so the
                // binding survives method returns (env restore) without
                // reverting to a stale value (same as the scalar path below).
                // See `propagate_bind_to_ancestor_frames`'s doc comment for
                // what actually carries the binding across the call chain.
                self.propagate_bind_to_ancestor_frames(
                    &effective_source,
                    effective_source_is_own_lexical,
                    &container,
                );
                self.set_env_with_main_alias(name, container.clone());
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
            // Record local-slot binding pair for write propagation.
            // Only record if the source is also a local in the same code unit,
            // so cross-scope name collisions do not cause spurious propagation.
            if is_vardecl
                && let Some(source_idx) = code.locals.iter().position(|n| n == &resolved_source)
                && source_idx != idx
            {
                self.local_bind_pairs.push((source_idx, idx));
            }
            // Only use ContainerRef for same-scope rebind when the value is a
            // simple scalar (not a type object, array, hash, etc.)
            let val_is_simple_scalar = !matches!(
                val.view(),
                ValueView::Package(_)
                    | ValueView::Array(..)
                    | ValueView::Hash(..)
                    | ValueView::Sub(..)
                    | ValueView::Instance { .. }
            );
            if (source_in_outer_frame
                || (is_rebind && source_in_same_scope && val_is_simple_scalar))
                && !name.starts_with('@')
                && !name.starts_with('%')
                && !name.starts_with('&')
            {
                // Reuse the source's existing cell when it already has one, so a
                // further bind joins the same cell instead of minting a rival.
                // Reading the source derefs its cell, so `val` is the plain value
                // and the `ContainerRef` arm below cannot catch this: without the
                // lookup, `my $c := $x; my $d := $c` gave `$d` a fresh cell and cut
                // it off from `$x`'s, so a write through `$d` never reached `$x`
                // (nor, for an rw param, the caller). Mirrors the whole-container
                // bind path, which already reuses the source cell.
                let source_cell = code
                    .locals
                    .iter()
                    .rposition(|n| n == &source_name)
                    .and_then(|s| match self.locals[s].view() {
                        ValueView::ContainerRef(arc) => Some(arc.clone()),
                        _ => None,
                    })
                    .or_else(|| match self.env().get(&resolved_source).map(Value::view) {
                        Some(ValueView::ContainerRef(arc)) => Some(arc.clone()),
                        _ => None,
                    })
                    // ADR-0024: an intervening frame's own identically-named
                    // local (e.g. a raw-captured constructor parameter that
                    // happens to share a mainline lexical's bare name) can
                    // shadow the source in both lookups above; fall back to
                    // the mainline capture store before minting a
                    // disconnected cell.
                    .or_else(|| self.mainline_lexical_cell(&resolved_source));
                let container = match (val.view(), source_cell) {
                    (ValueView::ContainerRef(arc), _) => Value::container_ref(arc.clone()),
                    (_, Some(arc)) => Value::container_ref(arc),
                    _ => val.clone().into_container_ref(),
                };
                self.locals[idx] = container.clone();
                // Update source in locals if present
                if let Some(source_idx) = code.locals.iter().rposition(|n| n == &resolved_source) {
                    self.locals[source_idx] = container.clone();
                    self.flush_local_to_env(code, source_idx);
                }
                // Update source in env
                self.env_mut()
                    .insert(resolved_source.clone(), container.clone());
                // Propagate ContainerRef to all saved call frame envs so the
                // binding survives method returns (env restore) and a later
                // restore doesn't overwrite with stale values. See
                // `propagate_bind_to_ancestor_frames`'s doc comment for what
                // actually carries the binding across the call chain.
                self.propagate_bind_to_ancestor_frames(
                    &resolved_source,
                    resolved_source_is_own_lexical,
                    &container,
                );
                // Propagate ContainerRef to aliased attribute locals (e.g., when
                // binding sigilless `$x`, also update `!x` so attribute writeback picks it up).
                let alias_key_for_target = format!("__mutsu_sigilless_alias::{}", name);
                if let Some(alias_target) =
                    self.env()
                        .get(&alias_key_for_target)
                        .and_then(|v| match v.view() {
                            ValueView::Str(s) => Some(s.clone()),
                            _ => None,
                        })
                    && let Some(alias_idx) =
                        code.locals.iter().rposition(|n| n == alias_target.as_str())
                {
                    self.locals[alias_idx] = container.clone();
                    self.flush_local_to_env(code, alias_idx);
                }
                self.set_env_with_main_alias(name, container.clone());
                // For `our` variables, persist ContainerRef in our_vars so that
                // subsequent method calls (e.g., get_x) see the binding.
                for (slot, qualified_name) in &code.our_locals {
                    if *slot == idx {
                        self.set_our_var(qualified_name.clone(), container.clone());
                        self.set_our_var(name.to_string(), container.clone());
                    }
                }
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
        }
        // Lazy sync: if the local is not a ContainerRef but env has one
        // (from a cross-scope `:=` binding), adopt the ContainerRef so the
        // write-through below preserves shared container identity.
        // Skip for type objects and complex values.
        if !is_bind
            && !is_rebind
            && !self.locals[idx].is_container_ref()
            && !matches!(
                self.locals[idx].view(),
                ValueView::Package(_)
                    | ValueView::Array(..)
                    | ValueView::Hash(..)
                    | ValueView::Sub(..)
                    | ValueView::Instance { .. }
            )
            && let Some(arc) = self.env().get(name).and_then(|v| match v.view() {
                ValueView::ContainerRef(arc) => Some(arc.clone()),
                _ => None,
            })
        {
            self.locals[idx] = Value::container_ref(arc);
        }
        // Write through ContainerRef in slow path: update inner value
        if !is_bind
            && !is_rebind
            && let ValueView::ContainerRef(arc) = self.locals[idx].view()
        {
            // Slice 2a: a `=`-array-shared scalar reassigned as a whole REPLACES
            // the slot (raku value semantics); drop the share and fall through to
            // the plain-replace path below instead of writing through the cell.
            let scalar = !name.starts_with('@') && !name.starts_with('%');
            if scalar && self.array_share_active && self.is_array_share_scalar(name) {
                self.clear_array_share_marker(name);
            } else {
                let arc = arc.clone();
                if scalar {
                    val = Self::itemize_scalar_store(
                        name,
                        Self::normalize_scalar_assignment_value(val),
                    );
                    self.check_container_cell_constraint(&arc, &val)?;
                    Value::store_through_cell(&arc, &val);
                } else {
                    // Container identity (§3.1): re-apply the element/key-type
                    // metadata so a typed native array or typed/object hash
                    // written through its shared cell (`my @b := @a; @a = ...`,
                    // a loop-var alias, or a scalar-container-share cell) keeps
                    // its identity. The metadata-tagging block further below is
                    // bypassed by this early return, so it must be applied here.
                    let name = name.clone();
                    let old = arc.lock().unwrap().clone();
                    val = if name.starts_with('%') {
                        self.hash_container_writethrough_value(&name, val, &old)?
                    } else {
                        self.array_container_writethrough_value(&name, val, &old)?
                    };
                    self.check_container_cell_constraint(&arc, &val)?;
                    // Write back into the EXISTING backing `Gc` (instead of
                    // swapping the cell to a fresh pointer) so any other
                    // holder of the old container (e.g. the outer `%ao` a
                    // loop-var `%a` is aliased to) observes this mutation.
                    Self::cell_store_preserving_container_identity(&name, &arc, &val);
                }
                self.flush_local_to_env(code, idx);
                return Ok(());
            }
        }
        // If the current value is a Proxy, invoke STORE instead of overwriting
        if let ValueView::Proxy { storer, .. } = self.locals[idx].view()
            && !storer.is_nil()
        {
            let proxy_val = self.locals[idx].clone();
            loan_env!(self, assign_proxy_lvalue(proxy_val, val))?;
            // A Proxy STORE wrote the referent caller lexical by name; drain the
            // precise retain-on-miss writeback it recorded instead of the blanket
            // env→locals pull.
            self.apply_pending_rw_writeback(code);
            return Ok(());
        }
        // First write through a missing-key `:=` bind: materialize the path into
        // a shared `ContainerRef` cell (phantom-entry; see
        // `materialize_bound_slot_to_cell`).
        if !is_bind
            && !is_rebind
            && matches!(self.locals[idx].view(), ValueView::HashEntryRef { .. })
        {
            self.materialize_bound_slot_to_cell(code, idx, val);
            return Ok(());
        }
        // When binding a Proxy to a variable, update FETCH/STORE closures' captured envs
        // so they can reference the Proxy by its binding variable name (simulating capture-by-ref).
        let val = Self::update_proxy_closure_envs(val, name);
        if self.fatal_mode
            && !name.contains("__mutsu_")
            && let Some(err) = self.failure_to_runtime_error_if_unhandled(&val)
        {
            return Err(err);
        }
        self.locals[idx] = val.clone();
        // When assigning (not binding) to an untyped hash/array variable, drop
        // container type metadata still attached to the assigned value. The
        // value may share its backing `Arc` with a typed source container
        // (`my @a = @typed`), and an untyped declaration must not present its
        // value as typed. Skip for attribute variables (.h, !h) which get
        // typed metadata from the class definition, not var_type_constraints.
        //
        // Also skip for a `constant %h` / `constant @a`: its `declared_type`
        // ("Map" for a `%`-sigil list-coerced constant, per
        // `coerce_constant_hash_value`) is the deliberate final type of the
        // value, not stale element/key metadata inherited from a typed source.
        // Clearing it would demote `constant %h = <a b>` back to a plain Hash.
        if !is_bind
            && !is_constant
            && (name.starts_with('%') || name.starts_with('@'))
            && !name.contains('.')
            && !name.contains('!')
            && loan_env!(self, var_type_constraint(name)).is_none()
            && self.container_type_metadata(&val).is_some()
        {
            // Clear the embedded container type metadata in place so an
            // untyped variable never reports a typed element/key constraint.
            let cleared = crate::runtime::Interpreter::clear_hash_type_metadata(std::mem::replace(
                &mut self.locals[idx],
                Value::NIL,
            ));
            self.locals[idx] = cleared;
        }
        // When binding a typed hash/array to a variable, propagate the container's
        // type constraints to the variable so that subsequent element assignments
        // are type-checked (e.g. `my %h := Hash[Int].new; %h<a> = "b"` should die).
        // QuantHash values (Set/Bag/Mix) are excluded: their element typing is
        // enforced through the container's own metadata and the ctor's key
        // coercion, not an `Associative[ValueType]` variable constraint. Recording
        // a Set's element type as the variable's constraint would wrongly make an
        // untyped `my %qh` typed and block a later rebind (`%qh := Set[Int].new`).
        if is_bind
            && (name.starts_with('%') || name.starts_with('@'))
            && !matches!(
                val.view(),
                ValueView::Set(..) | ValueView::Bag(..) | ValueView::Mix(..)
            )
            && let Some(info) = self.container_type_metadata(&val)
            && !info.value_type.is_empty()
        {
            // Build the constraint string that set_var_type_constraint expects.
            // For hash variables, parse_container_constraint expects "ValueType{KeyType}"
            // format for key-typed hashes, and plain "ValueType" otherwise.
            let constraint_str = if name.starts_with('%') {
                if let Some(ref kt) = info.key_type {
                    format!("{}{{{}}}", info.value_type, kt)
                } else {
                    info.value_type.clone()
                }
            } else {
                info.value_type.clone()
            };
            self.vm_set_var_type_constraint(name, Some(constraint_str));
        }
        // Circular hash reference fixup: when assigning to a hash variable,
        // if any values in the new hash reference the old hash (captured on the
        // RHS before assignment), replace them with the new hash's Arc to create
        // a true circular reference (matching Raku container semantics).
        if name.starts_with('%') && !is_bind && !is_constant {
            Self::fixup_circular_hash_refs(&mut self.locals[idx], &old_hash_arc);
        }
        // Circular array reference fixup: when assigning to an array variable,
        // if any elements in the new array reference the old array (captured on the
        // RHS before assignment), replace them with the new array's Arc to create
        // a true circular reference (matching Raku container semantics).
        if name.starts_with('@') && !is_bind && !is_constant {
            Self::fixup_circular_array_refs(&mut self.locals[idx], &old_array_arc);
        }
        // Apply the variable's declared element/key type to the stored
        // container. For hashes this embeds the metadata in `HashData` so it
        // travels with the value through later copy-on-write; for arrays it
        // updates the Arc-pointer side table. Done before the value is cloned
        // and propagated to env/aliases below, so every copy carries it.
        if (name.starts_with('@') || name.starts_with('%'))
            && let Some(value_type) = loan_env!(self, var_type_constraint(name))
        {
            let info = crate::runtime::ContainerTypeInfo {
                declared_type: if name.starts_with('@')
                    && crate::runtime::native_types::is_native_array_element_type(&value_type)
                {
                    Some(format!("array[{value_type}]"))
                } else {
                    None
                },
                value_type,
                key_type: if name.starts_with('%') {
                    loan_env!(self, var_hash_key_constraint(name))
                } else {
                    None
                },
            };
            let stored = std::mem::replace(&mut self.locals[idx], Value::NIL);
            self.locals[idx] = self.tag_container_metadata(stored, info);
        }
        // Container identity (§3, splice.t): copy the final container's contents
        // back into the ORIGINAL backing `Gc` so aliases (a by-value `@a` capture
        // in a list / a `\param`) observe this reassignment. The slot already
        // holds the fully-shaped, metadata-tagged value; we reuse that shape but
        // keep the original pointer. Skip when the pointer is already the same
        // (e.g. a self-referential `@a = @a`), which needs no copy.
        if let Some(old_gc) = &inplace_old_array
            && let ValueView::Array(new_gc, kind) = self.locals[idx].view()
            && !crate::gc::Gc::ptr_eq(old_gc, &new_gc)
        {
            let (new_gc, kind) = (new_gc.clone(), kind);
            self.locals[idx] = Self::array_inplace_reassign(old_gc, &new_gc, kind);
        } else if let Some(old_gc) = &inplace_old_hash
            && let ValueView::Hash(new_gc) = self.locals[idx].view()
            && !crate::gc::Gc::ptr_eq(old_gc, &new_gc)
        {
            let new_gc = new_gc.clone();
            self.locals[idx] = Self::hash_inplace_reassign(old_gc, &new_gc);
        } else if inplace_old_array.is_none()
            && inplace_old_hash.is_none()
            && !is_bind
            && (name.starts_with('@') || name.starts_with('%'))
        {
            // No reusable same-typed container in the slot (a fresh `my @b = @a`
            // declaration, or a slot that did not hold an array/hash): the `@`/`%`
            // var must own a DISTINCT container per Raku `=` copy semantics, so
            // detach from any shared backing `Gc`. (A reassignment whose slot
            // already holds an array/hash goes through the in-place branches above
            // and keeps its identity.)
            let cur = std::mem::replace(&mut self.locals[idx], Value::NIL);
            self.locals[idx] = Self::detach_shared_container(cur);
        }
        // A typed scalar declaration owns a scalar container even when nothing
        // else (closure capture, `:=`, `.VAR`) would have forced boxing.  Its
        // `of` belongs to that cell, so restoring a shadowed outer value also
        // restores its constraint without consulting either name-keyed lane.
        if is_vardecl
            && !is_bind
            && !name.starts_with('@')
            && !name.starts_with('%')
            && !name.starts_with('&')
            && !self.legacy_atomic_lane_owns(name.trim_start_matches('$'))
            && !self.locals[idx].is_container_ref()
            && !matches!(
                self.locals[idx].view(),
                ValueView::Array(..)
                    | ValueView::BufStorage(..)
                    | ValueView::Hash(..)
                    | ValueView::Set(..)
                    | ValueView::Bag(..)
                    | ValueView::Mix(..)
            )
            && let Some(constraint) = loan_env!(self, var_type_constraint(name))
            // Aggregate/native scalar values have specialized lvalue paths
            // which do not yet accept a scalar cell wrapper. Their own
            // container metadata already carries the relevant constraint.
            && !matches!(
                constraint.as_str(),
                "atomicint"
                    | "Array"
                    | "Hash"
                    | "List"
                    | "Buf"
                    | "Blob"
                    | "SetHash"
                    | "BagHash"
                    | "MixHash"
            )
        {
            let cell =
                crate::gc::Gc::new(crate::value::ContainerCell::new(self.locals[idx].clone()));
            crate::value::register_container_constraint(&cell, &constraint);
            self.locals[idx] = Value::container_ref(cell);
        }
        // Use the potentially fixed-up value for env/shared_vars.
        let val = self.locals[idx].clone();
        // Mark variable as readonly when storing a LazyThunk. Tag-probed first:
        // a `view()` would materialize a lazy Match (see `exec_get_local_op`).
        if val.is_lazy_thunk_value() && matches!(val.view(), ValueView::LazyThunk(..)) {
            self.mark_readonly(name);
        }
        // `(B)` per-store env-write: a slot-authoritative plain lexical skips its
        // env mirror — the slot is the single source of truth, so env-COW drops
        // the write (docs/lexical-scope-slot-campaign.md). Excludes binds/constants
        // (env-shape carriers), captured/reflective frames (read the caller's env
        // by name), and names still in `needs_env_sync` (a mechanism consumer). The
        // term-symbol block and `:=` alias chain below stay unconditional.
        let skip_env_write = !is_bind
            && !is_constant
            && !code.needs_env_sync.get(idx).copied().unwrap_or(true)
            && !crate::opcode::reflective_name_access_possible()
            && Self::term_symbol_from_name(name).is_none();
        if (is_bind || is_constant) && name.starts_with('@') {
            // For `:=` bind and `constant @x`, bypass set_shared_var's
            // List->Array normalization so the container type is preserved.
            self.env_mut().insert(name.to_string(), val.clone());
        } else if code.plain_locals.get(idx).copied().unwrap_or(false) {
            // Plain lexical (no sigil/twigil/qualifier — the overwhelmingly
            // common SetLocal): every alias branch of the full mirror is
            // unreachable, so take the one-Symbol-insert writer and skip the
            // per-store `Symbol::intern` + `Main::` probe (J4d; profiled at
            // ~12% of a hot Num loop). Same writer flush_local_to_env uses.
            if !skip_env_write {
                let sym = code.locals_sym.get(idx).copied();
                self.set_env_plain_lexical(name, sym, val.clone());
            }
        } else {
            self.set_env_with_main_alias(name, val.clone());
        }
        if let Some(symbol) = Self::term_symbol_from_name(name) {
            self.env_mut().insert(symbol.to_string(), val.clone());
            let pkg = self.current_package().to_string();
            if pkg != "GLOBAL" {
                self.env_mut()
                    .insert(format!("{pkg}::term:<{symbol}>"), val.clone());
            }
        }
        // Follow the `:=` alias chain from this variable, if it has one. No
        // `__mutsu_sigilless_*` key can exist unless the program created a
        // sigilless/`:=` binding, so the whole walk (and its `format!`) is skipped
        // otherwise.
        let mut alias_name = if crate::env::closure_meta_keys_possible() {
            code.alias_sym(idx)
                .and_then(|sym| self.env().get_sym(sym))
                .and_then(|v| {
                    if let ValueView::Str(name) = v.view() {
                        Some(name.to_string())
                    } else {
                        None
                    }
                })
        } else {
            None
        };
        let mut seen_aliases = std::collections::HashSet::new();
        // Slots the forward walk writes. A `:=` records each alias's target as the
        // *root* of the chain (`my $y := $x; my $z := $y` stores `alias::z == "x"`),
        // so the walk from `z` reaches `x` but never the sibling alias `y`. Feeding
        // these slots into the reverse propagation below closes the bind group.
        let mut aliased_slots: Vec<usize> = Vec::new();
        while let Some(current_alias) = alias_name {
            if !seen_aliases.insert(current_alias.clone()) {
                break;
            }
            if let Some(slot) = self.find_local_slot(code, &current_alias) {
                aliased_slots.push(slot);
            }
            self.check_sigilless_alias_target_constraint(&current_alias, &val)?;
            self.update_local_if_exists(code, &current_alias, &val);
            self.env_mut().insert(current_alias.clone(), val.clone());
            // Sigilless attribute write: mirror an attr-twigil alias (`!x`) into
            // self's shared cell (no-op for non-attribute aliases). Stage 2c (ii).
            self.write_self_attr_cell(&current_alias, val.clone());
            // Slice F: a sigilless param (`\target`) aliases a caller variable;
            // record it so the call-site drain writes the env value through to the
            // caller's local slot (without relying on the reverse pull).
            self.pending_rw_writeback_sources
                .push(current_alias.clone());
            let next_key = format!("__mutsu_sigilless_alias::{}", current_alias);
            alias_name = self.env().get(&next_key).and_then(|v| {
                if let ValueView::Str(name) = v.view() {
                    Some(name.to_string())
                } else {
                    None
                }
            });
        }
        // Reverse propagation: when writing to a variable that is the source
        // of another variable's `:=` binding, update the bound variable's
        // local slot too.  For example, `my $y := $x; my $x = 3;` must
        // update $y's slot so it reads 3.
        // Uses local_bind_pairs recorded at binding time to avoid
        // cross-scope name collisions.
        //
        // The pairs point root -> alias, so writing the root reaches every alias
        // directly. Writing an *alias* only reaches the root (through the forward
        // chain walk above); the root's other aliases are its siblings, not its
        // targets, and would keep a stale value (`my $y := $x; my $z := $y; $z = 9`
        // left `$y` at its old value). Propagating from the slots the walk wrote —
        // i.e. from the root as well as from `idx` — closes the group, so every name
        // bound together observes the write regardless of which one is assigned.
        // The pairs are per-frame (saved/restored across calls), but a frame torn
        // down by an exception can leave a pair whose slot index does not address
        // this frame's `locals`. Skip those rather than index out of bounds.
        for &(source, target) in &self.local_bind_pairs {
            if target < self.locals.len() && (source == idx || aliased_slots.contains(&source)) {
                self.locals[target] = val.clone();
            }
        }
        if let Some(attr) = name.strip_prefix('.') {
            self.env_mut().insert(format!("!{}", attr), val.clone());
        } else if let Some(attr) = name.strip_prefix('!') {
            self.env_mut().insert(format!(".{}", attr), val.clone());
        }
        if name == "_"
            && !Self::is_topic_ro_assignment(&val)
            && let Some(ref source_var) = self.topic_source_var
            && !source_var.starts_with('@')
            && !source_var.starts_with('%')
        {
            let sv = source_var.clone();
            self.set_env_with_main_alias(&sv, val.clone());
            self.update_local_if_exists(code, &sv, &val);
            // An attribute topic (`with $!result { .PQclear; $_ = Nil }` —
            // DBDish::Pg's StatementHandle.finish) must reach self's attribute
            // cell, not just the env mirror: the stale cell otherwise keeps the
            // freed C pointer and the next finish double-frees it.
            if Self::attr_twigil_base(&sv).is_some() && !Self::is_non_mirrorable_attr_value(&val) {
                self.write_self_attr_cell(&sv, val.clone());
            }
        }
        // ADR-0022 Slice 5: a companion env marker recording that `name` is a
        // `constant` — the runtime-visible twin of the compiler's
        // `constant_vars_in_scope`, needed because `interpolate_regex_scalars`
        // (regex `$name` interpolation) runs long after compilation, on a bare
        // `&self` with no compiler access. Mirrors the established
        // `__mutsu_sigilless_readonly::` companion-key convention (same env
        // map, same overlay/tombstone scoping — a `my $x` that later shadows
        // an outer `constant $x` correctly un-marks it here, exactly as that
        // marker is cleared on shadowing). Only ever consulted by
        // `Interpreter::is_compile_time_constant_scalar`; scalar names only
        // (an `@`/`%`-sigiled `constant` never participates in `$name` regex
        // interpolation).
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            if is_constant {
                self.constant_var_names_seen.insert(name.to_string());
                self.env_mut()
                    .insert(format!("__mutsu_constant_var::{name}"), Value::TRUE);
            } else if is_vardecl && !is_bind && self.constant_var_names_seen.contains(name) {
                self.env_mut()
                    .remove(&format!("__mutsu_constant_var::{name}"));
            }
        }
        Ok(())
    }

    pub(super) fn exec_set_var_dynamic_op(
        &mut self,
        code: &CompiledCode,
        name_idx: u32,
        dynamic: bool,
    ) {
        let name = Self::const_str(code, name_idx);
        // The env is Symbol-keyed and this op runs on *every* `my` declaration
        // (five times per iteration of a loop body that declares five lexicals),
        // so the name is interned once per constant slot instead of on each
        // execution.
        let name_sym = code.const_sym(name_idx);
        loan_env!(self, set_var_dynamic(name, dynamic));
        // While the cross-thread shared store is active, a re-declaration is a
        // fresh binding shadowing the captured outer lexical: mark the name so
        // subsequent writes stay thread-local instead of leaking to the parent
        // through the shared store (`start { my $x ... }` / a pointy-if
        // binding must not clobber the caller's same-named lexical).
        //
        // A plain lexical `my @a` / `my %h` is masked for the same reason: the
        // store is keyed by BARE NAME and so cannot represent two concurrently
        // live bindings of one name. Without this, once any thread has run, a
        // routine's own `my @components` *is* the caller's `@components` —
        // `box_decl_local_container_cell` publishes the fresh per-binding cell
        // under the shared name key and every later read prefers it
        // (`todo/deep/threaded-array-mutation-escapes-to-the-caller.md`, the
        // top blocker for the vendored Cro::HTTP round-trip suite). The mask is
        // lifted at the next spawn, which force-`declare`s this binding's
        // current value into the lineage (`clone_for_thread`), so a genuinely
        // shared `@a` keeps working. `&` names are routines and `state` sharing
        // goes through the dedicated shared-state cells — both must keep
        // propagating. Twigil'd forms (`@!x`, `%*y`) share a name across
        // instances/dynamic scopes by design and keep the name lane.
        if self.shared_vars_active
            && !name.starts_with('&')
            && (!name.starts_with(['@', '%']) || Self::is_plain_lexical_name(name))
        {
            let is_state = code
                .state_locals
                .iter()
                .any(|(_, key)| key.contains_str(&format!("::{}@", name)));
            if !is_state {
                self.thread_redeclared_vars
                    .borrow_mut()
                    .insert(name.to_string());
                // The initializer has not run yet, so neither this frame's slot
                // nor `env` holds the new binding. Mark the window so a spawn
                // performed BY the initializer cannot unmask the name and let the
                // shadowed outer value be pulled back over it
                // (`thread_decl_in_flight`). Cleared by the store that ends it.
                self.thread_decl_in_flight.insert(name.to_string());
            }
        }
        // A fresh declaration without an explicit type must not inherit stale
        // constraints from an earlier lexical with the same name.
        self.vm_set_var_type_constraint(name, None);
        if !name.starts_with('@') && !name.starts_with('%') && !name.starts_with('&') {
            loan_env!(self, reset_atomic_var_key_decl(name));
        }
        // A fresh @-variable declaration must clear any CAS atomic array
        // state left over from a previous lexical with the same name
        // (e.g. when `my @arr[N]` is declared inside a loop body).
        if name.starts_with('@') {
            self.clear_atomic_array_state(name);
        } else if name.starts_with('%') {
            self.clear_atomic_hash_state(name);
        }
        // Before this loop-body-local declaration overwrites the env entry for
        // `name`, record the outer value it shadows so the enclosing same-named
        // binding can be restored when the loop exits. Conditions are excluded
        // (`loop_cond_active`): a `my` in a `while`/`until`/`loop` condition is
        // enclosing-scoped and often read after the loop. Only record names that
        // already had a binding (a genuine shadow — there is nothing to clobber
        // otherwise), and only the first time in this loop scope so the saved
        // value is the pre-loop one. See Interpreter::loop_local_saved_env.
        //
        // Crucially, only record names the compiler scoped to the loop body — i.e.
        // names with a local slot in `code.locals`. A `my` in a *statement
        // modifier* loop (`(my @a).push: $_ for ^3`) introduces no block, so the
        // compiler scopes it to the *enclosing* unit as an env-only var with no
        // local slot; it is enclosing-scoped (read after the loop) and must NOT be
        // restored. Restoring it would wipe an accumulated `0,1,2` back to its
        // first-iteration value. Genuine body-local shadows (`for {...{ my @a }}`)
        // always get a slot, so this distinguishes the two reliably.
        //
        // `already_loop_local` is tested FIRST (it is a plain set probe): from the
        // second iteration on it is true for every body-local `my`, which makes
        // `has_coherent_slot` false regardless — so the env read and the O(locals)
        // coherence scan below are pure waste on all but the first iteration.
        let already_loop_local = self
            .loop_local_vars
            .last()
            .is_some_and(|s| s.contains(&name_sym));
        let prev_env_value = if self.loop_cond_active || already_loop_local {
            None
        } else {
            self.env().get_sym(name_sym).cloned()
        };
        if !self.loop_cond_active && !already_loop_local && prev_env_value.is_none() {
            // The name did not exist before this body-local declaration, so there
            // is nothing to *restore* — but the entry this `my` is about to create
            // must not outlive the block either. Record a removal marker.
            //
            // Gated on the declaration having a local slot, the same discriminator
            // the shadow branch below relies on: a `my` in a *statement modifier*
            // loop (`(my @a).push: $_ for ^3`) introduces no block and is scoped to
            // the ENCLOSING unit with no slot, so it must survive the loop.
            //
            // A `state` declaration is excluded outright: it is re-executed every
            // iteration but denotes ONE binding that must persist across them (and
            // past the loop, for the next call). Removing it at loop exit emptied
            // zef's `for @*ARGS -> $arg { state @named; ... LAST { ... } }` arg
            // reordering, which broke every `zef` invocation.
            let is_state = code
                .state_locals
                .iter()
                .any(|(_, key)| key.contains_str(&format!("::{}@", name)));
            let is_body_local = !is_state && code.locals.iter().any(|n| n.as_str() == name);
            if is_body_local
                && let Some(saved) = self.loop_local_saved_env.last_mut()
                && !saved.contains_key(name)
            {
                saved.insert(name.to_string(), None);
            }
        }
        if !self.loop_cond_active
            && !already_loop_local
            && let Some(prev) = prev_env_value
        {
            // Only record a *genuine, live* enclosing binding for restoration.
            // The restore writes back both env and the local slot, so the value
            // being shadowed must be a real outer binding that is backed by a
            // local slot whose value is coherent with env. A leaked or freshly
            // hoisted env entry is NOT: e.g. a statement-modifier `(my @a)` whose
            // name collides in the frame-wide `code.locals` with a *popped* sibling
            // block's slot (`{ my @a=... } { (my @a).push: $_ for ^3 }`) leaves
            // env=[] but that slot reset to Nil — env and slot disagree. Recording
            // it would wipe the accumulated `0,1,2` back to the hoisted empty value
            // at loop exit. Requiring slot==env restores only true shadows
            // (`my @a=1,2,3; for { my @a=7,8 }` keeps the outer `1,2,3`) and leaves
            // enclosing-scoped statement-modifier declarations alone.
            // Names this loop already declared in a prior iteration
            // (`loop_local_vars`, populated below) are excluded by the
            // `already_loop_local` guard on the `if` above: after iteration 1 of an
            // accumulating statement-modifier loop, env and slot agree on the
            // partial result, which would spuriously look coherent; that value is
            // the loop's own, not an enclosing binding.
            let has_coherent_slot = code
                .locals
                .iter()
                .enumerate()
                .any(|(i, n)| n.as_str() == name && self.locals.get(i) == Some(&prev));
            if has_coherent_slot
                && let Some(saved) = self.loop_local_saved_env.last_mut()
                && !saved.contains_key(name)
            {
                saved.insert(name.to_string(), Some(prev));
            }
        }
        // Pre-initialize the variable in the env with a default value so that
        // closures created during the RHS expression can capture it.
        // This enables capture-by-reference patterns like:
        //   my $proxy := Proxy.new(STORE => -> $, \v { $proxy.VAR... })
        //
        // Skip &-sigiled variables here: seeding the lexical environment with
        // `&name = Any` before the RHS runs makes `EVAL(q[sub name() { ... }])`
        // look like a routine redeclaration instead of producing a callable to
        // bind into `my &name = ...`.
        if !name.starts_with('&') && !self.env().contains_key_sym(name_sym) {
            let default = if name.starts_with('@') {
                Value::real_array(Vec::new())
            } else if name.starts_with('%') {
                Value::hash(std::collections::HashMap::new())
            } else {
                Value::package(crate::symbol::Symbol::intern("Any"))
            };
            crate::env::note_env_key(name);
            self.env_mut().insert_sym(name_sym, default);
        }
        // Track this variable as declared within the current block scope.
        // BlockScope restoration uses this to avoid propagating block-local
        // variable values back to the outer scope.
        if let Some(set) = self.block_declared_vars.last_mut() {
            set.insert(name_sym);
        }
        // Track loop-body declarations so a closure created in the body can mark
        // this name as a per-iteration `owned_capture` (see Interpreter::loop_local_vars).
        if let Some(set) = self.loop_local_vars.last_mut() {
            set.insert(name_sym);
        }
    }

    pub(super) fn exec_assign_expr_local_op(
        &mut self,
        code: &CompiledCode,
        idx: u32,
    ) -> Result<(), RuntimeError> {
        let r = self.exec_assign_expr_local_op_inner(code, idx);
        // Phase 3 Stage 2: write-through scalar attribute writes to the cell.
        if r.is_ok() {
            self.mirror_attr_local_to_cell(code, idx as usize);
        }
        r
    }
}
