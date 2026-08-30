use super::vm_control_ops::ForLoopSpec;
use super::*;

impl Interpreter {
    /// Put the enclosing `$_` back after a `for` loop. A `for` block owns its
    /// topic — raku binds `$_` as the block's own implicit parameter — so the
    /// loop's topic must never survive into the enclosing scope. mutsu keeps the
    /// topic in env, so every exit path (normal, `last`/`next`, error) restores
    /// the value captured before the loop, or removes the key when there was
    /// none. `saved_local` additionally restores the topic's *local slot* when
    /// the frame has one — see [`Interpreter::save_loop_topic_local`].
    pub(super) fn restore_loop_topic(
        &mut self,
        saved: Option<Value>,
        saved_local: Option<(usize, Value)>,
    ) {
        match saved {
            Some(v) => {
                self.env_mut().insert("_".to_string(), v);
            }
            None => {
                self.env_mut().remove("_");
            }
        }
        if let Some((slot, v)) = saved_local {
            self.locals[slot] = v;
        }
    }

    /// The frame's local slot for `$_` (compiler-baked in
    /// [`ForLoopSpec::topic_local`]), together with its value on loop entry —
    /// `None` when the topic has no slot in this frame, which is the common case.
    ///
    /// `code.locals` positions never move within a frame, so the slot index
    /// stays valid across the body.
    pub(super) fn save_loop_topic_local(&mut self, spec: &ForLoopSpec) -> Option<(usize, Value)> {
        let slot = spec.topic_local? as usize;
        Some((slot, self.locals.get(slot)?.clone()))
    }

    /// Put the topic's read-only marking back to what it was before a `for`
    /// loop that decides the marking per item (see the `take-rw` case in
    /// `vm_for_loop_lazy.rs`).
    pub(super) fn restore_topic_readonly(&mut self, saved: Option<crate::ast::ReadonlyKind>) {
        match saved {
            Some(kind) => self.mark_readonly_with("_", kind),
            None => self.unmark_readonly("_"),
        }
    }

    /// Bind the loop's implicit topic: `env["_"]` plus the frame's topic slot
    /// when it has one (see [`Interpreter::save_loop_topic_local`]).
    pub(super) fn set_loop_topic(&mut self, topic_local: Option<usize>, val: Value) {
        if let Some(slot) = topic_local {
            self.locals[slot] = val.clone();
        }
        self.env_mut().insert("_".to_string(), val);
    }

    /// The `'$name'`-quoted form a loop parameter's bare env-key name
    /// (sigil-stripped for scalars, sigil-kept for `@`/`%`/`&`) appears as in
    /// a `X::TypeCheck::Binding::Parameter` message, matching how routine
    /// parameter binding errors display a parameter name elsewhere.
    fn for_param_display_name(name: &str) -> String {
        if name.starts_with(['@', '%', '&']) {
            name.to_string()
        } else {
            format!("${}", name)
        }
    }

    /// Drop the cross-thread bare-name-lane masks a named-param `for` loop
    /// installed on entry. Only the names the loop itself added are dropped —
    /// an enclosing `my` of the same name keeps its own mask. Called on every
    /// exit path, including the error returns, so a mask can never outlive the
    /// binding it describes.
    fn unmask_for_params(&mut self, names: &[String]) {
        let mut redeclared = self.thread_redeclared_vars.borrow_mut();
        for name in names {
            redeclared.remove(name);
        }
    }

    /// Whether the value a loop iteration is about to bind already **is** (or
    /// contains) an element container handed out by a container-aware producer.
    ///
    /// A single-parameter loop binds the item itself, so the item is the cell.
    /// A multi-parameter loop binds out of a chunk — `for @a.kv -> $i, $v is rw`
    /// gets `[index, cell]` — and the bind-prefix `Stmt::Assign`s hand `$v` the
    /// cell, so the chunk carrying one anywhere is equally a reason to retire
    /// the writeback: writing a cell back over the source element would replace
    /// the element with a container instead of assigning into it.
    fn binding_carries_element_cell(item: &Value) -> bool {
        if item.is_container_ref() {
            return true;
        }
        match item.view() {
            ValueView::Array(chunk, _) => chunk.items().iter().any(Value::is_container_ref),
            _ => false,
        }
    }

    #[allow(clippy::too_many_arguments)]
    pub(super) fn exec_for_loop_body(
        &mut self,
        code: &CompiledCode,
        spec: &ForLoopSpec,
        items: &[Value],
        body_start: usize,
        loop_end: usize,
        compiled_fns: &CompiledFns,
        resume_index: usize,
    ) -> Result<bool, RuntimeError> {
        // This construct handles `next`/`last`/`redo`, so a loop-control
        // statement raised anywhere in its dynamic extent has somewhere to go
        // (`runtime/loop_handler_depth.rs`). Without the guard the raise site
        // would convert the signal into a thrown `X::ControlFlow` and silently
        // break this loop.
        let _loop_handler = crate::runtime::loop_handler_depth::LoopHandlerGuard::new();
        // `true`  = the loop ran every item to completion;
        // `false` = it exited early via `last` or `return` (the live-array
        // continuation in `exec_for_loop_op` must NOT pick up newly-pushed tail
        // elements after an early exit).
        let mut completed_all = true;
        // Nested-resume entry: when the slot holds a state for a loop nested
        // INSIDE this body (its loop_ip lies in the body range), the resumed
        // iteration's first body run starts AT that loop op — re-running the
        // ops before it would replay completed sibling loops / side effects.
        let this_code_id = code.ops.as_ptr() as usize;
        let mut nested_entry: Option<usize> = self.gather_resume_body_ip.take().or_else(|| {
            self.gather_for_loop_resume
                .as_ref()
                .filter(|s| s.code_id() == Some(this_code_id))
                .and_then(|s| s.loop_ip())
                .filter(|lip| *lip > body_start && *lip < loop_end)
        });
        let param_name = spec
            .param_idx
            .map(|idx| match code.constants[idx as usize].view() {
                ValueView::Str(s) => s.to_string(),
                _ => unreachable!("ForLoop param must be a string constant"),
            });

        let arity = spec.arity.max(1) as usize;
        // The implicit-topic loop (`for @a { $_ = ... }`). A multi-param
        // signature binds named parameters out of the chunk instead, so `$_`
        // holds the chunk, not a source element — writing it back would
        // overwrite the element with the chunk. (Reachable at `arity == 1` only
        // since a trailing slurpy started forcing one element per iteration.)
        let writes_back_topic = spec.param_idx.is_none()
            && spec.param_local.is_none()
            && spec.arity <= 1
            && spec.multi_param_names.is_empty();
        let mut rw_writeback = spec.do_writeback;
        // ADR-0045 slice 3: a plain (non-rw, non-copy) named loop variable used
        // to get a writeback of its own, on the theory that `for @m -> @row
        // { @row.push(9) }` and `for @m -> $row { $row.push(9) }` need one to
        // mutate `@m`. **They do not, and the writeback was actively harmful.**
        //
        // A `-> $v` parameter binds the element's *value*, not its container
        // (§1.1, measured: `for @a -> $v { @a[0] = 9; say $v }` prints `1` in
        // raku, and the deferred-read form prints `1 2` — rows 45/46), so there
        // is nothing for it to write back. In-place *container* mutation
        // through such a parameter already propagates on its own, because the
        // bound value shares the source element's `Gc` (rows 10/32/33), and the
        // parameter cannot be assigned at all (row 31). Meanwhile the writeback
        // was a whole-container rebuild from a snapshot taken before the body
        // ran, so an ordinary `for @a -> $v { @a[1] = 99 }` — no `rw`, no
        // closure, no advanced feature — silently lost the write (row 22).
        //
        // So the named-parameter half is a **pure deletion**: nothing replaces
        // it. Only the implicit topic keeps a writeback here, and only until it
        // is promoted below.
        //
        // `.pairs`/`.antipairs` loop variables are `Pair`s wrapping the element,
        // not the element itself — writing one back would overwrite the source
        // element with the Pair (S32-array/pairs.t 14). The Pair's rw `.value`
        // alias handles propagation (and immutability for Mix), so suppress the
        // plain writeback here while keeping the source tag.
        let writes_back_loop_var = writes_back_topic && !spec.loop_var_wraps_element;
        let chunked_items: Vec<Value> = if spec.chunks_items() {
            items
                .chunks(arity)
                .map(|chunk| Value::array(chunk.to_vec()))
                .collect()
        } else {
            items.to_vec()
        };
        // ADR-0052 Slice 1: a construct that runs a body owns a stack base and
        // truncates to it at the end of EVERY iteration, not only when it is
        // collecting. A sink-position loop used to establish no base at all, so
        // anything an iteration left behind (a body that exits mid-statement via
        // `succeed`, and — once the clause starts pushing — a non-matching
        // `when`) piled up one value per pass.
        let stack_base = self.stack.len();
        let mut collected = if spec.collect { Some(Vec::new()) } else { None };
        let mut deferred_container_refs: Vec<(usize, String)> = Vec::new();
        // A `for` block owns its topic (raku binds `$_` as the block's own
        // implicit parameter), so the enclosing `$_` is restored on every exit —
        // normal, `last`/`next`, and error.
        let saved_topic = self.env().get("_").cloned();
        // The topic's local slot, when this frame has one (see
        // `save_loop_topic_local`): the loop must mirror each item into it.
        let saved_topic_local = self.save_loop_topic_local(spec);
        let topic_local = saved_topic_local.as_ref().map(|(s, _)| *s);
        let saved_topic_source = self.topic_source_var.take();
        let saved_quanthash_bind = std::mem::take(&mut self.quanthash_bind_params);
        // The tagged source name plus its compile-time-baked local slot (§1.5):
        // the slot lets the topic writeback target the exact `locals` slot when
        // shadow slots are active (a shadowed name occupies several slots and
        // the by-name `position` search resolves to the outer one).
        let container_binding_full = self.take_container_ref_for(code);
        let container_source_slot = container_binding_full.as_ref().and_then(|(_, s)| *s);
        let container_binding = container_binding_full.map(|(n, _)| n);
        // A sigilless/`is rw` for-param aliases the source element, but an
        // *immutable* Mix/Set/Bag yields immutable weights — assigning to the
        // alias must throw X::Assignment::RO, and no writeback may run (it would
        // corrupt the immutable collection). Mutable MixHash/BagHash/SetHash and
        // arrays/hashes are unaffected. Detect the immutable-QuantHash source at
        // runtime (the compiler cannot know mutability) and force the params
        // read-only with writeback suppressed.
        let source_immutable_quant = container_binding.as_ref().is_some_and(|name| {
            matches!(
                self.get_env_with_main_alias(name).as_ref().map(Value::view),
                Some(ValueView::Mix(_, false))
                    | Some(ValueView::Set(_, false))
                    | Some(ValueView::Bag(_, false))
            )
        });
        if source_immutable_quant {
            rw_writeback = false;
        }
        // A *mutable* QuantHash (MixHash/BagHash/SetHash) source iterated via
        // `.values`/`.kv`/`.pairs` aliases its weights: `$_ = X for $b.values`,
        // `for $b.kv -> \k,\v { v = X }` and `.value = X for $b.pairs` all mutate
        // the QuantHash. Detect it so the topic stays writable (not readonly) and
        // the writeback paths can update the weight by key order, coercing the
        // assigned value (X::Str::Numeric on a bad string; weight 0 removes the key).
        let source_mutable_quant = container_binding.as_ref().is_some_and(|name| {
            matches!(
                self.get_env_with_main_alias(name).as_ref().map(Value::view),
                Some(ValueView::Mix(_, true))
                    | Some(ValueView::Set(_, true))
                    | Some(ValueView::Bag(_, true))
            )
        });
        let container_reversed = self.container_ref_reversed;
        self.container_ref_reversed = false;
        // Capture hash key order before the loop so writeback uses the
        // original key order even after the hash is mutated during iteration.
        // Needed for the rw-param `%h.values -> $v is rw` writeback and for the
        // plain topic `$_ = X for %h.values` writeback (values_mode).
        let hash_keys_for_writeback: Option<Vec<String>> =
            if rw_writeback || (writes_back_loop_var && spec.values_mode) {
                container_binding.as_ref().and_then(|source| {
                    // Deref a `ContainerRef` cell (a `:=`-bound hash) so the key
                    // order is captured from the inner Hash (Stage 1).
                    match self
                        .get_env_with_main_alias(source)
                        .as_ref()
                        .map(|v| v.deref_container())
                        .as_ref()
                        .map(Value::view)
                    {
                        Some(ValueView::Hash(hash_items)) if source.starts_with('%') => {
                            Some(hash_items.keys().cloned().collect())
                        }
                        // A mutable QuantHash bound to a scalar: capture the weight
                        // map's key order so `.values`/`.kv` writeback lands on the
                        // same key `.values()`/`.kv` yielded (same unmodified map →
                        // identical iteration order).
                        Some(ValueView::Bag(b, true)) => Some(b.keys().cloned().collect()),
                        Some(ValueView::Mix(m, true)) => Some(m.keys().cloned().collect()),
                        Some(ValueView::Set(s, true)) => Some(s.elements.iter().cloned().collect()),
                        _ => None,
                    }
                })
            } else {
                None
            };
        // Save multi-param values and readonly state so they can be restored
        // after the loop (inner loops must not clobber outer scope bindings).
        // Also snapshot each name's compile-time-baked local slot (§1.5,
        // `ForLoopSpec::multi_param_locals`) directly — `build_for_bind_stmts`
        // binds a multi-param via plain `Stmt::Assign`, not a `my`-style
        // declaration, so it never gets a fresh shadow slot: it overwrites
        // whatever slot the name already occupied. That outer local is not
        // necessarily mirrored into `env` (single-store default), so relying
        // on the `env` snapshot alone silently drops the local-slot restore
        // whenever the outer binding is a pure local
        // (`todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md`).
        // (name, saved env value, saved readonly kind, saved sigilless-readonly flag, saved (slot, value))
        type SavedMultiParam = (
            String,
            Option<Value>,
            Option<crate::ast::ReadonlyKind>,
            Option<Value>,
            Option<(usize, Value)>,
        );
        let saved_multi_params: Vec<SavedMultiParam> = spec
            .multi_param_names
            .iter()
            .enumerate()
            .map(|(i, name)| {
                let val = self.env().get(name).cloned();
                let was_readonly = self.readonly_kind(name);
                let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
                let sigilless_ro = self.env().get(&sigilless_key).cloned();
                let saved_local = spec
                    .multi_param_locals
                    .get(i)
                    .copied()
                    .flatten()
                    .map(|slot| (slot as usize, self.locals[slot as usize].clone()));
                (name.clone(), val, was_readonly, sigilless_ro, saved_local)
            })
            .collect();
        // A multi-param loop variable is a FRESH per-iteration binding
        // (ADR-0023 provenance), but `build_for_bind_stmts` binds it via a
        // plain `Stmt::Assign`, whose exec writes THROUGH a `ContainerRef`
        // when the shadowed outer name is a boxed cell — corrupting the outer
        // binding for every alias of the cell. roast
        // integration/advent2013-day14.t's `config_combiner`: the captured
        // vow `$v` is a cell (Instance boxing, ADR-0025 slice 1), and
        // `for %kvs.kv -> $k, $v` wrote each config Str into that cell, so
        // `$v.keep(%result)` after the loop called .keep on a Str and the
        // returned promise never resolved. Sever a scalar cell binding up
        // front: the save above keeps the cell itself for the post-loop
        // restore, so only the loop-duration binding becomes a plain fresh
        // value. `@`/`%`/`&` keep their container-cell aliasing (see the
        // slot-restore comment below).
        for (i, name) in spec.multi_param_names.iter().enumerate() {
            if name.is_empty() || name.starts_with(['@', '%', '&']) {
                continue;
            }
            if matches!(
                self.env().get(name).map(Value::view),
                Some(ValueView::ContainerRef(_))
            ) {
                self.env_mut().remove(name);
            }
            if let Some(slot) = spec.multi_param_locals.get(i).copied().flatten() {
                let slot = slot as usize;
                if self.locals[slot].is_container_ref() {
                    self.locals[slot] = Value::NIL;
                }
            }
        }
        // A multi-parameter loop (`-> $k, $v`) binds its parameters with plain
        // assignments emitted into the body prefix (`build_for_bind_stmts`), and
        // `SetLocal` type-checks an assignment against the *name-keyed* constraint
        // map. That map is not block-scoped, so an unrelated `my Int $v` anywhere
        // in the program made `-> $k, $v` reject every non-Int value
        // ("Type check failed in assignment to $v; expected Int"). A parameter is
        // a fresh binding that shadows whatever the name meant outside, so clear
        // the constraint for the duration of the loop and restore it after — the
        // same contract `bind_param_type_constraint` gives an untyped routine
        // parameter, minus the permanent loss of the enclosing lexical's type.
        // (The single-param form binds natively and never had this problem.)
        //
        // Loop parameter types themselves are still unenforced: `ForLoopSpec`
        // carries no per-parameter constraint. See
        // `todo/tickets/for-loop-multi-param-types-unenforced.md`.
        let saved_multi_param_types: Vec<(String, Option<String>)> = spec
            .multi_param_names
            .iter()
            .map(|name| {
                let tc = loan_env!(self, var_type_constraint(name));
                (name.clone(), tc)
            })
            .collect();
        for (name, tc) in &saved_multi_param_types {
            if tc.is_some() {
                self.vm_set_var_type_constraint(name, None);
            }
        }
        // A named loop variable is a FRESH per-iteration binding, so it must
        // not join the cross-thread bare-name lane — the same rule a `my`
        // re-declaration gets in `exec_set_var_dynamic_op`. Both the native
        // single-param bind and `build_for_bind_stmts`' multi-param assignments
        // otherwise publish every iteration's value under the BARE NAME, where
        // a later cross-thread drain can read an older value back over the fresh
        // binding. For `@row`, that made every iteration after a `start` mutate
        // the first row's stale container. The same lane also let an unrelated
        // frame using the same name read the loop binding back at its next
        // `await` (`sync_shared_vars_to_env`). That is how
        // Cro's `for @components-in.kv -> $i, $comp` pipeline compose rewrote the
        // `$i` of a `for 1..5 -> $i` loop in the user's own test file
        // (`todo/deep/shared-store-bare-name-collision-across-unrelated-frames.md`).
        // A `start` block that genuinely captures such a variable is unaffected:
        // it gets a per-binding `ContainerRef` cell from `box_captured_lexicals`,
        // which is the mechanism the lane is redundant with.
        let masked_params: Vec<String> = spec
            .multi_param_names
            .iter()
            .chain(param_name.iter())
            .filter(|name| !name.starts_with('&') && name.as_str() != "_")
            .filter(|name| {
                self.thread_redeclared_vars
                    .borrow_mut()
                    .insert((*name).clone())
            })
            .cloned()
            .collect();
        // Save the single named loop param (`for ... -> $x`) too, so a loop in a
        // called sub that reuses the same variable name does not clobber an outer
        // loop's binding of that name (the env keys these by bare name). Skip
        // `@`/`%` sigils, which bind a shared mutable container the body may
        // legitimately reassign, and skip the rw case (handled via writeback).
        let saved_param: Option<(String, Option<Value>, Option<u32>)> = param_name
            .as_ref()
            .filter(|n| !n.starts_with('@') && !n.starts_with('%'))
            .map(|name| {
                (
                    name.clone(),
                    self.env().get(name).cloned(),
                    spec.param_local,
                )
            });
        // Track loop-body declarations for per-iteration closure capture
        // (owned_captures). Balanced by pop on every exit.
        //
        // ADR-0023: the loop's own parameter name(s) are fresh, readonly,
        // per-iteration bindings independent of value type — recording them
        // here lets `block_captured_scalars` keep a spawned `start {}`'s
        // capture of the loop parameter off the cross-thread bare-name lane
        // even when the item is not a "plain" scalar type (e.g. an Instance).
        // Gate on `is_rw`: an `<->`/rw loop param writes back to the source
        // element, so keep it on its pre-ADR path.
        let loop_param_names: rustc_hash::FxHashSet<String> = if spec.is_rw {
            Default::default()
        } else {
            param_name
                .iter()
                .cloned()
                .chain(
                    spec.multi_param_names
                        .iter()
                        .map(|name| name.trim_start_matches('$').to_string()),
                )
                .filter(|name| {
                    !name.starts_with('&')
                        && !name.starts_with('@')
                        && !name.starts_with('%')
                        && name != "_"
                })
                .collect()
        };
        self.push_loop_local_scope(loop_param_names.clone());
        // ADR-0027: the for-loop's own pointy parameter(s) are a genuine
        // per-iteration fresh binding but, unlike an ordinary loop-body `my`
        // declaration, are bound via a direct env/slot store (below) that
        // never runs through the generic declaration path
        // (`exec_set_var_dynamic_op`) which populates `loop_local_vars` for
        // `my` locals. Without this, `compute_owned_captures` never marks a
        // closure over the pointy param itself as loop-owned — invisible for
        // a closure invoked standalone (its captured value simply has no
        // competing binding to lose to), but wrong the moment such a closure
        // is invoked NESTED inside a call frame that has a DIFFERENT
        // iteration's value of the same name live (the IIFE-factory shape;
        // see `todo/deep/for-loop-var-shared-across-nested-closure-captures.md`).
        // Reuses the exact name set just pushed for `active_loop_param_names`
        // (ADR-0023), so this is additive bookkeeping, not a new computation.
        if let Some(set) = self.loop_local_vars.last_mut() {
            set.extend(loop_param_names.iter().map(|n| Symbol::intern(n)));
        }
        // Determine if the implicit topic ($_) should be read-only.
        // Only mark $_ readonly when iterating over an *immutable* collection
        // (Mix/Set/Bag, the `(_, false)` variants). This blocks `.value = ...`
        // and `$_ = ...` mutations on values/pairs from immutable collections
        // while keeping $_ writable for a *mutable* QuantHash (MixHash/BagHash —
        // `for $b.values { $_ = X }` / `.value = X for $b.pairs` must write back),
        // expression results, multi-param loops, and Scalar containers.
        //
        // A source whose items are provably bare values (`for 1, 2`,
        // `for <a b>`, `for %h.keys` — see
        // `ForLoopSpec::source_items_are_bare`) gets the same treatment: the
        // topic aliases the item itself, with no container behind it, so raku
        // rejects `$_ = ...` and reports the item's own type from `.VAR`.
        let topic_readonly =
            !spec.is_rw && param_name.is_none() && spec.multi_param_names.is_empty() && {
                spec.source_items_are_bare
                    || match &container_binding {
                        None => false,
                        Some(name) => {
                            if let Some(val) = self.get_env_with_main_alias(name) {
                                matches!(
                                    val.view(),
                                    ValueView::Mix(_, false)
                                        | ValueView::Set(_, false)
                                        | ValueView::Bag(_, false)
                                )
                            } else {
                                false
                            }
                        }
                    }
            };
        let total_items = chunked_items.len();
        // `is copy` loop param (is_rw set, do_writeback suppressed): the param
        // owns a DISTINCT container per iteration. Mutations write through the
        // shared backing node (container identity §3), so binding the element
        // value as-is would let `@row[0] = v` reach the source element.
        let param_is_copy = spec.is_rw && !spec.do_writeback;
        // ADR-0045 slices 1-3: an aliasing binding — `is rw` / `<->` /
        // sigilless `\v`, or the implicit topic — over a real mutable
        // `Array`/`Hash` source binds the element's own `ContainerRef` instead
        // of a value clone, and the per-iteration writeback for that shape is
        // retired. The alias then has the lifetime of the binding, not of the
        // body: a closure or `start` block that outlives the iteration still
        // writes through, a read through the alias sees a later write to the
        // element, and a direct `@a[i] = v` in the body is no longer reverted
        // by an end-of-iteration whole-container rebuild. It also removes that
        // rebuild's O(n^2) (§1.5) — promotion is O(1) and idempotent.
        //
        // `plan_for_element_alias` owns the whole discriminator (which
        // parameters alias, which sources do, and the shaped/native/`Map`
        // carve-outs); see `vm_for_loop_alias.rs`.
        let element_alias = self.plan_for_element_alias(
            code,
            spec,
            container_binding.as_deref(),
            container_reversed,
            arity,
            param_name.as_deref(),
            writes_back_topic,
            topic_readonly,
            hash_keys_for_writeback.as_deref(),
            &chunked_items,
        );
        // The base decisions; each iteration retires them for itself only when
        // the element really was promoted (see the bind site below).
        // `spec.do_writeback` itself is left alone: ADR-0040's bind-side
        // itemization carve-out keys off it, and ADR-0045 §5 Q3 keeps that
        // carve-out unchanged here.
        let rw_writeback_base = rw_writeback;
        let topic_writeback_base = writes_back_loop_var;
        // Set per iteration at the bind site below; the initial value is never
        // read.
        let mut writes_back_loop_var;
        'for_loop: for (idx, item) in chunked_items.into_iter().enumerate().skip(resume_index) {
            let item = if param_is_copy {
                item.detach_shared_container()
            } else {
                item
            };
            // A Proxy element FETCHes on iteration in value context (raku:
            // `for $proxy-list.list { }` yields the values). An rw loop
            // (`<->`) keeps the Proxy so writes go through STORE.
            let item = if !spec.is_rw && matches!(item.view(), ValueView::Proxy { .. }) {
                loan_env!(self, auto_fetch_proxy(&item))?
            } else {
                item
            };
            // Enforce declared loop-parameter types at BIND time, not via the
            // bind-prefix `Stmt::Assign`'s plain, untyped `SetLocal` (which
            // would check nothing at all for a multi-param loop, and would
            // raise an "assignment" error with the wrong exception class for
            // a single-param one). Raku raises
            // `X::TypeCheck::Binding::Parameter` here, so build that directly
            // from the per-iteration `item` before it's ever bound.
            // (todo/tickets/for-loop-multi-param-types-unenforced.md)
            if let Some(ref name) = param_name {
                if let Some(tc) = spec.param_type_constraint.as_deref()
                    && !self.type_matches_value(tc, &item)
                {
                    let display = Self::for_param_display_name(name);
                    self.unmask_for_params(&masked_params);
                    return Err(RuntimeError::typecheck_binding_parameter_with_repr(
                        &display, tc, &item,
                    ));
                }
            } else if !spec.multi_param_type_constraints.is_empty()
                && let ValueView::Array(chunk, ..) = item.view()
            {
                for (i, tc) in spec.multi_param_type_constraints.iter().enumerate() {
                    let Some(tc) = tc else { continue };
                    let Some(v) = chunk.items().get(i) else {
                        continue;
                    };
                    if !self.type_matches_value(tc, v) {
                        let display = spec
                            .multi_param_names
                            .get(i)
                            .map(|n| Self::for_param_display_name(n))
                            .unwrap_or_default();
                        self.unmask_for_params(&masked_params);
                        return Err(RuntimeError::typecheck_binding_parameter_with_repr(
                            &display, tc, v,
                        ));
                    }
                }
            }
            // ADR-0045 slices 1-3: promote this element to its own container
            // and bind THAT, so the binding is a real alias for the lifetime of
            // the binding.
            //
            // A `Proxy` element is left alone (ADR-0045 §5 Q6): it mediates its
            // own STORE, and a cell bound *around* one would take a plain write
            // instead of calling it (`t/proxy-list-transparency.t`).
            let promoted =
                if element_alias.is_active() && !matches!(item.view(), ValueView::Proxy { .. }) {
                    self.for_element_alias(code, &element_alias, idx)
                } else {
                    None
                };
            // ADR-0036 slice 3 / ADR-0045 slice 4: the item may ALREADY be an
            // element container, because a container-aware producer handed it
            // out (`for @a.reverse`, `for @a.sort`, `for @a.values`, and the
            // `.kv` chunk's value slot). That is the whole point of routing at
            // the producer: the item carries its own identity, so the loop needs
            // no index reconstruction — and there is nothing to write back,
            // whatever order the producer chose. This is what makes
            // `container_reversed` correct-by-construction for `.reverse`
            // instead of a mirror-image index to compute, and what gives `.sort`
            // an alias at all when it has no index to reconstruct.
            let item_carries_cell = Self::binding_carries_element_cell(&item);
            // Both writebacks are retired for exactly the iterations whose
            // element was promoted (here or at the producer). An iteration that
            // fell back to a plain value bind — a `Proxy` element, or a body
            // that removed the index/key out from under the loop — keeps the
            // writeback that bind depends on. Retiring per LOOP instead of per
            // ITERATION silently drops such an iteration's write.
            let aliased = promoted.is_some() || item_carries_cell;
            rw_writeback = rw_writeback_base && !aliased;
            writes_back_loop_var = topic_writeback_base && !aliased;
            let item = promoted.unwrap_or(item);
            // `topic_source_var` drives the whole-topic writeback for a scalar
            // source (`for $x { $_[1] = ... }` writes the mutated `$_` back to
            // `$x`). For a `.values` loop over a mutable QuantHash the topic is a
            // *weight*, not the container — wholesale-overwriting `$b` with the
            // weight would clobber the MixHash/BagHash. The per-element
            // `write_back_quanthash_value_item` handles that source, so suppress
            // the whole-topic writeback here.
            self.topic_source_var =
                if writes_back_topic && !(spec.values_mode && source_mutable_quant) {
                    container_binding.clone()
                } else {
                    None
                };
            // Only set $_ when no named parameter is given (for @list { ... })
            // When -> $k is used, $_ should remain from the enclosing scope
            if param_name.is_none() {
                self.set_loop_topic(topic_local, item.clone());
            }
            // A plain `$`-sigiled loop parameter is an item binding: the bound
            // element behaves as ONE value in list context (a row Array fed to
            // a sprintf slurpy stays one argument; `.raku` shows `$[...]`),
            // matching Raku's `-> $v` signature binding. Sigilless (`\v`) and
            // `<->` rw params bind raw; `@`/`%`/`&` params bind the container
            // itself; the implicit topic (param_name None) also stays raw.
            // Itemizing keeps the SAME backing Gc (only the kind flips), so
            // `loop_var_unchanged`'s ptr_eq still sees in-place mutations and
            // the source-element writeback stays a no-op for read-only loops.
            let item = match param_name.as_deref() {
                Some(name) if !name.starts_with(['@', '%', '&', '\\']) && !spec.do_writeback => {
                    Self::itemize_scalar_store(name, item)
                }
                // An `@`/`%`-sigil parameter binds a Positional/Associative, so
                // when the element it binds is a shared `ContainerRef` cell —
                // the rw-alias cell `.grep` leaves in its source array, or a
                // `:=`-bound element — it must bind the container INSIDE the
                // cell, not the cell itself. Otherwise `@row.push(8)` pushes
                // onto a cell rather than the row (`t/for-loop-cell-elements.t`).
                // A shared `Gc` is what makes the mutation propagate, and the
                // deref keeps it: the cell holds the very same `Gc`.
                //
                // This used to be masked by the plain named parameter's
                // writeback, which ADR-0045 slice 3 deletes — the writeback
                // re-stored the mutated binding over the element, hiding the
                // fact that the binding was never the container to begin with.
                Some(name) if name.starts_with(['@', '%']) => item.deref_container(),
                _ => item,
            };
            if let Some(ref name) = param_name {
                self.env_mut().insert(name.clone(), item.clone());
                // Create non-twigil alias for placeholder params: $^a → $a
                if let Some(bare) = name.strip_prefix("&^") {
                    self.env_mut().insert(format!("&{}", bare), item.clone());
                } else if let Some(bare) = name.strip_prefix('^') {
                    self.env_mut().insert(bare.to_string(), item.clone());
                }
            }
            if let Some(slot) = spec.param_local {
                self.locals[slot as usize] = item.clone();
            }
            // Mark implicit $_ readonly when source is immutable.
            // Also set a deep-readonly flag so that method-lvalue
            // assignments like .value = ... are blocked too.
            if topic_readonly {
                // The topic aliases an immutable item directly, with no
                // container of its own: rakudo throws X::AdHoc "Cannot assign
                // to an immutable value" (not the readonly-*variable* wording
                // a named `-> $v` alias gets).
                self.mark_readonly_with("_", crate::ast::ReadonlyKind::Immutable);
                self.env_mut()
                    .insert("__mutsu_deep_readonly::_".to_string(), Value::TRUE);
            }
            // Mark named params readonly when not in rw mode.
            // Skip @-sigil and %-sigil params: they bind to a mutable
            // Array/Hash container, so assignments like `@a = values` must
            // be allowed (matching Raku semantics).
            if !spec.is_rw
                && let Some(ref name) = param_name
                && !name.starts_with('@')
                && !name.starts_with('%')
            {
                self.mark_readonly(name);
            }
            // `%`-sigil for-loop bindings preserve a QuantHash value (and keep
            // its type across a `%a = ...pairs` reset) instead of coercing it to
            // a plain Hash — Raku binds params, it does not assign-coerce them.
            self.quanthash_bind_params = spec
                .multi_param_names
                .iter()
                .chain(param_name.iter())
                .filter(|n| n.starts_with('%'))
                .cloned()
                .collect();
            // Temporarily clear readonly flags for multi-param names
            // so the bind stmts (Stmt::Assign) at the start of the body can
            // re-bind variables that may be readonly from an outer scope.
            for mp_name in &spec.multi_param_names {
                // Clear regular readonly flag
                self.unmark_readonly(mp_name);
                // Clear sigilless readonly flag
                let key = format!("__mutsu_sigilless_readonly::{}", mp_name);
                self.env_mut().insert(key, Value::FALSE);
            }
            'body_redo: loop {
                let run_start = nested_entry.take().unwrap_or(body_start);
                if let Some(slot) = spec.block_callable_local {
                    self.push_block(self.locals[slot as usize].clone());
                }
                let mut body_result = self.run_range(code, run_start, loop_end, compiled_fns);
                if spec.block_callable_local.is_some() {
                    self.pop_block();
                }
                // An immutable Mix/Set/Bag source yields immutable weights: if the
                // body modified a sigilless/rw alias, Raku throws X::Assignment::RO.
                // Detect it here (writeback is already suppressed above) and convert
                // a successful body into the same error, so the shared Err arm runs
                // its readonly/topic cleanup before propagating.
                if body_result.is_ok()
                    && source_immutable_quant
                    && let Some(err) = self.immutable_quant_param_mutation(
                        &param_name,
                        &spec.multi_param_names,
                        &item,
                    )
                {
                    body_result = Err(err);
                }
                // `$_ = X for $b.values` / `for $b.values -> $v { $v = X }` where
                // `$b` is a mutable QuantHash: write the aliased weight back here,
                // before the match, so a coercion failure (X::Str::Numeric on a
                // non-numeric string) flows through the shared Err-arm cleanup. The
                // Ok arm's `write_back_for_topic_item` skips scalar quant sources.
                if body_result.is_ok()
                    && writes_back_loop_var
                    && spec.values_mode
                    && source_mutable_quant
                    && let Some(ref source) = container_binding
                    && let Err(err) = self.write_back_quanthash_value_item(
                        code,
                        source,
                        &param_name,
                        idx,
                        &hash_keys_for_writeback,
                    )
                {
                    body_result = Err(err);
                }
                // The rw-param sibling of the above: `for $b.kv -> \k, \v { v = X }`
                // and `for $b.values -> $v is rw { $v = X }` over a mutable QuantHash.
                // Handled pre-match (coercion may raise X::Str::Numeric) so the
                // Ok-arm `write_back_for_rw_param` (which no-ops on scalar quant
                // sources) need not change.
                if body_result.is_ok()
                    && rw_writeback
                    && source_mutable_quant
                    && (spec.kv_mode || spec.values_mode)
                    && let Some(ref source) = container_binding
                    && let Err(err) = self.write_back_quanthash_rw(
                        code,
                        source,
                        &spec.rw_param_names,
                        &param_name,
                        idx,
                        spec.kv_mode,
                        &hash_keys_for_writeback,
                    )
                {
                    body_result = Err(err);
                }
                // Sync state variables modified in this iteration so that
                // StateVarInit in the next iteration sees the updated values.
                // State mutations persist on every exit path (`next`/`redo`/
                // `last`/exception), not just normal completion.
                if !code.state_locals.is_empty() {
                    self.sync_state_locals_in_range(code, body_start, loop_end);
                }
                match body_result {
                    Ok(()) => {
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                container_source_slot,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            idx,
                        );
                        if let Some(ref mut coll) = collected
                            && self.stack.len() > stack_base
                        {
                            let val = self.stack.pop().unwrap();
                            let deferred_ref = self.take_container_ref_for(code).map(|(n, _)| n);
                            let coll_start_len = coll.len();
                            Self::collect_loop_value(coll, val);
                            if let Some(name) = deferred_ref
                                && coll.len() == coll_start_len + 1
                            {
                                deferred_container_refs.push((coll_start_len, name));
                            }
                        }
                        // Drain anything else this iteration left behind.
                        self.stack.truncate(stack_base);
                        break 'body_redo;
                    }
                    Err(e) if e.is_succeed() => {
                        // A matched `when` abandons the body mid-range, so drop
                        // whatever it had already pushed (ADR-0052 Slice 1); the
                        // clause's value travels in the signal, not here.
                        self.stack.truncate(stack_base);
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                container_source_slot,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e) if e.is_redo() && Self::label_matches(&e.label, &spec.label) => {
                        // The iteration restarts from the top; anything the
                        // abandoned pass pushed is not part of the retry.
                        self.stack.truncate(stack_base);
                        if param_name.is_none() {
                            self.set_loop_topic(topic_local, item.clone());
                        }
                        if let Some(ref name) = param_name {
                            self.env_mut().insert(name.clone(), item.clone());
                        }
                        if let Some(slot) = spec.param_local {
                            self.locals[slot as usize] = item.clone();
                        }
                        continue 'body_redo;
                    }
                    Err(e)
                        if e.is_leave
                            && e.leave_callable_id().is_none()
                            && e.leave_routine().is_none()
                            && Self::label_matches(&e.label, &spec.label) =>
                    {
                        // `leave` ends the loop; its value comes from the signal
                        // (pushed below), not from the abandoned body's stack.
                        self.stack.truncate(stack_base);
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                container_source_slot,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            idx,
                        );
                        if let Some(v) = e.return_value {
                            if let Some(ref mut coll) = collected {
                                Self::collect_loop_value(coll, v.clone());
                            } else {
                                self.set_loop_topic(topic_local, v.clone());
                                // Push return value on stack so enclosing compiled
                                // closures can see it as the block result.
                                self.stack.push(v);
                            }
                        }
                        completed_all = false;
                        break 'for_loop;
                    }
                    Err(e) if e.is_last() && Self::label_matches(&e.label, &spec.label) => {
                        self.stack.truncate(stack_base);
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                container_source_slot,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            idx,
                        );
                        completed_all = false;
                        break 'for_loop;
                    }
                    Err(e) if e.is_next() && Self::label_matches(&e.label, &spec.label) => {
                        self.stack.truncate(stack_base);
                        if writes_back_loop_var {
                            self.write_back_for_topic_item(
                                code,
                                &container_binding,
                                container_source_slot,
                                &param_name,
                                idx,
                                container_reversed,
                                total_items,
                                spec.values_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        if rw_writeback {
                            self.write_back_for_rw_param(
                                code,
                                &container_binding,
                                &param_name,
                                &spec.rw_param_names,
                                idx,
                                arity,
                                spec.kv_mode,
                                &hash_keys_for_writeback,
                            );
                        }
                        self.write_back_to_source_var(
                            code,
                            &spec.source_var_names,
                            &spec.source_var_locals,
                            &param_name,
                            idx,
                        );
                        break 'body_redo;
                    }
                    Err(e)
                        if e.message
                            == crate::runtime::Interpreter::LAZY_GATHER_TAKE_LIMIT_SIGNAL =>
                    {
                        // Save for-loop state for gather coroutine resumption.
                        // A state already in the slot belongs to a loop nested
                        // inside this body: chain it (and re-enter the CURRENT
                        // iteration so that loop can continue) instead of
                        // overwriting it. A take-suspend site directly in this
                        // body likewise re-enters the CURRENT iteration, right
                        // after the take.
                        let mut e = e;
                        let code_id = code.ops.as_ptr() as usize;
                        let nested = if self.gather_for_loop_resume.as_ref().is_some_and(|st| {
                            st.is_lexically_nested_in(code_id, body_start, loop_end)
                        }) {
                            self.gather_for_loop_resume.take()
                        } else {
                            None
                        };
                        let take_site = e.take_suspend_site().filter(|(cid, t)| {
                            *cid == code_id && *t >= body_start && *t < loop_end
                        });
                        if take_site.is_some() {
                            e.set_take_suspend_site(None);
                        }
                        let resume_body_ip = take_site.map(|(_, t)| t + 1);
                        // Keep promoted items in the continuation snapshot.  A
                        // resumed gather may have let its consumer mutate an
                        // earlier element before the producer re-enters; the
                        // source-entry guard must compare the same cell, not
                        // the old by-value snapshot that preceded promotion.
                        let mut resume_items = items.to_vec();
                        if let Some(slot) = resume_items.get_mut(idx) {
                            *slot = item.clone();
                        }
                        self.gather_for_loop_resume =
                            Some(crate::value::ForLoopResumeState::List {
                                items: resume_items,
                                next_index: if nested.is_some() || resume_body_ip.is_some() {
                                    idx
                                } else {
                                    idx + 1
                                },
                                container_binding: container_binding
                                    .clone()
                                    .map(|name| (name, container_source_slot)),
                                code_id,
                                loop_ip: body_start - 1,
                                resume_body_ip,
                                inner: nested.map(Box::new),
                            });
                        if topic_readonly {
                            self.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.unmark_readonly(name);
                        }
                        self.topic_source_var = saved_topic_source;
                        self.quanthash_bind_params = saved_quanthash_bind.clone();
                        self.restore_loop_topic(saved_topic, saved_topic_local);
                        self.pop_loop_local_scope(code);
                        self.unmask_for_params(&masked_params);
                        return Err(e);
                    }
                    Err(e) => {
                        // Unmark readonly before propagating error
                        if topic_readonly {
                            self.unmark_readonly("_");
                            self.env_mut().remove("__mutsu_deep_readonly::_");
                        }
                        if !spec.is_rw
                            && let Some(ref name) = param_name
                        {
                            self.unmark_readonly(name);
                        }
                        // Restore the loop param's prior binding on abnormal exit
                        // (`return`, an exception, an outer loop's `last`/`next`):
                        // the normal path defers this to `RestoreForParam`, which
                        // never runs when the frame unwinds past it. Leaving the
                        // final iteration value bound leaked it out of the routine
                        // via merge_method_env when the CALLER had a same-named
                        // binding (`for @!ranges -> $r { ... and return True }`
                        // inside a method clobbered the caller's `-> $r` param —
                        // Text::CSV RangeSet.in vs method CSV's gather loop).
                        if let Some((name, saved_val, colliding_slot)) = &saved_param {
                            if let Some(slot) = colliding_slot
                                && (*slot as usize) < self.locals.len()
                            {
                                self.locals[*slot as usize] =
                                    saved_val.clone().unwrap_or(Value::NIL);
                            }
                            match saved_val {
                                Some(v) => {
                                    self.env_mut().insert(name.clone(), v.clone());
                                }
                                None => {
                                    self.env_mut().remove(name);
                                }
                            }
                        }
                        self.restore_loop_topic(saved_topic.clone(), saved_topic_local.clone());
                        self.pop_loop_local_scope(code);
                        self.unmask_for_params(&masked_params);
                        return Err(e);
                    }
                }
            }
            if self.is_halted() {
                break;
            }
        }
        // Unmark readonly topic after loop completion
        if topic_readonly {
            self.unmark_readonly("_");
            self.env_mut().remove("__mutsu_deep_readonly::_");
        }
        // Unmark readonly params after loop completion
        if !spec.is_rw
            && let Some(ref name) = param_name
        {
            self.unmark_readonly(name);
        }
        // Restore saved multi-param values and readonly state
        for (name, saved_val, was_readonly, sigilless_ro, saved_local) in saved_multi_params {
            match saved_val {
                Some(v) => {
                    self.env_mut().insert(name.clone(), v);
                }
                None => {
                    self.env_mut().remove(&name);
                }
            }
            // env<->locals coherence (§1.5): a multi-param loop variable
            // (scalar or sigilless `\value`) shares its bare name — and
            // therefore its local slot — with an enclosing binding of the
            // same name whenever one exists (`build_for_bind_stmts` binds via
            // plain `Stmt::Assign`, which reuses the existing slot rather than
            // declaring a fresh one). Restoring only the `env` entry leaves
            // that local slot clobbered with the last iteration's value, so a
            // later read of the outer name (with the reverse env->locals pull
            // disabled) sees stale data — write the pre-loop LOCAL value
            // (captured directly, not derived from the possibly-absent `env`
            // snapshot) straight back into the slot.
            // `@`/`%`/`&`-sigil params are excluded: unlike a plain scalar
            // lexical (a bare value in its slot, replaced wholesale on every
            // `SetLocal`), an Array/Hash/Sub variable is a container whose
            // CONTENTS get mutated in place through the same identity —
            // reusing the shadowed outer slot means the loop body's per-
            // iteration bind mutates the very container this snapshot
            // aliases, so `saved_local`'s cloned `Value` (same Arc, already
            // mutated) would not actually restore the outer contents. Fixing
            // that needs a deep-copy/rebind strategy, tracked separately —
            // see `todo/tickets/for-multi-param-shadow-clobbers-outer-lexical.md`.
            if !name.starts_with(['@', '%', '&'])
                && let Some((slot, v)) = saved_local
            {
                self.locals[slot] = v;
            }
            self.restore_readonly(&name, was_readonly);
            let sigilless_key = format!("__mutsu_sigilless_readonly::{}", name);
            if let Some(ro_val) = sigilless_ro {
                self.env_mut().insert(sigilless_key, ro_val);
            } else {
                self.env_mut().remove(&sigilless_key);
            }
        }
        self.unmask_for_params(&masked_params);
        // Restore the enclosing type constraint each multi-param name shadowed.
        for (name, tc) in saved_multi_param_types {
            if tc.is_some() {
                self.vm_set_var_type_constraint(&name, tc);
            }
        }
        // Defer restoring the single named loop param's prior binding until
        // after the loop's LAST/post phasers have run — they must still observe
        // the param at its final iteration value (e.g.
        // `for 1,2 -> $x { LAST { say $x } }` must see 2). The paired
        // `RestoreForParam` opcode (emitted right after the post phasers) pops
        // this and applies it. Only pushed here on normal completion, which
        // keeps it balanced with that opcode; an early return/exception from the
        // body exits before this point, so no entry is pushed and the matching
        // opcode is likewise skipped as the frame unwinds.
        if let Some(entry) = saved_param {
            self.for_param_restore_stack.push(entry);
        }
        self.pop_loop_local_scope(code);
        // Slice F (env<->locals coherence, docs/env-locals-coherence.md): a
        // `.value = X` / `.value--` in the body of `for $b.pairs` over a mutable
        // QuantHash writes the new weight back to the source `$b` *by name*
        // (`quanthash_set_weight`, reached via `topic_source_var` from the lvalue
        // builtin, called with an empty `CompiledCode` because that path lacks
        // the bytecode — see methods_mut.rs). With the real loop `code` in hand,
        // write the final env value of the source straight through to its local
        // slot so the post-loop read sees the mutation without the reverse pull
        // (skipping a live `HashEntryRef` binding slot, as the reverse pull does).
        if source_mutable_quant
            && let Some(ref source) = container_binding
            && let Some(slot) = self.find_local_slot(code, source)
            && !matches!(self.locals[slot].view(), ValueView::HashEntryRef { .. })
            && let Some(val) = self.env().get(source).cloned()
        {
            self.locals[slot] = val;
        }
        self.topic_source_var = saved_topic_source;
        self.quanthash_bind_params = saved_quanthash_bind.clone();
        self.restore_loop_topic(saved_topic, saved_topic_local);
        if let Some(coll) = collected {
            let mut coll = coll;
            for (idx, name) in deferred_container_refs {
                if idx < coll.len()
                    && let Some(v) = self.get_env_with_main_alias(&name)
                {
                    coll[idx] = v;
                }
            }
            self.stack.push(Value::array(coll));
        }
        Ok(completed_all)
    }
}
