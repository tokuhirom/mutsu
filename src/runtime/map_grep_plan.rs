//! The loop-invariant half of an inline `.map`/`.grep` loop (#10187).
//!
//! The inline loops (`eval_map_over_items`, `eval_map_over_items_rw`,
//! `eval_grep_over_items_with_mutated`) run a callback's body through
//! `run_reuse` instead of a full closure call. Before the first element they
//! compile the body, classify every captured name (does the capture overwrite
//! the consuming frame's binding, or yield to it?) and list every name the
//! loop binds or the body declares, so all of them can be put back afterwards.
//! None of that depends on the elements, and almost none of it on the frame
//! doing the consuming.
//!
//! A deferred `.map`/`.grep` consumed by a `for` loop or an `Iterator` runs
//! that loop once per element (a one-element prefix pull, #9936/#10186), so
//! the per-loop setup used to be paid per element: ~3x the per-element cost
//! of the bulk loop. [`InlineLoopPlan`] is that setup, computed once and kept
//! in the Seq's [`MapGrepPlanSlot`]; each pull then pays only the env swap
//! ([`Interpreter::enter_inline_loop_env`] / [`Interpreter::leave_inline_loop_env`]),
//! the bind and the body. A bulk loop builds the same plan in a throwaway
//! slot, so both drive one implementation.
//!
//! Only the part that is a pure function of the callback is cached. What the
//! *consuming* frame contributes — whether it already holds a captured name,
//! its current topic — is read again on every pull, because a stream can be
//! pulled from different frames (an `Iterator` handed to another routine).
//! The one ambient compiler input, `lexically_in_routine`, is part of the
//! plan's key.

use super::*;
use crate::opcode::{CompiledCode, CompiledFns};
use crate::symbol::Symbol;
use crate::value::SubData;
use std::sync::Arc;

/// Which inline loop a plan was built for: the three differ in the
/// temporaries they bind.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum InlineLoopKind {
    Map,
    MapRw,
    Grep,
    First,
}

/// One name in the callback's captured env.
#[derive(Clone, Copy)]
struct CaptureKey {
    name: Symbol,
    /// The capture overwrites the consuming frame's binding of the same name
    /// (`capture_wins_over_caller`, or `self`); otherwise it is installed only
    /// where the consuming frame has no such binding.
    overwrites: bool,
    /// The name is also in [`InlineLoopPlan::temporaries`], which already
    /// saved it.
    is_temporary: bool,
}

/// The loop-invariant setup of an inline map/grep loop.
pub(crate) struct InlineLoopPlan {
    kind: InlineLoopKind,
    /// Address of the callback's `SubData`. The Seq that owns the slot keeps
    /// the callback alive, so the address cannot be reused while the plan is.
    origin: usize,
    lexically_in_routine: bool,
    /// The tail-normalized body, compiled (see `compile_loop_block_cached`).
    pub(crate) code: Arc<CompiledCode>,
    pub(crate) fns: Arc<CompiledFns>,
    /// `data.params`, interned, for the per-element bind.
    pub(crate) param_syms: Vec<Symbol>,
    /// See `block_keeps_outer_topic`.
    pub(crate) keeps_outer_topic: bool,
    /// See `CompiledCode::immutable_topic` / `set_loop_topic_readonly`.
    pub(crate) immutable_topic: bool,
    /// See `frame_authoritative_set`.
    pub(crate) block_authoritative: Vec<Symbol>,
    /// Names the loop binds per iteration or the body declares: the params,
    /// `_`/`$_`, grep's topic-source key, the body's own `my` names. Saved
    /// before the capture merge, restored afterwards.
    temporaries: Vec<Symbol>,
    captures: Vec<CaptureKey>,
}

/// The consuming frame's bindings an inline loop displaced, put back by
/// [`Interpreter::leave_inline_loop_env`].
pub(crate) struct SavedLoopBindings(Vec<(Symbol, Option<Value>)>);

/// Where a deferred `.map`/`.grep` keeps what it computed about its callback
/// across pulls (`SeqSource::MapGrep::plan`). Empty until the first pull; a
/// bulk loop uses a fresh one.
#[derive(Clone, Default)]
pub(crate) struct MapGrepPlanSlot {
    inline: Option<Arc<InlineLoopPlan>>,
    /// Whether the callback can take the native rw map loop at all
    /// (`native_rw_map_block_shape`), keyed by the callback's address.
    native_rw_candidate: Option<(usize, bool)>,
    /// Whether the Seq can be pulled a chunk at a time
    /// (`map_grep_pullable_by_prefix`) — a function of the Seq's callback and
    /// mode, which never change.
    prefix_pullable: Option<bool>,
    /// The package the callback runs under when a pull happens elsewhere
    /// (`run_map_grep_chunk`), likewise fixed for the Seq.
    callback_package: Option<Option<Symbol>>,
}

/// The address a plan is keyed by.
// Cost: O(1).
pub(crate) fn sub_origin(data: &SubData) -> usize {
    std::ptr::from_ref(data) as usize
}

/// Elements an inline map/grep loop binds per call: the positional params
/// not supplied by `.assuming`, at least one.
// Cost: O(1).
pub(crate) fn inline_loop_arity(data: &SubData) -> usize {
    data.params
        .len()
        .saturating_sub(data.assumed_positional.len())
        .max(1)
}

impl MapGrepPlanSlot {
    /// The cached prefix-pullability verdict, or `compute`'s answer, cached.
    /// Only a Seq's own slot may use this: the verdict is not keyed.
    // Cost: O(1) on a hit; `compute` otherwise.
    pub(crate) fn prefix_pullable(&mut self, compute: impl FnOnce() -> bool) -> bool {
        *self.prefix_pullable.get_or_insert_with(compute)
    }

    /// The cached callback package, or `compute`'s answer, cached. Only a
    /// Seq's own slot may use this: the answer is not keyed.
    // Cost: O(1) on a hit; `compute` otherwise.
    pub(crate) fn callback_package(
        &mut self,
        compute: impl FnOnce() -> Option<Symbol>,
    ) -> Option<Symbol> {
        *self.callback_package.get_or_insert_with(compute)
    }

    /// The cached native-rw-loop verdict for the callback at `origin`, or
    /// `compute`'s answer, cached.
    // Cost: O(1) on a hit; `compute` otherwise.
    pub(crate) fn native_rw_candidate(
        &mut self,
        origin: usize,
        compute: impl FnOnce() -> bool,
    ) -> bool {
        match self.native_rw_candidate {
            Some((o, verdict)) if o == origin => verdict,
            _ => {
                let verdict = compute();
                self.native_rw_candidate = Some((origin, verdict));
                verdict
            }
        }
    }
}

impl Interpreter {
    /// The plan for running `data` through the inline loop `kind`: the one in
    /// `slot` when it was built for this callback in this compile context,
    /// otherwise a new one, stored in `slot`.
    // Cost: O(1) on a hit; otherwise one body compile (cached across calls by
    // `compile_loop_block_cached`) plus O(c + d), c = captured names, d =
    // names the body declares.
    pub(crate) fn inline_loop_plan(
        &mut self,
        data: &SubData,
        kind: InlineLoopKind,
        slot: &mut MapGrepPlanSlot,
    ) -> Arc<InlineLoopPlan> {
        if let Some(plan) = self.cached_inline_loop_plan(data, kind, slot) {
            return plan;
        }
        let origin = sub_origin(data);
        let lexically_in_routine = !self.routine_stack.is_empty();
        let plan = Arc::new(self.build_inline_loop_plan(data, kind, origin, lexically_in_routine));
        slot.inline = Some(plan.clone());
        plan
    }

    /// The plan in `slot`, when it was built for `data` and `kind` in the
    /// current compile context. Its presence also answers the loops' routing
    /// question — only a callback the inline loop accepts ever gets a plan —
    /// so a loop that finds one skips its call-path checks.
    // Cost: O(1).
    pub(crate) fn cached_inline_loop_plan(
        &self,
        data: &SubData,
        kind: InlineLoopKind,
        slot: &MapGrepPlanSlot,
    ) -> Option<Arc<InlineLoopPlan>> {
        let plan = slot.inline.as_ref()?;
        (plan.origin == sub_origin(data)
            && plan.kind == kind
            && plan.lexically_in_routine != self.routine_stack.is_empty())
        .then(|| plan.clone())
    }

    // Cost: see `inline_loop_plan`.
    fn build_inline_loop_plan(
        &mut self,
        data: &SubData,
        kind: InlineLoopKind,
        origin: usize,
        lexically_in_routine: bool,
    ) -> InlineLoopPlan {
        let (code, fns) = self.compile_loop_block_cached(data);
        let param_syms: Vec<Symbol> = data.params.iter().map(|p| Symbol::intern(p)).collect();
        let mut temporaries: Vec<Symbol> = Vec::with_capacity(param_syms.len() + 3);
        let push = |temporaries: &mut Vec<Symbol>, k: Symbol| {
            if !temporaries.contains(&k) {
                temporaries.push(k);
            }
        };
        for &p in &param_syms {
            push(&mut temporaries, p);
        }
        // A sigilless param (`-> \x`) is marked by a top-level
        // `MarkSigillessReadonly` statement in the body (a simple pointy block
        // carries no `param_defs` to say so), which writes its readonly marker
        // into the consuming frame's env. Left behind, the marker made a later
        // `$x is rw` parameter of an unrelated call read as readonly (#11429).
        for stmt in data.body.iter() {
            if let crate::ast::Stmt::MarkSigillessReadonly(name) = stmt {
                push(
                    &mut temporaries,
                    crate::runtime::sigilless_readonly_key(name),
                );
            }
        }
        // An `is rw`/`is raw` scalar param shares its bare name with a live
        // enclosing sigilless `\x`; the loop drops that marker for the param's
        // own scope (`eval_map_over_items_rw`), so it is restored on exit
        // (#11700).
        for pd in data.param_defs.iter() {
            if pd.traits.iter().any(|t| t == "rw" || t == "raw") {
                push(
                    &mut temporaries,
                    crate::runtime::sigilless_readonly_key(&pd.name),
                );
            }
        }
        push(&mut temporaries, crate::symbol::wk::topic());
        push(&mut temporaries, crate::symbol::wk::topic_sigiled());
        if kind == InlineLoopKind::Grep {
            push(&mut temporaries, crate::symbol::wk::grep_topic_source());
        }
        // The body's own `my` names run in the consuming frame's env, so an
        // enclosing lexical of the same name would be clobbered on exit (zef's
        // `provides-spec-matcher` clobbered the calling method's `$spec`
        // param this way). A
        // declared name that is also a free var refers to the outer binding
        // (used before its declaration) and is left alone. A `my $*x`
        // redeclaration is block-private too (roast S32-io/indir.t's
        // `my $*CWD = $_` in a `.map`), and gets no free-var exemption:
        // dynamic reads compile to by-name lookups, so a declared-and-read
        // dynamic always registers as a free var.
        for &k in &code.my_declared_sym {
            if !code.free_var_syms.contains(&k) {
                push(&mut temporaries, k);
            }
        }
        for &k in &code.dynamic_declared_sym {
            push(&mut temporaries, k);
        }
        let free_vars = data
            .compiled_code
            .as_ref()
            .map(|cc| cc.capture_free_var_set());
        let self_sym = crate::symbol::wk::self_();
        let captures = data
            .env
            .iter()
            .map(|(k, v)| CaptureKey {
                name: *k,
                overwrites: *k == self_sym
                    || super::resolution_map_grep::capture_wins_over_caller(free_vars, k, v),
                is_temporary: temporaries.contains(k),
            })
            .collect();
        let keeps_outer_topic = super::resolution_map_grep::block_keeps_outer_topic(data);
        InlineLoopPlan {
            kind,
            origin,
            lexically_in_routine,
            code,
            fns,
            param_syms,
            keeps_outer_topic,
            immutable_topic: !keeps_outer_topic
                && data
                    .compiled_code
                    .as_ref()
                    .is_some_and(|cc| cc.immutable_topic),
            block_authoritative: data
                .compiled_code
                .as_ref()
                .map(|cc| {
                    super::resolution_map_grep::frame_authoritative_set(
                        cc,
                        &data.authoritative_captures,
                        &data.own_cell_captures,
                    )
                })
                .unwrap_or_default(),
            temporaries,
            captures,
        }
    }

    /// Merge the callback's captured env into the running env for an inline
    /// loop, saving every binding the loop is about to displace.
    ///
    /// Caller priority by default, with the exceptions every other closure-env
    /// merge makes (`call_sub_value`'s merge, `call_compiled_closure_with_topic`;
    /// the list is `capture_wins_over_caller`):
    ///
    ///  - `self` is lexical — the block's captured invocant wins.
    ///  - a captured `ContainerRef` is a shared container cell
    ///    (box-on-capture, ADR-0025/ADR-0055), the single source of truth for
    ///    its lexical; yielding to the caller silently resolved the closure's
    ///    own free variable to an unrelated same-named caller lexical
    ///    (ADR-0055 §1.2(b), through the `.map($f)` path). A dynamic variable
    ///    (`$*x`) keeps caller priority — it is dynamic-scope by design.
    ///  - a captured value for one of the block's own free variables wins for
    ///    the same reason: the binding it names is the one at the block's
    ///    creation site, never a same-named lexical live in the frame doing
    ///    the consuming. The cell rule alone covered only the `$`-scalars
    ///    `box_captured_lexicals` boxes, so an `@`/`%` free variable still
    ///    resolved to the caller's — visible whenever the pull happens in
    ///    another frame, which ADR-0058 made the normal case: `sub mk(@p) {
    ///    [1].map({ @p.elems }) }` consumed inside a routine with its own `@p`
    ///    read the consumer's, and in a recursive producer the callback read an
    ///    outer invocation's parameter, so the recursion never reached its base
    ///    case (the stack overflow that aborted
    ///    `roast/integration/99problems-21-to-30.t` when ADR-0058 step 3 landed).
    ///
    /// Every key the merge overwrites is saved, not just the ones it
    /// introduces: a nested map that overwrote a name the enclosing map had
    /// installed would otherwise leave its own value behind for the enclosing
    /// map's next iteration (`sub inner(@sizes) { map -> $e { map -> $g {...},
    /// inner(@sizes[1..*]) }, ['a','b'] }` read `@sizes` as the inner call's on
    /// iteration 2).
    // Cost: O(t + c), t = plan temporaries, c = captured names.
    pub(crate) fn enter_inline_loop_env(
        &mut self,
        data: &SubData,
        plan: &InlineLoopPlan,
    ) -> SavedLoopBindings {
        let mut saved = Vec::with_capacity(plan.temporaries.len() + plan.captures.len());
        for &k in &plan.temporaries {
            saved.push((k, self.env.get_sym(k).cloned()));
        }
        for c in &plan.captures {
            let current = if c.overwrites {
                self.env.get_sym(c.name).cloned()
            } else if self.env.contains_key_sym(c.name) {
                // Caller priority: nothing to install, nothing to save.
                continue;
            } else {
                None
            };
            let Some(v) = data.env.get_sym(c.name) else {
                continue;
            };
            // A body that assigns a captured name the consuming frame holds
            // as the very same binding (`current` identical to the capture, the
            // creator frame consuming its own closure) must leave its write in
            // the env: restoring `current` would drop it for a variable with no
            // cell to carry it (an env-only pointy/for parameter, `-> $a is
            // copy { (0..1).map({ $a += 10 }); $a }`). When the two differ the
            // consuming frame's binding is an unrelated lexical and is restored.
            let same_binding = c.overwrites
                && plan.code.free_var_writes.contains(&c.name)
                && current
                    .as_ref()
                    .is_some_and(|cur| crate::runtime::utils::container_identity_identical(cur, v));
            if !c.is_temporary && !same_binding {
                saved.push((c.name, current));
            }
            self.env.insert_sym(c.name, v.clone());
        }
        SavedLoopBindings(saved)
    }

    /// Put back what [`Self::enter_inline_loop_env`] displaced.
    // Cost: O(s), s = saved bindings.
    pub(crate) fn leave_inline_loop_env(&mut self, saved: SavedLoopBindings) {
        for (k, orig) in saved.0 {
            match orig {
                Some(v) => {
                    self.env.insert_sym(k, v);
                }
                None => {
                    self.env.remove_sym(k);
                }
            }
        }
    }
}
