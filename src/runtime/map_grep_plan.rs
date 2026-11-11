//! The loop-invariant half of an inline `.map`/`.grep` loop (#10187).
//!
//! The inline loops (`eval_map_over_items`, `eval_map_over_items_rw_into`,
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
use crate::opcode::{CompiledCode, CompiledFns, OpCode};
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
    /// The topic's writability is decided per ELEMENT
    /// ([`super::resolution_map_grep::set_loop_topic_readonly`]): the source
    /// is a `List` value and no call site spoke for the callback, so an
    /// element that is a container (`($x, $y).map(&f)`) keeps `$_` writable
    /// while a bare one (`(1, 2)` reaching `.map` through a parameter, #16949)
    /// makes it immutable.
    pub(crate) topic_per_element: bool,
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
pub(crate) struct SavedLoopBindings(Vec<(Symbol, Option<Value>)>, Option<usize>);

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
    /// A lazy list a `.map` callback slipped that cannot be seen to end
    /// (#14295), with the index of its next element: the next prefix pull
    /// reads it before it calls the callback again. A Seq's own slot only.
    pub(crate) pending_slip: Option<(Value, usize)>,
    /// Whether the chunk being run is a prefix pull, which keeps a slipped
    /// lazy list in `pending_slip`; any other loop reads it to its end.
    pub(crate) stream_slips: bool,
    /// Whether the callback's `FIRST` phaser has run for this Seq, and its
    /// `LAST` (#14196): a prefix pull runs the loop once per pull, but the
    /// phasers belong to the whole iteration.
    first_fired: bool,
    last_fired: bool,
    /// Set by a chained pull around a chunk that does not reach the end of
    /// its upstream: running out of the chunk's elements is not the end, so
    /// `LAST` must wait (#14196).
    pub(crate) chunk_open: bool,
    /// The arguments of the last call of a signature-bearing map block, which
    /// its `LAST` phaser runs with. A pull that finds the source already read
    /// to its end has no call of its own to take them from (#14196).
    pub(crate) last_call_args: Option<Vec<Value>>,
    /// The Seq's source is a `List` value (not an `Array`), so a plain
    /// callback's implicit `$_` is writable only where the element itself is
    /// a container, even when the callback arrived as a value
    /// (`$list.map(&f)`); set where the receiver is known
    /// (`dispatch_map_method`) and turned into
    /// `InlineLoopPlan::topic_per_element`.
    pub(crate) immutable_source: bool,
}

impl MapGrepPlanSlot {
    /// An empty slot for a Seq whose source is `immutable` (an immutable
    /// `List` value); see [`MapGrepPlanSlot::immutable_source`].
    // Cost: O(1).
    pub(crate) fn for_source(immutable: bool) -> Self {
        Self {
            immutable_source: immutable,
            ..Self::default()
        }
    }
}

/// The address a plan is keyed by.
// Cost: O(1).
pub(crate) fn sub_origin(data: &SubData) -> usize {
    std::ptr::from_ref(data) as usize
}

/// Elements an inline map/grep loop binds per call: the positional params
/// not supplied by `.assuming`, a slurpy parameter not counted, at least one.
// Cost: O(1).
pub(crate) fn inline_loop_arity(data: &SubData) -> usize {
    let slurpies = data.param_defs.iter().filter(|pd| pd.slurpy).count();
    data.params
        .len()
        .saturating_sub(slurpies)
        .saturating_sub(data.assumed_positional.len())
        .max(1)
}

impl MapGrepPlanSlot {
    /// Whether the loop about to run must fire the callback's `FIRST`: once
    /// per Seq, when its first element is about to be read.
    // Cost: O(1).
    pub(crate) fn take_first_phaser(&mut self, has_element: bool) -> bool {
        let fire = has_element && !self.first_fired;
        self.first_fired |= fire;
        fire
    }

    /// Whether `FIRST` ran and `LAST` has not: a pull that finds the source
    /// already read to its end runs the loop once more to fire it.
    // Cost: O(1).
    pub(crate) fn last_phaser_due(&self) -> bool {
        self.first_fired && !self.last_fired
    }

    /// Whether the loop that just ended must fire the callback's `LAST`: once
    /// per Seq, when the iteration ended (`ended`: the source ran out or the
    /// callback said `last`) after `FIRST` ran.
    // Cost: O(1).
    pub(crate) fn take_last_phaser(&mut self, ended: bool) -> bool {
        let fire = ended && self.first_fired && !self.last_fired;
        self.last_fired |= fire;
        fire
    }

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
        let mut plan = self.build_inline_loop_plan(data, kind, origin, lexically_in_routine);
        let site_decided = data
            .compiled_code
            .as_ref()
            .is_some_and(|cc| cc.topic_site_decided);
        plan.topic_per_element =
            slot.immutable_source && !plan.keeps_outer_topic && !site_decided;
        let plan = Arc::new(plan);
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
        let param_syms: Vec<Symbol> = data.params.iter().map(|p| data.param_storage_sym(p)).collect();
        let mut temporaries: Vec<Symbol> = Vec::with_capacity(param_syms.len() + 3);
        let push = |temporaries: &mut Vec<Symbol>, k: Symbol| {
            if !temporaries.contains(&k) {
                temporaries.push(k);
            }
        };
        for &p in &param_syms {
            push(&mut temporaries, p);
        }
        // A sigilless param (`-> \x`, stored under its term key) has its
        // readonly marker written into the consuming frame's env on entry. Left
        // behind, the marker made a later `$x is rw` parameter of an unrelated
        // call read as readonly (#11429).
        for p in data.params.iter() {
            if crate::runtime::term_names::term_spelling(p).is_some() {
                push(&mut temporaries, crate::runtime::sigilless_readonly_key(p));
            }
        }
        // An `is rw`/`is raw` scalar param shares its bare name with a live
        // enclosing sigilless `\x`; the loop drops that marker for the param's
        // own scope (`eval_map_over_items_rw_into`), so it is restored on exit
        // (#11700).
        for pd in data.param_defs.iter() {
            if pd.traits.iter().any(|t| t == "rw" || t == "raw") {
                push(
                    &mut temporaries,
                    crate::runtime::sigilless_readonly_key(&pd.storage_name()),
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
        let body_names_only_what_it_mentions = !code.static_name_lookup
            && !code.ops.iter().any(|op| {
                matches!(
                    op,
                    OpCode::RegisterDecl(..)
                        | OpCode::RoutineScope { .. }
                        | OpCode::PackageScope { .. }
                        | OpCode::RegisterSubset { .. }
                        | OpCode::MakeAnonSub(..)
                        | OpCode::MakeAnonSubParams(..)
                        | OpCode::MakeLambda(..)
                        | OpCode::MakeBlockClosure(..)
                        | OpCode::MakeGather(..)
                )
            });
        let captures = data
            .env
            .iter()
            .filter(|(k, _)| {
                // A captured type or enum name the body never mentions is
                // not worth installing in the consuming frame and putting
                // back again for every element: the program's own names
                // (`CookieName`, `Path`, ...) are in every closure's capture
                // once a module is loaded, and each cost a probe, an insert
                // and a remove per call (`@a.map(* * 2).grep(* > 4)`: 4x the
                // bare program with Cro loaded, #7817). Only a body that
                // cannot reach a name any other way qualifies: no nested
                // closure or routine, no by-name lookup.
                // A shared cell or a captured free variable wins over the
                // consuming frame's binding, but a name the body does not
                // mention (`mentions_name` covers its free variables) cannot be
                // read there, so that rule has nothing to decide for it.
                !(body_names_only_what_it_mentions
                    && !temporaries.contains(k)
                    && droppable_subject(k).is_some_and(|subject| {
                        !code.mentions_name(k) && !code.mentions_name(&subject)
                    }))
            })
            .map(|(k, v)| CaptureKey {
                name: *k,
                overwrites: super::resolution_map_grep::capture_wins_over_caller(free_vars, k, v),
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
            topic_per_element: false,
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
    // Cost: O(t + c), t = plan temporaries, c = captured names the body can
    // name (a type or enum name it never mentions is not in the plan).
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
        // The callback's `self` and `?CLASS` are its creator's (ADR-14843).
        let fixtures_mark = data.fixtures.clone().map(|f| {
            let mark = self.routine_stack.fixtures_len();
            self.routine_stack.push_scoped_fixtures(f);
            mark
        });
        SavedLoopBindings(saved, fixtures_mark)
    }

    /// Put back what [`Self::enter_inline_loop_env`] displaced.
    // Cost: O(s), s = saved bindings.
    pub(crate) fn leave_inline_loop_env(&mut self, saved: SavedLoopBindings) {
        if let Some(mark) = saved.1 {
            self.routine_stack.truncate_fixtures(mark);
        }
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

/// The name a captured key stands for when it is one a body can only reach by
/// naming it: a type, enum or constant name (`Pt`, `Cro::HTTP`), a sigilless
/// term (`\size`) or the readonly marker of one
/// (`__mutsu_sigilless_readonly::\size`), a lexical type's own key
/// (`\u{1}Foo`) or a `state` variable's metadata key
/// (`__mutsu_state_key::<name>`, read only for a free variable `<name>`).
/// `None` for a sigiled variable, a `&` code variable, a dynamic, any other
/// internal `__mutsu_*` key and a `?`/`=` special: those are read by machinery
/// that does not name them.
// Cost: O(1) after the first call per symbol (a memo probe); the first call
// is O(len).
fn droppable_subject(k: &Symbol) -> Option<Symbol> {
    // A pure function of the name's text, asked about every captured name each
    // time a Seq builds its plan: with a module loaded that is ~60 names per
    // `.map`/`.grep` call, so it is decided once per symbol (#7817).
    thread_local! {
        static MEMO: std::cell::RefCell<rustc_hash::FxHashMap<Symbol, Option<Symbol>>> =
            std::cell::RefCell::new(rustc_hash::FxHashMap::default());
    }
    if let Some(hit) = MEMO.with(|m| m.borrow().get(k).copied()) {
        return hit;
    }
    let subject = droppable_subject_uncached(k);
    MEMO.with(|m| m.borrow_mut().insert(*k, subject));
    subject
}

// Cost: O(len).
fn droppable_subject_uncached(k: &Symbol) -> Option<Symbol> {
    k.with_str(|s| {
        let term = s.strip_prefix("__mutsu_sigilless_readonly::").unwrap_or(s);
        let ident_byte = |b: u8| b.is_ascii_alphanumeric() || b == b'_' || b == b'-';
        let ident = |t: &str| !t.is_empty() && t.bytes().all(ident_byte);
        // A lexical type's own key (`\u{1}Foo`, `crate::term_names::lexical_type_key`)
        // is consulted only by a reader that names the type `Foo`.
        if let Some(name) = s.strip_prefix(crate::term_names::LEXICAL_TYPE_PREFIX) {
            return ident(name).then(|| Symbol::intern(name));
        }
        // A `state` variable's metadata key (`__mutsu_state_key::<name>`) is read
        // only for a free variable `<name>` of the running body.
        if let Some(name) = s.strip_prefix("__mutsu_state_key::") {
            return (!name.is_empty()).then(|| Symbol::intern(name));
        }
        if let Some(name) = term.strip_prefix('\\') {
            return ident(name).then(|| Symbol::intern(name));
        }
        // A package-qualified routine binding (`&Cro::Uri::encode-percents`) is
        // reached by its own name or, from inside its package, by the bare
        // tail; the tail stands for it.
        if let Some(qualified) = term.strip_prefix('&') {
            if !qualified.bytes().all(|b| ident_byte(b) || b == b':') {
                return None;
            }
            let (_, tail) = crate::qualified::split_qualified(Symbol::intern(qualified))?;
            return tail.with_str(ident).then_some(tail);
        }
        // Only a type-shaped name qualifies unmarked; a readonly marker is
        // always for a sigilless term.
        let type_like = term.as_bytes().first().is_some_and(u8::is_ascii_uppercase)
            && term
                .bytes()
                .all(|b| b.is_ascii_alphanumeric() || b == b'_' || b == b':');
        (std::ptr::eq(term, s) && type_like).then_some(*k)
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn droppable_subject_memo_agrees_with_the_classification() {
        for name in [
            "CookieName",
            "Cro::HTTP::Header",
            "\\size",
            "__mutsu_sigilless_readonly::\\size",
            "&Cro::Uri::encode-percents",
            "$x",
            "@a",
            "?FILE",
            "__mutsu_state_key::__ANON_STATE#1__",
        ] {
            let k = Symbol::intern(name);
            let expected = droppable_subject_uncached(&k);
            // Twice: the first call fills the memo, the second reads it.
            assert_eq!(droppable_subject(&k), expected, "{name}");
            assert_eq!(droppable_subject(&k), expected, "{name} (memoized)");
        }
        assert!(droppable_subject(&Symbol::intern("CookieName")).is_some());
        assert!(droppable_subject(&Symbol::intern("\u{1}Stream")).is_some());
        assert!(
            droppable_subject(&Symbol::intern("__mutsu_state_key::__ANON_STATE#11__")).is_some()
        );
        assert!(droppable_subject(&Symbol::intern("$x")).is_none());
    }
}
