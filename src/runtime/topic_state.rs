//! The `topic` subsystem of ADR-10779: the bookkeeping around `$_` and the
//! constructs that bind it -- the topic's source variable and container (so a
//! write to `$_` reaches what it aliases), the save stacks `given`/`for`
//! push and pop, `when`'s matched flag, the smartmatch-RHS context flags, the
//! pointy-block captures of `given`, and the per-loop name scopes.

use super::*;

#[derive(Default)]
pub(crate) struct TopicState {
    /// `Box<Cell<bool>>`-backed (not a plain `bool`, and not a bare `Cell`):
    /// read/written through the `when_matched()`/`set_when_matched()`
    /// accessors below AND directly by `vm_call_state_guard::WhenMatchedGuard`,
    /// whose `Drop` impl restores it via a raw pointer into this separate heap
    /// allocation -- immune to Stacked Borrows retags of `Interpreter`'s own
    /// memory from `&mut self` calls made after the guard was constructed
    /// (see that module's doc comment for why a bare `Cell` field is not
    /// enough).
    pub(crate) when_matched: Box<Cell<bool>>,
    pub(crate) in_smartmatch_rhs: bool,
    pub(crate) transliterate_in_smartmatch: bool,
    pub(crate) substitution_in_smartmatch: bool,
    /// How many regexes with a captured `$_` are being matched right now
    /// (`install_regex_closure_scope`). While non-zero, `$_` inside the regex
    /// is that captured topic, not the match subject.
    pub(crate) regex_topic_pinned: u32,
    pub(crate) last_topic_value: Option<Value>,
    pub(crate) topic_save_stack: Vec<Value>,
    /// Current values for active pointy `given`/`with` bindings. Kept separately
    /// from `$_` so restoring the enclosing topic after the bind does not
    /// retarget the parameter's alias to the enclosing topic.
    pub(crate) given_pointy_topic_values: Vec<Value>,
    /// Saved `$_` + `topic_source_var` for a pointy-topic scope (`if COND -> $_`,
    /// `with COND -> $_`). The pointy binding introduces a FRESH lexical `$_`
    /// that shadows an enclosing `given`'s topic, so its writes must NOT flow
    /// back to the given's source variable — `EnterPointyTopic` saves + clears
    /// `topic_source_var` for the block, `ExitPointyTopic` restores it.
    pub(crate) topic_source_save_stack: Vec<(Value, Option<String>)>,
    /// The named container the current topic/loop source came from
    /// (`TagContainerRef`), paired with its compile-time-baked local slot
    /// (§1.5; `None` = non-local or runtime-derived) and the fingerprint of
    /// the `CompiledCode` that set it (`resume_code_fp`). The slot lets the
    /// for/given container writeback target the exact `locals` slot when
    /// shadow slots are active, instead of the by-name `position` search.
    /// The fingerprint scopes the signal to its own frame: the tag is always
    /// emitted immediately before the for/given op that consumes it, in the
    /// SAME code object, so consumers (`take_container_ref_for`) discard a
    /// tag whose fingerprint does not match — a leftover from a callee frame
    /// (e.g. a module method's own `for @x` loop) would otherwise be mistaken
    /// for the caller's loop source and its slot would index the WRONG frame's
    /// locals (Text::CSV t/90_csv.t 507-508: `method CSV`'s `@in` tag, slot 28
    /// in the method frame, made the caller's untagged `for in () -> $in` loop
    /// write its items over the mainline's slot 28).
    pub(crate) container_ref_var: Option<(String, Option<u32>, usize)>,
    pub(crate) container_ref_reversed: bool,
    pub(crate) topic_source_var: Option<String>,
    /// The `@`/`%` source variable when `$_` is a whole-container topic
    /// (`given @a` / `with %h`), where `$_` aliases the entire container. A `.=`
    /// metaop on the topic (`TopicDotAssign`) writes the reassigned `$_` straight
    /// through to this source with container-assignment coercion. Distinct from
    /// `topic_source_var`, which a `for @a` element loop also sets but where `$_`
    /// is a single element (handled by the per-element writeback, not this).
    pub(crate) topic_container_source: Option<String>,
    pub(crate) element_source: Option<(String, Vec<(Value, bool)>)>,
    pub(crate) quanthash_bind_params: Vec<String>,
    /// Deferred restore of a single named for-loop param's prior binding, applied
    /// by `RestoreForParam` after the loop's LAST/post phasers. Tuple is
    /// `(name, saved_env_value, colliding_local_slot)`: the slot is `Some` when
    /// the loop param shares a compile-time local slot with an enclosing binding
    /// of the same bare name (`my \x = 10; for ... -> \x { }`), so the restore
    /// must write the saved value back through that slot too — otherwise a later
    /// `GetLocal` read of the outer name sees the loop's last iteration value.
    pub(crate) for_param_restore_stack: Vec<(String, Option<Value>, Option<u32>)>,
    /// Local-frame slot indices of `given`/`with` pointy-topic parameters
    /// (`given EXPR -> $v {...}`) currently mid-writeback: the enclosing
    /// `Given`/`With` op still needs the slot's final value after its body
    /// finishes. The pointy param's own `VarDecl` makes
    /// `exec_block_local_scope_op` treat it as an ordinary vanishing
    /// block-local `my`, Nil-ing its slot on block exit (and, when the name
    /// shadows an outer variable, `pop_loop_local_scope` may instead
    /// overwrite the slot with the outer binding's restored value) — both of
    /// which run BEFORE the enclosing op's writeback can read it, and a
    /// scalar pointy param's live value has NO other home by then (a plain
    /// scalar lexical skips its env mirror under the `(B)` per-store
    /// env-write gate, see `docs/lexical-scope-slot-campaign.md`). So
    /// `exec_block_local_scope_op` captures each protected slot's live value
    /// into `given_pointy_captured` unconditionally, right after body
    /// execution finishes and before either of those two exit paths can
    /// touch it.
    ///
    /// Keyed by exact SLOT index, not by name/symbol: two nested `given`s can
    /// bind the SAME name (`given $a -> $v { given $b -> $v {...} }`), each
    /// getting its own distinct compiled slot under shadow slots, and a
    /// pointy param can also shadow an outer variable of the same name
    /// (`given 5 -> $x {...}` inside `my $x = 1`) — slot identity is the only
    /// thing that disambiguates either case; name-based matching captured
    /// from (or reset) the wrong declaration's slot in both. `exec_given_op`
    /// determines its own pointy param's slot by peeking the compiled body
    /// for the first `SetLocalDecl`, which is always that param's own
    /// synthetic declaration (`pointy_topic_bind` always inserts it as the
    /// body's first statement) — found before any nested construct's own
    /// declarations, so it is unambiguous even under same-name nesting.
    pub(crate) given_pointy_capture_slots: Vec<usize>,
    /// Parallel stack to `given_pointy_capture_slots`: the captured final value for
    /// each active `given`/`with` pointy param's slot, filled in by
    /// `exec_block_local_scope_op` (`None` until then) and consumed by
    /// `exec_given_op`'s writeback.
    pub(crate) given_pointy_captured: Vec<Option<Value>>,
    pub(crate) loop_local_vars: ScopeStack<NameSet>,
    /// Names currently bound as for-loop parameters in this frame chain, one
    /// set per active loop (ADR-0023). Bare names (no `$` sigil), matching
    /// env keys. Consulted by `block_captured_scalars` only; never persisted.
    pub(crate) active_loop_param_names: ScopeStack<rustc_hash::FxHashSet<String>>,
    /// Parallel to [`Self::active_loop_param_names`], for the parameters that
    /// **alias** rather than copy: the bare names the enclosing `for` loops
    /// currently bind as genuinely rw parameters (`is rw`, a `<->` block, a
    /// sigilless `\v`, a `.kv` value slot).
    ///
    /// An rw parameter is the source element's own container, so a closure over
    /// it reads *through* it and a later write to the element is visible
    /// (`for @a -> $x is rw, $y is rw { $c = -> { $x } }; @a[0] = 99; $c()` is
    /// `99`). `freeze_readonly_owned_captures` consults this to leave such a
    /// name alone: a MULTI-parameter loop binds through
    /// `build_for_bind_stmts`' declaration prefix, which registers the name as
    /// loop-local, and the freeze would otherwise deep-deref the element cell
    /// into a per-iteration snapshot. A single-parameter rw loop binds natively
    /// and never registers, so it was always right -- this is what makes the two
    /// forms agree.
    ///
    /// Runtime-scoped, not a per-`CompiledCode` name set: names are reused
    /// across the loops of one compilation unit, so a compile-time set would let
    /// one loop's `is rw` exempt an unrelated later loop's same-named *non-rw*
    /// parameter (measured: `t/for-loop-element-alias.t`'s per-iteration
    /// identity rows).
    pub(crate) active_loop_rw_param_names: ScopeStack<rustc_hash::FxHashSet<String>>,
    /// Per loop-body scope: what each body-local `my` name must be restored to
    /// when the loop exits. `Some(v)` is a genuine shadow (re-expose the outer
    /// binding's value); `None` means the name did not exist before the loop, so
    /// the entry must be REMOVED — otherwise a body-local `my` outlives its block
    /// as an env key, which is how `HTTP::HPACK`'s Huffman-table `my int $i`
    /// stayed visible process-wide and was later merged over an unrelated frame's
    /// loop variable.
    pub(crate) loop_local_saved_env: ScopeStack<rustc_hash::FxHashMap<crate::symbol::Symbol, Option<Value>>>,
    pub(crate) loop_cond_active: bool,
}

impl TopicState {
    /// A spawned thread starts with no topic binding, loop or `given` in
    /// progress (as `clone_for_thread` did field by field).
    // Cost: O(1).
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self::default()
    }
}
