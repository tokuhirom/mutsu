//! The `dispatch` subsystem of ADR-10779: the state of sub/method dispatch
//! that is not a cache -- the multi/proto/method/metamodel/wrap/samewith
//! frame stacks, the `.wrap` chains and handles, the user-declared operator
//! tables, the function-key indexes the resolver walks, and the per-call
//! dispatch flags.

use super::*;

#[derive(Default)]
pub(crate) struct DispatchState {
    pub(crate) operator_assoc: std::sync::Arc<HashMap<String, String>>,
    /// Short-form infix operator sub names (`infix:<+>`, ...) that have ever
    /// been user-declared, regardless of package/associativity. Consulted as a
    /// cheap guard by the VM's native-arithmetic fast paths (`exec_add_op` and
    /// friends) so they only pay for a full multi-dispatch resolution lookup
    /// when a user override could plausibly exist, keeping the common
    /// no-override hot path (e.g. tight `Int + Int` loops) free of registry
    /// lookups.
    /// Each entry maps the operator name to the compilation units that declared
    /// it (`?FILE` at declaration time) or imported it with a `use`. An EMPTY
    /// file set means "provenance unknown, visible everywhere". An exported
    /// operator is lexically visible in the unit that imported it, and in no
    /// other unit (#9944), so an import records the importing unit here.
    ///
    /// The file set is what makes operator scoping lexical rather than dynamic:
    /// a `sub infix:<+>` declared in the main script must not override
    /// arithmetic inside an imported module (Test.rakumod's own counter
    /// arithmetic is the motivating case), and must still apply inside a
    /// main-script block even when a module routine is what invokes that block.
    /// See `Interpreter::user_infix_override`.
    pub(crate) user_declared_infix_ops: std::sync::Arc<HashMap<String, HashSet<Symbol>>>,
    /// Active proto bodies a `{*}` may dispatch from (#10746).
    pub(crate) proto_dispatch_stack: Vec<ProtoDispatchFrame>,
    /// How many method calls are in progress (the VM's method-call opcodes,
    /// and a proto's `{*}` running its winning candidate). A method has a
    /// dispatcher of its own, so a `{*}` reached through one does not reach an
    /// enclosing proto body: `ProtoDispatchFrame::method_depth` records the
    /// depth a proto body runs at.
    pub(crate) method_call_depth: u32,
    /// One-shot suppression of the user `postcircumfix:<[ ]>`/`<{ }>`
    /// multi-candidate probe in `exec_index_op_with_positional`. Set only
    /// while the *core* subscript routine (`builtin_postcircumfix_subscript`,
    /// what `&postcircumfix:<[ ]>` resolves to) drives that op: real Raku's
    /// CORE candidate performs native indexing and never re-enters the
    /// user's override, so a delegating candidate (`old-same SELF, $index`,
    /// the `Array::Rounded` idiom) must not recurse into itself. Consumed by
    /// the probe with `mem::take`, so it only ever masks the one immediately
    /// following dispatch, never a nested subscript evaluated underneath it.
    pub(crate) skip_postcircumfix_overload: bool,
    /// When set, pseudo-method names (DEFINITE, WHAT, etc.) bypass native fast path.
    /// Used for quoted method calls like `."DEFINITE"()`.
    pub(crate) skip_pseudo_method_native: Option<String>,
    /// Set by multi-method resolution when two or more candidates are equally
    /// specific (an ambiguous dispatch). Consumed by the caller to raise an
    /// `X::Multi::Ambiguous` error instead of silently picking one.
    pub(crate) dispatch_ambiguous: bool,
    /// Set while a multi DISPATCHER wrap's terminal re-dispatch is running
    /// (`dispatcher_wrap::redispatch_after_dispatcher_wrap`): the method
    /// name plus the VM call-frame and routine-stack depths it was set at.
    /// The dispatcher-wrap check skips the chain only for a call of that
    /// name at exactly those depths, so the re-dispatch itself does not
    /// re-enter the wrapper while a nested call of the same method from
    /// inside the chosen candidate (deeper) is wrapped as usual.
    pub(crate) dispatcher_wrap_bypass: Option<(String, usize, usize)>,
    /// Stack of remaining multi dispatch candidates for callsame/nextsame/nextcallee.
    /// Each entry is (function_name, remaining_candidates, original_args,
    /// first_candidate_rw_params). The 4th element lists the FIRST (winning,
    /// compiled) candidate's scalar `is rw`/`is raw` positional params as
    /// (positional_arg_index, sigil-less_param_name); it stays fixed across the
    /// redispatch chain so a `nextsame`+rw redispatch can (a) pass the rw param's
    /// CURRENT value to the next candidate and (b) write the chain's final value
    /// back into the first candidate's VM local slot, instead of the first
    /// candidate's exit flush clobbering it with its own stale value (§D capstone).
    pub(crate) multi_dispatch_stack: Vec<MultiDispatchEntry>,
    pub(crate) method_dispatch_stack: Vec<MethodDispatchFrame>,
    /// Method calls whose deferral frame is not built yet, interleaved with
    /// `method_dispatch_stack` by `dispatch_token` (see `method_dispatch_lazy`).
    pub(crate) pending_method_dispatch: Vec<method_dispatch_lazy::PendingMethodDispatch>,
    /// Stack of samewith dispatch contexts, pushed whenever a multi sub,
    /// multi method, or proto is entered, popped on exit. ADR-0019 E9c-1:
    /// a single `Vec<SamewithContext>` — every push site funnels through
    /// `push_samewith_context`/`push_method_samewith_context`, so `args` can
    /// never desync from `name`/`invocant` the way the former separate
    /// `samewith_call_args_stack` could (see `SamewithContext`'s doc comment).
    pub(crate) samewith_context_stack: Vec<SamewithContext>,
    /// Metamodel-method dispatch contexts:
    /// (samewith_depth, receiver_class, method_name, args).
    /// Pushed alongside the samewith context when the receiver's MRO includes
    /// a builtin metamodel class (Metamodel::ClassHOW / Metamodel::GrammarHOW),
    /// so a `callsame` in a user HOW method that exhausts the user MRO can
    /// fall through to the NATIVE metamodel implementation (e.g. the default
    /// `find_method`), which is not represented as a `MethodDef` candidate.
    /// `samewith_depth` ties each entry to its samewith frame so the shared
    /// pop helper knows whether the top entry belongs to the frame being popped.
    pub(crate) metamodel_dispatch_stack: Vec<(usize, String, String, Vec<Value>)>,
    /// Wrap chains: sub_id -> stack of (handle_id, wrapper_sub). Outermost is last.
    pub(crate) wrap_chains: std::sync::Arc<HashMap<u64, Vec<(u64, Value)>>>,
    /// Maps sub_id to function name for named call wrap chain lookup.
    pub(crate) wrap_sub_names: HashMap<u64, String>,
    /// Maps function name to the Sub value that was wrapped. Used to get the right sub_id
    /// when dispatching named function calls through the wrap chain.
    pub(crate) wrap_name_to_sub: ValueMap,
    /// Maps function name to the callable_id at the time wrap was first called.
    /// Used to detect sub redefinition (e.g. `sub foo` in a new block).
    pub(crate) wrap_callable_ids: std::sync::Arc<HashMap<String, Option<i64>>>,
    /// Counter for generating unique wrap handle IDs.
    pub(crate) wrap_handle_counter: u64,
    /// Stack of wrap dispatch frames for callsame/callwith inside wrappers.
    pub(crate) wrap_dispatch_stack: Vec<WrapDispatchFrame>,
    /// Monotonic counter stamped onto `wrap_dispatch_stack`/`method_dispatch_stack`/
    /// `multi_dispatch_stack` frames at push time (ADR-0019 E9b-0). callsame/nextsame/
    /// lastcall/nextcallee select the live frame with the HIGHEST token — the innermost
    /// dynamic dispatch context — instead of a fixed wrap-then-method-then-multi search
    /// order, so a method deferral nested inside a sub wrapper (or vice versa) resolves
    /// to its own chain instead of shadowing/being shadowed by the other stack.
    pub(crate) dispatch_token_counter: u64,
    /// One-shot chain-skip for callsame/callwith invoking the *original* sub
    /// (or an inner wrapper) of an active wrap dispatch: the very next
    /// `call_sub_value` on this sub id must run the sub directly instead of
    /// re-entering its wrap chain. Everything else — notably a *recursive*
    /// named call from inside the original body — re-enters the chain, the
    /// way Raku re-dispatches every fresh call of a wrapped sub.
    pub(crate) wrap_skip_once: Option<u64>,
    /// When set, a binding failure inside `call_compiled_closure` is returned
    /// raw instead of going through `enhance_binding_error`. The interpreter
    /// value-call carrier (`call_sub_value`) sets this around its
    /// compiled-routine fork (ADR-0019 C6d-4): a value call is never a
    /// compile-time-diagnosable call, so reclassifying its binding failure as
    /// a compile-flavored `X::TypeCheck::Argument` loses the runtime
    /// `X::TypeCheck::Binding` identity a sequence endpoint check relies on
    /// (roast S03-sequence/misc.t).
    pub(crate) suppress_binding_error_enhance: bool,
    pub(crate) method_dispatch_pure: bool,
    /// Registry function keys grouped by their BASE name — the index behind
    /// [`Interpreter::fn_keys_for_base`].
    ///
    /// Every candidate key a name-keyed dispatch can match (`Pkg::name`,
    /// `Pkg::name/<arity>`, `Pkg::name/<arity>:<types>`, `…__m<n>`) reduces to
    /// the same base name, so a candidate gather that used to iterate the whole
    /// functions map — several times per call, formatting a prefix `String` per
    /// package and resolving every key back to a `&str` — iterates a handful of
    /// keys instead. Filled on a miss — for every evicted base name at once
    /// when it can be (see `runtime::fn_keys_index`).
    ///
    /// Evicted per base name by `invalidate_fn_resolution_for_keys` (and
    /// wholesale by `invalidate_fn_resolution`), not polled against
    /// `fn_resolve_gen` like the other five dispatch caches — and audited
    /// against a fresh scan in debug builds, so a registry mutation that misses
    /// its invalidation fails CI rather than silently mis-dispatching.
    pub(crate) fn_keys_by_base: rustc_hash::FxHashMap<Symbol, std::sync::Arc<[Symbol]>>,
    /// Which base names `fn_keys_by_base` can answer without a scan: see
    /// `runtime::fn_keys_index`.
    pub(crate) fn_keys_index: fn_keys_index::FnKeysIndexState,
    /// Names declared with an empty-signature proto (`proto bar {*}`). Such a
    /// proto's signature gates the whole multi dispatch: any call with
    /// positional arguments "will never work with signature of the proto ()"
    /// (rakudo rejects it at compile time). Populated at proto registration;
    /// checked cheaply (guarded by `is_empty()`) on each call.
    pub(crate) empty_sig_proto_names: std::collections::HashSet<Symbol>,
    /// Fingerprint of the sub declaration this registrar last installed under
    /// each `package::name` (single, non-multi) routine key, together with the
    /// exact `FunctionDef` it installed. A re-executed `RegisterSub` whose
    /// compile-time fingerprint matches -- AND whose recorded definition is
    /// still the one the registry holds -- is an idempotent no-op (see
    /// [`crate::ast::sub_registration_fingerprint`]), so the registrar can
    /// return early without re-deriving the FunctionDef and without
    /// invalidating the resolution caches.
    ///
    /// The recorded `Arc` is what makes the identity check exact. This map is
    /// name-keyed and is NOT updated by `restore_routine_registry`, which puts
    /// a whole snapshot of `registry.functions` back when a routine scope ends
    /// -- so after an inner `sub r` of routine A returns, the key `Pkg::r` can
    /// again hold a DIFFERENT routine B's inner `sub r`, while this map still
    /// names A's. A presence-only check ("something is installed under this
    /// name") then wrongly reports A's declaration as already installed and
    /// leaves B's definition live inside A's body. `Digest::SHA2` is the real
    /// case: `sha256` and `sha512` each declare `rotr`/`Σ0`/`Σ1`/`σ0`/`σ1`, and
    /// `sha256` silently computed with `sha512`'s 64-bit rotations.
    pub(crate) registered_fn_fingerprints:
        rustc_hash::FxHashMap<Symbol, (u64, std::sync::Arc<FunctionDef>)>,
    /// Declaration sites (fully-qualified name, compile-time site fingerprint)
    /// that registered a yada-stub routine. A `RegisterSub` executes both
    /// hoisted at block top and in place, so a stub's in-place re-arrival can
    /// find its name already overwritten by the real definition (which the
    /// stub forward-declared); membership here identifies that re-arrival as
    /// an idempotent no-op, while a textually NEW stub after a definition (a
    /// different site, different fingerprint) still raises X::Redeclaration.
    pub(crate) registered_stub_decl_sites: std::sync::Arc<rustc_hash::FxHashSet<(Symbol, u64)>>,
    /// Derive-once cache: a declaration is parsed into a `FunctionDef` exactly
    /// once, then shared. Keyed by the routine's fully-qualified name
    /// (`package::name`), the value is `(declaration fingerprint, Arc<FunctionDef>)`.
    /// A `my sub` inside a routine is removed from the registry when the routine
    /// returns (lexical-scope snapshot/restore) and re-installed on the next call;
    /// without this cache that re-install would re-run the full AST→FunctionDef
    /// derivation (auto-signature scan, validation, body clone) every call. With
    /// it, the re-install is a cheap `Arc` clone of the already-derived definition.
    /// The key is the FQ name (not the fingerprint) so two distinct subs that share
    /// an identical body but differ in name never alias; the stored fingerprint is
    /// re-checked on lookup so a redefined body at the same name re-derives.
    pub(crate) prepared_fn_defs: HashMap<Symbol, (u64, Arc<FunctionDef>)>,
    /// The one `(type-name address, method, receiver NaN-box word)` whose user
    /// augmentation [`Interpreter::native_lever_a_user_override_sym`] must not
    /// report while a deferral out of that augmentation runs the builtin
    /// method as its final candidate (#10198); see
    /// `builtins_dispatch_next_core`. Keyed by receiver identity so a nested
    /// call on another receiver still reaches the user method.
    pub(crate) native_base_bypass: Option<(usize, Symbol, u64)>,
    /// The multi families whose dispatcher code value (`&trait_mod:<is>`
    /// exported by `sub EXPORT`) a by-name call is running right now. That
    /// call re-enters resolution of the family, so a nested by-name call of
    /// the same family that found no candidate must not retry through the
    /// dispatcher again: see `Interpreter::is_live_family_dispatcher`.
    pub(crate) family_dispatchers_in_flight: Vec<Symbol>,
}

impl DispatchState {
    /// The state a spawned thread starts with: no dispatch in progress, but
    /// the program's operator tables and `.wrap` chains carried over (a
    /// wrapped routine stays wrapped on every thread).
    // Cost: O(w + n), w = `wrap_sub_names` entries, n = `wrap_name_to_sub`
    // entries; the `Arc`-held tables are refcount bumps.
    pub(crate) fn fork_for_thread(&self) -> Self {
        Self {
            operator_assoc: self.operator_assoc.clone(),
            user_declared_infix_ops: self.user_declared_infix_ops.clone(),
            wrap_chains: self.wrap_chains.clone(),
            wrap_sub_names: self.wrap_sub_names.clone(),
            wrap_name_to_sub: self.wrap_name_to_sub.clone(),
            wrap_callable_ids: self.wrap_callable_ids.clone(),
            wrap_handle_counter: self.wrap_handle_counter,
            // Stub-site knowledge is declaration-shape knowledge, not run
            // state: carry it into the thread so a routine registered on the
            // parent (stub hoisted, then defined) does not re-raise
            // X::Redeclaration when the thread re-executes the block.
            registered_stub_decl_sites: self.registered_stub_decl_sites.clone(),
            ..Default::default()
        }
    }
}
