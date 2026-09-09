use std::collections::{HashMap, HashSet};
use std::fmt;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, OnceLock};

use crate::symbol::Symbol;
use crate::value::Value;

/// The lexical environment's backing map. Keyed by interned `Symbol` (an 8-byte
/// `(u32, u32)`-free `u32` handle), looked up on every variable read/write — the
/// single hottest map in the interpreter. `FxHashMap` (a non-cryptographic hash
/// over the small integer key) replaces the default `SipHash`, whose per-lookup
/// cost dominated method/variable-heavy benchmarks (perf: ~7% of self time in
/// `SipHasher::write`/`hash_one`). Iteration order is unspecified either way, so
/// no env-iteration consumer (`iter`/`keys`/`values`/pseudo-stash) is affected.
pub(crate) type SymMap = rustc_hash::FxHashMap<Symbol, Value>;

/// Process-wide immutable "base" tier of the environment.
///
/// Holds the built-in enum constants (`Order`, `Endian`, `ProtocolFamily`,
/// `Signal` and their bare/qualified variant names) -- ~70 entries that are
/// constant for the whole process. They are looked up by name but never
/// mutated, removed, or iterated-over as part of the lexical environment.
///
/// Keeping them out of every per-frame `Env` overlay is the point: each
/// compiled call clones the env and the first write `Arc::make_mut`-deep-copies
/// it, an O(env_size) cost. With ~70 of the ~119 entries hoisted into this
/// shared, never-copied base, the per-call deep copy shrinks to the handful of
/// real lexicals. See docs/vm-dual-store.md (Slice 4b).
static GLOBAL_BASE: OnceLock<SymMap> = OnceLock::new();

/// Install the immutable base tier. Idempotent: the first caller wins and
/// later calls are ignored (the base is identical for every interpreter in a
/// process, so re-installation is a no-op rather than an error).
pub(crate) fn set_global_base(map: HashMap<Symbol, Value>) {
    // The base tier is read through by `get_sym` when the overlay chain misses,
    // so it is the one place a `&return` binding could become visible without
    // passing `insert_sym`. It only ever holds built-in enum values, but latch
    // the flag from here too so the invariant does not depend on that.
    if map.contains_key(&crate::symbol::wk::rebound_return()) {
        RETURN_REBOUND_SEEN.store(true, Ordering::Relaxed);
    }
    let _ = GLOBAL_BASE.set(map.into_iter().collect());
}

#[inline(always)]
fn global_base() -> Option<&'static SymMap> {
    GLOBAL_BASE.get()
}

/// True if `name` is a built-in global-base constant — the process-wide enum
/// values such as `Endian` (`NativeEndian`/`LittleEndian`/`BigEndian`), `Order`
/// (`Less`/`Same`/`More`), `ProtocolFamily`, `Signal`, .... Used to recognise a
/// bare enum-value parameter (`sub f(LittleEndian)`) as a valid value-param
/// rather than an undeclared type name. Checks only the immutable base tier, so
/// a user lexical of the same name cannot mask the answer.
pub(crate) fn global_base_contains(name: &str) -> bool {
    global_base().is_some_and(|b| b.contains_key(&Symbol::intern(name)))
}

/// Env-key prefix under which a block-scoped `sub` stores its `Sub` value so a
/// closure that escapes the block can still call it by bare name (block exit
/// restores the routine registry, dropping the registration). Reserved rather
/// than the plain `&name` key: while the block is live the registry entry is
/// authoritative — it carries `state` variables and the wrap chain — and the
/// bareword/call paths consult `&name` *ahead* of it. Read only after the
/// registry misses. See `exec_register_sub_op`.
pub(crate) const BLOCK_LEXICAL_SUB_PREFIX: &str = "__mutsu_block_lexical_sub::";

/// The env key / local-slot name of a **user lexical** spelled `$self`.
///
/// `self` is a *term* in Raku, not a `$`-sigiled variable, and mutsu binds a
/// method's invocant to the sigil-less env key `"self"`. Scalars are otherwise
/// stored sigil-less too, so a user's `my $self` would land on that very key and
/// be silently replaced by whatever invocant the enclosing closure is next
/// called with (ADR-0061). The sigiled spelling is reserved for the user
/// lexical: no ordinary scalar carries its `$`, so `"$self"` cannot collide.
///
/// Only the *lexical* takes this key. `"self"` still means the invocant
/// everywhere, and a routine whose own signature declares a `$self` parameter
/// (`method m($self: $n)`, `sub ($self)`, `-> $self, $x`) resolves `$self` back
/// to that parameter — see `Compiler::self_is_signature_param`.
pub(crate) const LEX_SELF: &str = "$self";

/// Render a scalar variable's env/AST name with its `$` sigil, without doubling
/// the sigil [`LEX_SELF`] already carries.
pub(crate) fn sigiled_scalar_name(name: &str) -> String {
    if name == LEX_SELF {
        name.to_string()
    } else {
        format!("${}", name)
    }
}

/// True if `name` is a *plain user lexical* env key — a `my`/`our`-declared
/// scalar/array/hash/sub whose name is an ordinary lowercase identifier (stored
/// scalar-sigilless, e.g. `$x` → `"x"`, `@a` → `"@a"`). These are the only env
/// keys a closure body reaches exclusively through a `GetGlobal`-family opcode,
/// so the compiler's `free_var_syms` set already lists every one a closure can
/// reference. The closure-capture upvalue path
/// ([`crate::runtime::Interpreter::capture_closure_env`], single-store Slice E)
/// uses this to drop the *non-free* plain lexicals from a closure's captured env
/// while keeping everything else.
///
/// Everything that is NOT a plain user lexical is kept by the capture, because a
/// closure body may read it through a dedicated opcode the free-var scan cannot
/// see: `self` (attribute access), special vars (`$_`→`"_"`, `$/`→`"/"`,
/// `$!`→`"!"`, `$?FILE`→`"?FILE"`, …), dynamic vars (`$*x`→`"*x"`/`"$*x"`), match
/// captures (`$0`→`"0"`, `$<n>`→`"<n>"`), `&?ROUTINE`/`&?BLOCK`, the `__mutsu_*`
/// shadow-meta, and type names (uppercase-initial). The classification is
/// therefore deliberately conservative: it returns `true` (droppable) only for a
/// lowercase-identifier-shaped name (optionally `@`/`%`/`&`-sigiled), and `self`
/// — the one lowercase system name read via a dedicated opcode — is excluded.
#[inline]
pub(crate) fn is_plain_user_lexical(name: &str) -> bool {
    if name == "self" {
        return false;
    }
    let b = name.as_bytes();
    let Some(&first) = b.first() else {
        return false;
    };
    // The character that decides the name's "kind": for a sigiled array/hash/sub
    // key it is the char after the sigil; otherwise the first char (scalars are
    // stored sigil-less). A plain user lexical starts there with a lowercase
    // ASCII letter. Anything else — uppercase (types), `?`/`*`/`/`/`!`/`<`
    // (specials/dynamics/captures), a digit (positional captures), `_` (`$_`,
    // `@_`), or `__mutsu_*` — is a system name the capture must keep.
    //
    // `$` is in the sigil set for the one key that carries it: [`LEX_SELF`], the
    // reserved key of a user `my $self` (ADR-0061). It IS a plain user lexical
    // and must be droppable when it is not free — kept unconditionally, a stale
    // outer `$self` would shadow the routine-local one a nested closure captures.
    // A dynamic key like `"$*x"` still has decider `*`, so it stays kept.
    let decider = if matches!(first, b'@' | b'%' | b'&' | b'$') {
        b.get(1).copied()
    } else {
        Some(first)
    };
    matches!(decider, Some(c) if c.is_ascii_lowercase())
}

/// True for an *attribute-twigil* env key (`!x`, `@!x`, `%.x`, `$.y`, …): a
/// per-frame materialization of one of `self`'s attributes rather than a
/// lexical.
///
/// The closure capture must never snapshot one: the closure has to read the
/// attribute through its captured `self` at RUN time, because a creation-time
/// copy goes stale the moment the instance mutates (a `start` block reading
/// `@!before` inside `Cro::CompositeConnector.connect` saw an empty
/// pre-mutation copy).
#[inline]
pub(crate) fn is_attr_twigil_env_key(name: &str) -> bool {
    let bare = match name.as_bytes().first() {
        Some(b'@' | b'%' | b'&' | b'$') => &name[1..],
        _ => name,
    };
    let b = bare.as_bytes();
    matches!(b.first(), Some(b'!') | Some(b'.')) && b.len() > 1 && b[1].is_ascii_alphabetic()
}

/// True when `name` is a sigil-less env key that holds a *magic variable* rather
/// than a name binding, and must therefore never be read as a lexical
/// type/package alias.
///
/// A lexically scoped `my class Foo` binds `env["Foo"]` to a `Package` naming its
/// mangled storage symbol, and several bare-name type lookups resolve a short
/// name through exactly that binding (see
/// [`crate::runtime::Interpreter::resolve_bare_type_name`]). The topic `$_` is
/// stored sigil-less as `"_"`, which collides with the identifier `_` in that
/// namespace: entering a routine seeds the implicit topic with the `Any` type
/// object, so `env.get("_")` answers `Package(Any)` and those lookups read the
/// topic as if it were a `my class _` aliasing `Any`. Inside a routine that
/// turned an unresolvable `_(1, 2)` into a coercion to `Any` returning `(1, 2)`
/// instead of "Unknown function: _" — which is how `EVAL '10_.0'` (parsed as a
/// speculative `infix:<_>`) stopped throwing whenever the `EVAL` ran inside a
/// routine.
///
/// `_` is the only sigil-less magic key that is also a legal bare identifier
/// (`/`, `!`, `?FILE`, `0`, `<n>`, `*x`, `__mutsu_*` are all unreachable as type
/// names), so this is deliberately exactly one name.
///
// TODO: the real fix is to stop storing the topic under a key that lives in the
// identifier namespace (`"$_"`, or a reserved prefix like `LEX_SELF`), which
// would also make a genuine `class _ { }` reachable — it is unreachable through
// these lookups for as long as the collision stands.
#[inline]
pub(crate) fn is_magic_sigilless_key(name: &str) -> bool {
    name == "_"
}

/// True for a dynamic variable's env key (the `*` twigil): `$*x` → `"*x"` (scalars
/// are stored sigil-less), `@*x` → `"@*x"`, `%*VAR` → `"%*VAR"`, `&*f` → `"&*f"`.
///
/// A dynamic variable is resolved through the *caller* chain at call time, never
/// from a lexical capture. Closure machinery that freezes a captured value per
/// closure instance (`closure_captured_state`) must therefore skip these names:
/// persisting one pins the closure to the value seen on its first call, so a later
/// re-assignment by the caller becomes invisible (`our %*VAR = ...` re-set per row
/// of a truth table left every row reading row 1's bindings — roast
/// integration/99problems-41-to-50.t P46).
#[inline]
pub(crate) fn is_dynamic_var_name(name: &str) -> bool {
    let b = name.as_bytes();
    let Some(&first) = b.first() else {
        return false;
    };
    let decider = if matches!(first, b'@' | b'%' | b'&') {
        b.get(1).copied()
    } else {
        Some(first)
    };
    decider == Some(b'*')
}

/// Whether `key` — an *env key*, not a source-level variable name — names a
/// dynamic variable: `*x`, `$*OUT`, `@*ARGS`, `%*ENV`, `&*foo`.
///
/// Deliberately distinct from [`is_dynamic_var_name`], which strips at most one
/// `@`/`%`/`&` and so answers `false` for a key that kept its `$` sigil. Env
/// keys are *mostly* stored sigil-less for scalars, but not always — the
/// built-in dynamics are seeded under their full spelling (`$*OUT`,
/// `$*ERR`, …; see `Interpreter::init_io_environment_impl`), and the
/// closure-capture merge must recognize exactly those. This predicate is the
/// one that merge has always used, lifted out of it verbatim so the memoized
/// [`crate::symbol::flags::DYNAMIC_VAR_ENV_KEY`] bit cannot drift from it.
#[inline]
pub(crate) fn is_dynamic_var_env_key(key: &str) -> bool {
    key.trim_start_matches(['$', '@', '%', '&'])
        .starts_with('*')
}

/// Monotonic, process-global flag: set the first time any closure-writeback
/// metadata key is inserted into *any* env. These keys --
/// `__mutsu_sigilless_readonly::*`, `__mutsu_sigilless_alias::*`,
/// `__mutsu_state_key::*`, `__mutsu_predictive_seq_iter::*` -- drive the
/// per-call sigilless/alias/state-var write-back scans in the closure exit path
/// (`call_compiled_closure`). The common program never creates any of them, so
/// the closure path consults [`closure_meta_keys_possible`] to skip those
/// scans (and their `format!` allocations) entirely.
///
/// Soundness: every such key is created via the String-keyed [`Env::insert`]
/// (always a `format!` result), so detecting them there catches *every*
/// creation site regardless of which of the ~20 scattered callers inserts it --
/// the robustness the per-site approach could not guarantee. The flag is
/// monotonic and global: once `true` it stays `true`, and `true` only ever
/// makes the (correct) scan run, so an over-set is conservative, never wrong.
/// A program's metadata lives in its own (per-thread) env, and the creating
/// `insert` runs earlier in that thread's program order than any closure that
/// reads it, so `Relaxed` ordering suffices.
static CLOSURE_META_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for `__mutsu_sigilless_readonly::*` keys
/// alone.
///
/// [`CLOSURE_META_KEY_SEEN`] lumps four unrelated key families together, so a
/// program that creates a `__mutsu_state_key::` (a `state` variable) or a
/// `__mutsu_predictive_seq_iter::` arms the readonly probe too -- and that
/// probe runs on EVERY whole-variable assignment (`OpCode::CheckReadOnly`),
/// building a `format!("__mutsu_sigilless_readonly::{name}")` and hashing it
/// into the env for a key the program never created. Sigilless/`:=` readonly
/// markers are much rarer than `state` variables, so they deserve their own
/// latch. Same soundness argument as [`CLOSURE_META_KEY_SEEN`]: every creation
/// site is a String-keyed [`Env::insert`], the flag is monotonic, and an
/// over-set only makes the (correct) probe run.
static SIGILLESS_READONLY_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for `__mutsu_bound::*` keys (the `:=`-bound
/// container markers consulted by `CheckReadOnly` on every whole-variable
/// assignment). Same soundness argument as [`CLOSURE_META_KEY_SEEN`]: every
/// creation site flows through the String-keyed [`Env::insert`], the flag is
/// monotonic, and an over-set only makes the (correct) check run.
static BOUND_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for `__mutsu_bound_array_slice::*` markers
/// (sigilless multi-dim slice binds), consulted by
/// `distribute_bound_multidim_slice` on every scalar `SetLocal`/assignment.
static BOUND_SLICE_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for the per-element index metadata keys
/// (`__mutsu_bound_index::*`, `__mutsu_elem_share::*`, `__mutsu_deleted_index::*`).
/// Their probes run on *every* element write (`@a[i] = x`) and on `:exists`, and
/// each would otherwise cost a `format!` plus a `Symbol::intern`ing env lookup.
/// The keys only ever appear once the program `:=`-binds an element, `=`-shares a
/// container into one, or `:delete`s an index — none of which the common program
/// does. Same soundness argument as [`CLOSURE_META_KEY_SEEN`]: every creation site
/// flows through the String-keyed [`Env::insert`], the flag is monotonic, and an
/// over-set only makes the (correct) probe run.
static ELEM_INDEX_META_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for `^name` placeholder-parameter keys (`$^a`
/// & co, bound under their careted name). Every by-name env *write* and *read*
/// (`set_env_with_main_alias` / `get_env_with_main_alias` — i.e. every mirrored
/// local store) otherwise probes for a de-careted alias, costing a `format!` plus
/// a `Symbol::intern`ing env lookup per store. Placeholder params are bound
/// through the String-keyed [`Env::insert`], so the same soundness argument as
/// [`CLOSURE_META_KEY_SEEN`] applies: monotonic, and an over-set only makes the
/// (correct) probe run.
static PLACEHOLDER_KEY_SEEN: AtomicBool = AtomicBool::new(false);

/// Monotonic, process-global flag for a lexically rebound `&return`
/// (`my &return = ...` / `my &return := ...`).
///
/// Raku lets `return` be rebound lexically, so *every* routine return has to ask
/// "is `&return` bound here?" before raising the built-in return signal — both
/// the interpreter's `OpCode::Return` arm and the JIT's `ret` shim. That question
/// is an `Env::get_sym` miss, which walks the whole overlay/parent chain and then
/// consults [`GLOBAL_BASE`]: measured as ~2% of `bench-fib`, where it is pure
/// waste (2 chain tiers hashed per call, 6421 `get_sym` entries for 3193 calls,
/// all of them misses).
///
/// Rebinding `&return` is vanishingly rare, and the binding can only become
/// visible to a return by first being *inserted* into an env — every insert path
/// funnels through [`Env::insert_sym`] (`insert` / `insert_through*` /
/// `entry_or_insert*` all delegate to it, and `inner_mut` has no callers), so
/// latching the flag there catches every creation site. Same soundness argument
/// as [`CLOSURE_META_KEY_SEEN`]: the flag is monotonic, the insert runs earlier in
/// program order than any return that could observe the binding, and an over-set
/// only makes the (correct) probe run.
static RETURN_REBOUND_SEEN: AtomicBool = AtomicBool::new(false);

/// True if a `__mutsu_sigilless_readonly::*` key may exist in some env. See
/// [`SIGILLESS_READONLY_KEY_SEEN`]. Strictly narrower than
/// [`closure_meta_keys_possible`], so it is the right gate for a probe that
/// only looks for that one key family.
#[inline]
pub(crate) fn sigilless_readonly_keys_possible() -> bool {
    SIGILLESS_READONLY_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if any closure-writeback metadata key may exist in some env. See
/// [`CLOSURE_META_KEY_SEEN`].
#[inline]
pub(crate) fn closure_meta_keys_possible() -> bool {
    CLOSURE_META_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if any `__mutsu_bound::*` marker may exist in some env. See
/// [`BOUND_KEY_SEEN`].
#[inline]
pub(crate) fn bound_marker_possible() -> bool {
    BOUND_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if any `__mutsu_bound_array_slice::*` marker may exist in some env.
/// See [`BOUND_SLICE_KEY_SEEN`].
#[inline]
pub(crate) fn bound_array_slice_possible() -> bool {
    BOUND_SLICE_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if any per-element index metadata key may exist in some env. See
/// [`ELEM_INDEX_META_SEEN`].
#[inline]
pub(crate) fn elem_index_meta_possible() -> bool {
    ELEM_INDEX_META_SEEN.load(Ordering::Relaxed)
}

/// True if any `^name` placeholder-parameter key may exist in some env. See
/// [`PLACEHOLDER_KEY_SEEN`].
#[inline]
pub(crate) fn placeholder_var_possible() -> bool {
    PLACEHOLDER_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if some env may hold a lexically rebound `&return`. See
/// [`RETURN_REBOUND_SEEN`].
#[inline]
pub(crate) fn return_rebound_possible() -> bool {
    RETURN_REBOUND_SEEN.load(Ordering::Relaxed)
}

/// Flip [`CLOSURE_META_KEY_SEEN`] / [`BOUND_KEY_SEEN`] / [`PLACEHOLDER_KEY_SEEN`]
/// if `key` is one of the tracked metadata keys. The outer `__mutsu_` / `^` gates
/// keep this ~1 byte compare for ordinary lexical names (which never start with
/// `_` or `^`).
///
/// `pub(crate)` because the Symbol-keyed [`Env::insert_sym`] does not run it (its
/// callers write pre-interned lexical slots, never metadata keys); a caller that
/// routes a *name-derived* write through `insert_sym` must call this itself to
/// keep the latches sound.
#[inline]
pub(crate) fn note_env_key(key: &str) {
    if key.as_bytes().starts_with(b"^") {
        PLACEHOLDER_KEY_SEEN.store(true, Ordering::Relaxed);
        return;
    }
    if key.as_bytes().starts_with(b"__mutsu_") {
        if key.starts_with("__mutsu_sigilless_")
            || key.starts_with("__mutsu_state_key::")
            || key.starts_with("__mutsu_predictive_seq_iter::")
        {
            CLOSURE_META_KEY_SEEN.store(true, Ordering::Relaxed);
            if key.starts_with("__mutsu_sigilless_readonly::") {
                SIGILLESS_READONLY_KEY_SEEN.store(true, Ordering::Relaxed);
            }
        } else if key.starts_with("__mutsu_bound::") {
            BOUND_KEY_SEEN.store(true, Ordering::Relaxed);
        } else if key.starts_with("__mutsu_bound_array_slice::") {
            BOUND_SLICE_KEY_SEEN.store(true, Ordering::Relaxed);
        } else if key.starts_with("__mutsu_bound_index::")
            || key.starts_with("__mutsu_elem_share::")
            || key.starts_with("__mutsu_deleted_index::")
            || key.starts_with("__mutsu_ro_index::")
        {
            ELEM_INDEX_META_SEEN.store(true, Ordering::Relaxed);
        }
    }
}

/// Copy-on-write environment wrapper.
///
/// Wraps `Arc<HashMap<Symbol, Value>>` so that cloning is O(1) (just an Arc bump).
/// Mutation goes through `Arc::make_mut`, triggering a deep clone only when
/// the Arc is shared.  Symbol keys make the deep clone cheaper: key clone is
/// O(1) (Copy) instead of O(n) heap allocation for String keys.
///
/// Name lookups (`get`/`contains_key`/`get_mut`) fall back to the shared
/// immutable [`GLOBAL_BASE`] tier on an overlay miss. Iteration, removal, and
/// in-place value iteration operate on the overlay only -- the base tier is a
/// read-only constant pool (built-in enums), not part of the mutable lexical
/// environment, so it is invisible to `iter`/`keys`/`values`/`len`/`remove`.
#[derive(Clone)]
pub struct Env {
    inner: Arc<SymMap>,
    /// Optional read-through "parent" tier: the enclosing call frame's whole env
    /// (itself possibly scoped, forming a *chain*). When present (a *scoped* env),
    /// name lookups fall through overlay -> parent-chain -> [`GLOBAL_BASE`], but
    /// `insert`/`remove`/`get_mut` and iteration (`iter`/`keys`/`values`/`len`)
    /// operate on this frame's overlay only.
    ///
    /// `parent=None` is the flat env: byte-identical to the pre-scoped behavior,
    /// so every non-converted dispatch path and the ~80 env-iteration consumers
    /// are unaffected. Holding the *whole* parent `Env` (not just its overlay
    /// `HashMap`) is what makes nesting free: a method already under a scoped
    /// overlay that calls another method just chains a new child over it, instead
    /// of `flattened()`-merging the 2-tier env into a flat map per nested call
    /// (the O(env) cost measured in PR #2683 / docs/vm-dual-store.md). The chain
    /// always bottoms out at a flat (`parent=None`) env, so [`GLOBAL_BASE`] is
    /// consulted exactly once at the chain's tail.
    ///
    /// A scoped env is transient -- it is only ever the live `self.env` during a
    /// converted call frame's own opcode execution and the `saved_env` slots that
    /// restore it. Anything that captures the env into a long-lived structure
    /// (a `Sub` closure, an END phaser, a thread) flattens it first via
    /// [`Env::flattened`] / `clone_env`, so an overlay-only iteration consumer is
    /// never starved of parent-chain lexicals. See docs/vm-dual-store.md (Slice 6).
    parent: Option<Arc<Env>>,
    /// Tombstones: keys `remove`d in this scoped overlay that still exist in the
    /// `parent`/base tier. Because the overlay can only *add* shadowing entries,
    /// a plain overlay `remove` cannot hide a parent key; a tombstone records
    /// "this key is deleted in this scope" so `get`/`contains_key` stop falling
    /// through to the parent. This is what makes "clear inherited state" idioms
    /// (e.g. `my $x` clearing an outer `\x`'s `__mutsu_sigilless_readonly::x`)
    /// behave correctly under a scoped overlay. `None` for flat envs (no scope to
    /// shadow). Tombstones are dropped with the overlay on return and are never
    /// merged back (a callee-local removal must not delete the caller's key).
    // Keyed by interned `Symbol`; use `FxHashSet` (non-cryptographic hash over
    // the small integer key), not the default `SipHash`. `is_tombstoned` runs on
    // every env lookup that misses the overlay and walks the parent chain, so a
    // `SipHash` here showed up as ~5% of self time on method-heavy benchmarks
    // (mzef ctor) — the same reason `SymMap` is `FxHashMap`.
    tombstones: Option<rustc_hash::FxHashSet<Symbol>>,
    /// Number of parent tiers below this env (0 for a flat env). Used to bound
    /// the chain length: a recursive function would otherwise grow the chain one
    /// tier per call, making `get`/`Drop`/`flattened` recurse to the recursion
    /// depth. [`Self::scoped_child`] flattens the parent once the chain reaches
    /// [`MAX_OVERLAY_DEPTH`], so the chain length (hence lookup cost and `Drop`
    /// recursion) stays O(1) while shallow nesting (methods, ~2-5 deep) pays no
    /// flatten.
    depth: u16,
    /// This env's visible `?FILE`, pre-interned — see [`Env::source_file_sym`].
    file_sym: Option<Symbol>,
    /// The by-name writes this env's **frame tier** has taken since the frame
    /// opened, recorded only for an env whose tier was collapsed into the flat
    /// map by [`Self::flattened_for_frame`]. `None` everywhere else — a scoped
    /// env's overlay *is* that record, so there is nothing to keep beside it.
    /// See [`Self::flattened_for_frame`].
    ///
    /// `Arc` rather than a plain `Vec` so cloning an env (once per light call,
    /// on the swap path) stays a refcount bump; the log is copy-on-write like
    /// `inner`.
    frame_writes: Option<Arc<Vec<Symbol>>>,
}

/// Maximum overlay chain length before [`Env::scoped_child`] flattens the parent.
/// Typical method/function nesting is a handful of tiers, well under this; only
/// deep recursion ever hits it, paying one O(env) flatten per this many frames.
const MAX_OVERLAY_DEPTH: u16 = 16;

/// Process-wide shared empty overlay map for fresh scoped children. A brand-new
/// overlay is always empty, so every frame can share one allocation: the first
/// `insert` goes through `cow_mut`'s `Arc::make_mut`, which sees the shared
/// refcount and clones the (empty) map into a private one — plain
/// copy-on-write, no behavioral difference. `Env::ptr_eq` consumers are
/// unaffected: two envs sharing this map are both empty, and any write
/// un-shares the writer before it can be observed.
fn empty_overlay() -> Arc<SymMap> {
    empty_overlay_ref().clone()
}

/// Borrowed access to the shared empty overlay singleton, for identity checks
/// that must not pay the `Arc` refcount round-trip a `clone` would cost.
fn empty_overlay_ref() -> &'static Arc<SymMap> {
    static EMPTY: std::sync::OnceLock<Arc<SymMap>> = std::sync::OnceLock::new();
    EMPTY.get_or_init(|| Arc::new(SymMap::default()))
}

/// The interned `"?FILE"` key. Interning it once keeps the maintenance hook in
/// every `Env` mutator down to a `u32` compare — see [`Env::source_file_sym`].
#[inline(always)]
fn file_key() -> Symbol {
    crate::symbol::wk::file()
}

/// Intern a `?FILE` value. A non-`Str` `?FILE` has no file symbol, matching
/// what a chain walk followed by a `ValueView::Str` match would yield.
#[inline]
fn file_sym_of(v: &Value) -> Option<Symbol> {
    match v.view() {
        crate::value::ValueView::Str(s) => Some(Symbol::intern(s.as_str())),
        _ => None,
    }
}

impl Env {
    pub(crate) fn new() -> Self {
        Self {
            inner: Arc::new(SymMap::default()),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym: None,
            frame_writes: None,
        }
    }

    /// Create a *scoped child* env: an empty overlay that reads through to
    /// `parent` (the whole caller-frame env, itself possibly scoped) and then to
    /// [`GLOBAL_BASE`]. Writes land in the (initially empty) overlay, so the
    /// inherited entries are never `make_mut`-deep-copied, and -- because the
    /// parent is the *whole* env rather than a flattened overlay -- chaining over
    /// an already-scoped env is O(1) (no per-nested-call flatten). To keep the
    /// chain from growing unbounded under deep recursion, the parent is flattened
    /// to a single tier once it reaches [`MAX_OVERLAY_DEPTH`]. See
    /// docs/vm-dual-store.md.
    pub(crate) fn scoped_child(mut parent: Env) -> Self {
        // A fresh overlay is empty with no tombstones, so the child's visible
        // `?FILE` is exactly the parent's -- including on the empty-tier-reuse
        // and flatten paths below, which only skip/collapse tiers that were
        // already invisible to lookups. See `source_file_sym`.
        let file_sym = parent.file_sym;
        // Empty-tier reuse: a scoped parent whose overlay never received a
        // write (and has no tombstones) is invisible to lookups, so chain the
        // new child over the parent's own parent instead of stacking another
        // tier. A pure recursive function (no env-synced params, no by-name
        // writes) then runs at constant chain depth and never triggers the
        // MAX_OVERLAY_DEPTH flatten — which was measured at >40% of fib(25)
        // wall time (HashMap deep clone + drop per flatten).
        //
        // J4d: the walked-to tier is reused as its existing `Arc` (instead of
        // cloning the `Env` out and re-wrapping it in a fresh `Arc`), and the
        // new empty overlay shares the process-wide [`empty_overlay`] map
        // (copy-on-write on first insert via `cow_mut`'s `Arc::make_mut`).
        // Steady-state recursion therefore allocates nothing here — the two
        // `Arc::new`s per call were ~14% of fib with the JIT on.
        if parent.inner.is_empty()
            && parent.tombstones.is_none()
            && let Some(gp) = parent.parent.take()
        {
            let mut arc = gp;
            while arc.inner.is_empty()
                && arc.tombstones.is_none()
                && let Some(gp) = &arc.parent
            {
                let gp = Arc::clone(gp);
                arc = gp;
            }
            if arc.depth >= MAX_OVERLAY_DEPTH {
                let flat = arc.flattened();
                return Self {
                    inner: empty_overlay(),
                    depth: flat.depth + 1,
                    parent: Some(Arc::new(flat)),
                    tombstones: None,
                    file_sym,
                    frame_writes: None,
                };
            }
            return Self {
                inner: empty_overlay(),
                depth: arc.depth + 1,
                parent: Some(arc),
                tombstones: None,
                file_sym,
                frame_writes: None,
            };
        }
        let parent = if parent.depth >= MAX_OVERLAY_DEPTH {
            parent.flattened()
        } else {
            parent
        };
        Self {
            inner: empty_overlay(),
            depth: parent.depth + 1,
            parent: Some(Arc::new(parent)),
            tombstones: None,
            file_sym,
            frame_writes: None,
        }
    }

    /// This env's visible `?FILE` as a `Symbol`, or `None` when `?FILE` is
    /// unset or is not a `Str`.
    ///
    /// Equivalent to `get("?FILE")` followed by `Symbol::intern`, but O(1):
    /// every `RoutineFrame` push records the call-site file (ADR-0037 Slice 1
    /// put a push on all four call paths), and resolving it through the
    /// overlay chain plus an intern of the whole path was ~5% of
    /// `benchmarks/bench-fib.raku`.
    ///
    /// The value is carried BY the env rather than mirrored on the
    /// `Interpreter` deliberately: the runtime swaps whole envs in and out at
    /// ~50 sites (`self.env = saved_env`), and an interpreter-side mirror
    /// silently went stale at every one of them. Living on `Env` makes those
    /// swaps correct for free — the answer travels with the env it describes.
    /// It is maintained by the mutators above; `Interpreter::current_source_file_sym`
    /// carries a debug assertion that re-derives it from a full chain walk on
    /// every call, so any mutator that forgets the hook fails the whole `t/`
    /// suite, which CI runs on the debug binary (ADR-0014).
    #[inline(always)]
    pub(crate) fn source_file_sym(&self) -> Option<Symbol> {
        self.file_sym
    }

    /// Re-derive [`Self::source_file_sym`] after a bulk overlay edit that may
    /// have dropped `?FILE` (`retain`, `retain_overlay`): the overlay's own
    /// entry wins, else the parent chain's.
    fn refresh_file_sym(&mut self) {
        self.file_sym = match self.inner.get(&file_key()) {
            Some(v) => file_sym_of(v),
            None if self.is_tombstoned(file_key()) => None,
            None => self.parent.as_ref().and_then(|p| p.file_sym),
        };
    }

    /// True if `key` is tombstoned (removed) in this scoped overlay.
    #[inline(always)]
    fn is_tombstoned(&self, key: Symbol) -> bool {
        self.tombstones.as_ref().is_some_and(|t| t.contains(&key))
    }

    /// True if this env has a parent tier (i.e. it is a scoped child).
    #[inline(always)]
    pub(crate) fn is_scoped(&self) -> bool {
        self.parent.is_some()
    }

    /// True when this scoped env's overlay is *exactly* the process-shared
    /// empty singleton (see [`empty_overlay`]) with no tombstones: the state a
    /// fresh `scoped_child` starts in and leaves only on the first by-name
    /// write (`cow_mut`'s `Arc::make_mut` un-shares it) or `remove`. This is
    /// the dynamic "did the frame ever write env?" latch the light-call frame
    /// reuse (ADR-0004 J4d) keys on: `true` on return proves the body wrote
    /// nothing by name, with no static analysis involved.
    #[inline(always)]
    pub(crate) fn overlay_is_shared_empty(&self) -> bool {
        self.parent.is_some()
            && self.tombstones.is_none()
            && Arc::ptr_eq(&self.inner, empty_overlay_ref())
    }

    /// The by-name writes this env's frame tier has taken since the frame that
    /// owns it opened, or `None` when they are not recorded (see
    /// [`Self::flattened_for_frame`]). May hold a key more than once, and may
    /// name a key that is no longer present.
    #[inline(always)]
    pub(crate) fn frame_writes(&self) -> Option<&[Symbol]> {
        self.frame_writes.as_ref().map(|w| w.as_slice())
    }

    /// [`Self::flattened`] for the guard a full method dispatch runs before it
    /// can capture or iterate the env (`Interpreter::flatten_scoped_env`): the
    /// result is the same flat env, but it carries the collapsed tier's own
    /// by-name writes forward as a log.
    ///
    /// Why the log exists. The light-call frame-reuse unwind (ADR-0004 J4d) is
    /// O(callee writes) because a scoped env's overlay *is* the list of writes
    /// the frame made: everything in it is a callee write and everything else
    /// is out of reach in the parent tier. Flattening destroys exactly that
    /// distinction -- afterwards "the overlay" and "the whole visible scope" are
    /// the same map -- so the unwind fell back to scanning every visible name,
    /// resolving each `Symbol` to a string and asking `is_callee_local_sym`
    /// about it. Correct (a caller lexical is not a callee-local, so it
    /// survives) but O(scope) where O(callee writes) was intended, and one
    /// method dispatch put a frame in that state for the rest of its life
    /// (#7630).
    ///
    /// Recording the tier's key set here is O(overlay) -- the same size the
    /// merge itself would have been -- and [`Self::insert_sym`] /
    /// [`Self::remove_sym`] / [`Self::get_mut_sym`] extend it, so a write made
    /// *after* the flatten is logged too and the record stays complete. A bulk
    /// edit that cannot be logged key-by-key (`retain`, `values_mut`,
    /// `retain_overlay`) drops the log instead, which costs only the full scan
    /// this replaces.
    pub(crate) fn flattened_for_frame(&self) -> Self {
        // Only a scoped env has a frame tier to record: on a flat one `inner` is
        // already the whole scope, and logging that would claim every caller
        // lexical as this frame's write.
        debug_assert!(
            self.is_scoped(),
            "flattened_for_frame is the scoped-env collapse; a flat env has no tier to record"
        );
        let mut flat = self.flattened();
        let mut writes: Vec<Symbol> = self.inner.keys().copied().collect();
        if let Some(tomb) = &self.tombstones {
            // A `remove` in this tier is a write the frame made too: the unwind
            // has to consider the name even though the flatten already applied
            // the tombstone and the key is gone from the merged map.
            writes.extend(tomb.iter().copied());
        }
        flat.frame_writes = Some(Arc::new(writes));
        flat
    }

    /// Log a by-name write against [`Self::frame_writes`], for an env that is
    /// recording them. A no-op (one predictable branch) for every other env,
    /// which is nearly all of them.
    #[inline(always)]
    fn note_frame_write(&mut self, key: Symbol) {
        if let Some(log) = &mut self.frame_writes {
            Arc::make_mut(log).push(key);
        }
    }

    /// Drop the frame's own by-name writes from a *flattened* env, consulting
    /// [`Self::frame_writes`] instead of scanning the whole map: the
    /// [`Self::retain_overlay`] return merge, replayed at O(frame writes) after
    /// a [`Self::flattened_for_frame`]. `keep` sees each logged key; a key it
    /// rejects is removed, the rest stay and remain logged for the enclosing
    /// frame. Does nothing when this env carries no log.
    ///
    /// Unlike `retain_overlay` this touches only the names the frame wrote, so
    /// a caller lexical that merely *looks* like a per-frame private name (a
    /// `?`-prefixed contextual var of the caller's own, swept up by the flatten)
    /// is no longer dropped along with the callee's.
    pub(crate) fn retain_frame_writes(&mut self, mut keep: impl FnMut(Symbol) -> bool) -> bool {
        // Taken, not borrowed: the `remove_sym` below would otherwise log the
        // very keys it is removing, and taking it also leaves the `Arc`
        // uniquely owned so the filter below runs in place with no allocation.
        let Some(mut writes) = self.frame_writes.take() else {
            return false;
        };
        let log = Arc::make_mut(&mut writes);
        let mut i = 0;
        while i < log.len() {
            let k = log[i];
            if keep(k) {
                i += 1;
            } else {
                // Order does not matter: the log is a set of names to consider,
                // read once per unwind.
                log.swap_remove(i);
                self.remove_sym(k);
            }
        }
        self.frame_writes = Some(writes);
        true
    }

    /// Filter this env's overlay in place, keeping only entries `keep` accepts,
    /// and drop any tombstones. When the overlay ends up empty it is reset to
    /// the shared empty singleton so the [`Self::overlay_is_shared_empty`] latch
    /// re-arms for the next frame-reuse call. Used by the light-call frame
    /// reuse unwind: with the caller's overlay known-empty at entry, every
    /// surviving entry is a callee write, so this is exactly the scoped-overlay
    /// return merge performed in place.
    pub(crate) fn retain_overlay(&mut self, keep: impl FnMut(&Symbol, &mut Value) -> bool) {
        self.tombstones = None;
        let map = self.cow_mut();
        map.retain(keep);
        if map.is_empty() {
            self.inner = empty_overlay();
        }
        // A wholesale filter cannot be logged key-by-key, so the frame-write
        // record (if any) is dropped rather than left stale -- see
        // `flattened_for_frame`.
        self.frame_writes = None;
        self.refresh_file_sym();
    }

    /// Collapse a scoped env into a flat (`parent=None`) env. For a flat env
    /// this is the O(1) `Arc` clone; for a scoped env it materializes
    /// `parent` merged under `overlay` (overlay shadows parent) into a fresh flat
    /// overlay. The base tier is never materialized (it stays shared). Used at
    /// every boundary that captures/clones the env so no full-view iteration
    /// consumer is starved of parent lexicals.
    pub(crate) fn flattened(&self) -> Self {
        match &self.parent {
            None => self.clone(),
            // An overlay that never received a write (and holds no tombstone)
            // is invisible to lookups, so the parent's own flattening IS this
            // env's -- including `file_sym`, which `scoped_child` copies from
            // the parent for exactly this reason. Short-circuiting is not just
            // cheaper than the merge below, it is *free* when the parent is
            // already flat: an `Arc` bump instead of a whole-map clone, with
            // the copy deferred to the first write via `cow_mut`. This is the
            // same "empty tier is not a tier" rule `scoped_child` applies when
            // it chains over an empty parent instead of stacking on it.
            Some(parent) if self.inner.is_empty() && self.tombstones.is_none() => {
                let mut flat = parent.flattened();
                // The collapsed tier is this frame's and the parent's is the
                // caller's, so a frame-write log the parent happens to carry
                // describes the wrong frame: drop it. `flattened_for_frame`
                // installs this env's own (see #7630).
                flat.frame_writes = None;
                flat
            }
            Some(_) => {
                // Collapse the whole chain in ONE pass: clone the flat root
                // tier's map once, then layer every tier's tombstones and
                // overlay on top of it, root-ward first. Recursing through
                // `parent.flattened()` instead materialized a full copy of the
                // map at EVERY tier of the chain -- a method call two routine
                // frames deep paid two whole-scope clones for one flatten. The
                // base tier stays shared, never materialized, as before.
                let mut tiers: Vec<&Env> = Vec::new();
                let mut cur: &Env = self;
                while let Some(parent) = &cur.parent {
                    tiers.push(cur);
                    cur = parent;
                }
                let mut merged: SymMap = (*cur.inner).clone();
                for tier in tiers.into_iter().rev() {
                    // An overlay that never received a write (and holds no
                    // tombstone) is invisible to lookups.
                    if let Some(tomb) = &tier.tombstones {
                        for k in tomb {
                            merged.remove(k);
                        }
                    }
                    for (k, v) in tier.inner.iter() {
                        merged.insert(*k, v.clone());
                    }
                }
                Self {
                    inner: Arc::new(merged),
                    parent: None,
                    tombstones: None,
                    depth: 0,
                    // Flattening preserves every visible value, `?FILE` included.
                    file_sym: self.file_sym,
                    // The merged map is no longer any one frame's tier;
                    // `flattened_for_frame` is what records the collapsed tier's
                    // writes when a light frame needs them (#7630).
                    frame_writes: None,
                }
            }
        }
    }

    /// Build a flat env holding exactly the entries `keep` accepts, walking
    /// every tier base-to-leaf (parents first, then each frame's tombstones,
    /// then its overlay). The visible-per-key result is identical to
    /// `self.flattened()` followed by a filtered copy — a shadowing leaf entry
    /// overwrites (or, when rejected, removes) the parent's entry, and a
    /// tombstoned key does not survive — but no intermediate whole-map clone
    /// is materialized. This is the closure-capture fast path: `flattened()`
    /// deep-clones the entire parent-chain map per call, which dominated
    /// lambda creation inside method frames (`todo/deep/closure-env-capture-cost.md`).
    /// The base tier (`GLOBAL_BASE`) is — as in `flattened()` — never
    /// materialized; it stays reachable through the flat env's tail lookup.
    pub(crate) fn filtered_flat(&self, keep: &dyn Fn(Symbol, &Value) -> bool) -> Env {
        fn collect(env: &Env, out: &mut SymMap, keep: &dyn Fn(Symbol, &Value) -> bool) {
            if let Some(parent) = &env.parent {
                collect(parent, out, keep);
            }
            if let Some(tomb) = &env.tombstones {
                for k in tomb {
                    out.remove(k);
                }
            }
            for (k, v) in env.inner.iter() {
                if keep(*k, v) {
                    out.insert(*k, v.clone());
                } else {
                    out.remove(k);
                }
            }
        }
        // Pre-size from the chain's total overlay count. That is an upper
        // bound on the result (`keep` can only reject entries, and a shadowing
        // leaf entry overwrites a parent's rather than adding to it), so the
        // map is allocated once instead of growing through hashbrown's
        // `reserve_rehash` ladder. This runs per closure creation and the
        // capture memo cannot help a closure created inside a *method* frame
        // (its env is fresh on every call, so the tier addresses never repeat)
        // -- on `benchmarks/bench-ctor.raku` that is 31 inserts into a map
        // starting at zero capacity, i.e. five reallocations and ~52 entry
        // moves, on every construction.
        let mut cap = 0usize;
        {
            let mut cur = Some(self);
            while let Some(env) = cur {
                cap += env.inner.len();
                cur = env.parent.as_deref();
            }
        }
        let mut out = SymMap::with_capacity_and_hasher(cap, Default::default());
        collect(self, &mut out, keep);
        // `keep` may have rejected `?FILE`, so re-derive rather than inherit.
        let file_sym = out.get(&file_key()).and_then(file_sym_of);
        Self {
            inner: Arc::new(out),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym,
            frame_writes: None,
        }
    }

    /// Overlay-only iterator: yields exactly this frame's own writes (the
    /// callee's overlay), excluding parent and base tiers. This is what a
    /// merge-back wants -- the keys the callee actually wrote.
    pub(crate) fn overlay_iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.inner.iter()
    }

    /// Overlay-only lookup: like `get`, but never walks the parent chain or
    /// base tier. Used where a caller-frame value of the same name must not
    /// be mistaken for a write this frame made itself (see `reconcile_attrs`).
    pub(crate) fn overlay_get(&self, key: &str) -> Option<&Value> {
        self.inner.get(&Symbol::intern(key))
    }

    /// Symbol-keyed twin of [`Self::overlay_get`] — avoids re-interning on a hot
    /// per-opcode read that already has a pre-interned `Symbol` in hand (see
    /// `exec_get_local_op`'s lazy-sync check).
    pub(crate) fn overlay_get_sym(&self, key: Symbol) -> Option<&Value> {
        self.inner.get(&key)
    }

    /// Check whether two `Env` values point to the same underlying overlay map.
    /// Note: this compares the overlay only; two scoped envs sharing an overlay
    /// but differing in parent would compare equal (callers that rely on ptr_eq
    /// to detect "no writes happened" only ever use it on flat envs).
    #[allow(dead_code)]
    pub(crate) fn ptr_eq(&self, other: &Self) -> bool {
        Arc::ptr_eq(&self.inner, &other.inner)
    }

    #[inline(always)]
    pub fn get(&self, key: &str) -> Option<&Value> {
        self.get_sym(Symbol::intern(key))
    }

    /// [`Self::get`] for a caller that *may* already hold the key's `Symbol` —
    /// a compiled slot whose `locals_sym` entry exists, or a hand-built chunk
    /// where it does not. Saves the re-hash on the common (pre-interned) side
    /// without forcing every such call site to spell the `match` out (#7736).
    #[inline]
    pub(crate) fn get_for(&self, key: &str, key_sym: Option<Symbol>) -> Option<&Value> {
        match key_sym {
            Some(sym) => {
                debug_assert_eq!(sym, Symbol::intern(key));
                self.get_sym(sym)
            }
            None => self.get(key),
        }
    }

    #[inline]
    pub fn get_sym(&self, key: Symbol) -> Option<&Value> {
        if let Some(v) = self.inner.get(&key) {
            return Some(v);
        }
        // A tombstoned key is deleted in this scope: do not fall through to the
        // parent/base tier.
        if self.is_tombstoned(key) {
            return None;
        }
        // Walk the parent chain (each tier may itself be scoped). The chain
        // bottoms out at a flat env, which consults GLOBAL_BASE; intermediate
        // tiers do not, so the base is checked exactly once.
        if let Some(parent) = &self.parent {
            return parent.get_sym(key);
        }
        global_base().and_then(|b| b.get(&key))
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.contains_key_sym(Symbol::intern(key))
    }

    #[inline]
    pub fn contains_key_sym(&self, key: Symbol) -> bool {
        if self.inner.contains_key(&key) {
            return true;
        }
        if self.is_tombstoned(key) {
            return false;
        }
        if let Some(parent) = &self.parent {
            return parent.contains_key_sym(key);
        }
        global_base().is_some_and(|b| b.contains_key(&key))
    }

    /// True if `key` is declared in THIS frame's own overlay tier specifically
    /// -- unlike [`contains_key`](Self::contains_key)/[`contains_key_sym`](Self::contains_key_sym),
    /// this does NOT walk the parent chain or fall through to the global base.
    ///
    /// A saved call frame's "propagate a promoted `ContainerRef` cell to every
    /// ancestor frame that owns this lexical" writeback (the `:=`/box-on-capture
    /// paths in `vm_var_assign_set_local.rs`/`vm_var_assign_coerce.rs`/
    /// `vm_exec_dispatch.rs`/`vm_env_helpers.rs`) must use this, not the
    /// chain-walking `contains_key`: a frame merely being able to *see* a name
    /// through its parent chain does not mean that frame *declares* it, and for
    /// a name that is essentially always chain-visible (the topic `$_`, `$!`)
    /// the chain-walking check is true for nearly every frame on the call
    /// stack, corrupting each one's own overlay with the bound cell.
    #[inline]
    pub fn contains_key_own_tier(&self, key: &str) -> bool {
        self.inner.contains_key(&Symbol::intern(key))
    }

    /// Copy-on-write access to the inner map for mutation. Equivalent to
    /// `Arc::make_mut`, but when stats are enabled it records an actual
    /// O(env_size) deep copy whenever the env is shared (the real dual-store
    /// cost; see docs/vm-dual-store.md and `vm_stats::record_env_deep_copy`).
    #[inline]
    fn cow_mut(&mut self) -> &mut SymMap {
        if crate::vm::vm_stats::enabled() && Arc::strong_count(&self.inner) > 1 {
            crate::vm::vm_stats::record_env_deep_copy();
        }
        Arc::make_mut(&mut self.inner)
    }

    /// Reserve room in this env's own overlay for `additional` more entries.
    ///
    /// A method frame's overlay starts empty and immediately takes a known,
    /// fixed set of entry-time writes (`self`, `?CLASS`, the topic, `$!`, the
    /// callable id, then one per bound parameter), which otherwise walk
    /// hashbrown's `reserve_rehash` growth ladder — 0 -> 3 -> 7 -> 14 — on
    /// every single call. The caller knows the count, so it can pay one
    /// allocation instead. Takes the copy-on-write path exactly as the first
    /// `insert` would, so this does not bring a deep copy forward.
    pub(crate) fn reserve(&mut self, additional: usize) {
        self.cow_mut().reserve(additional);
    }

    /// Clear a tombstone for `key` (it is being re-inserted, so it is no longer
    /// deleted in this scope). No-op for flat envs / un-tombstoned keys.
    #[inline(always)]
    fn untombstone(&mut self, key: Symbol) {
        if let Some(t) = &mut self.tombstones {
            t.remove(&key);
        }
    }

    #[inline]
    pub fn insert(&mut self, key: String, value: Value) -> Option<Value> {
        note_env_key(&key);
        let sym = Symbol::intern(&key);
        self.insert_sym(sym, value)
    }

    /// [`Self::insert_sym`] for a caller whose key symbol was pre-interned but
    /// whose key may belong to one of the families [`note_env_key`] latches
    /// (a `__mutsu_*` metadata key, a `^`-twigil placeholder).
    ///
    /// The plain symbol entry point deliberately skips that latch — its callers
    /// pass ordinary lexical names, none of which arm a flag — so a site that
    /// replaces a by-name [`Self::insert`] must come here instead to keep the
    /// flags monotonic. Resolving the symbol is a cached array read, so this is
    /// still far cheaper than rebuilding the `String` key and re-interning it
    /// (#7736).
    #[inline]
    pub(crate) fn insert_sym_noting(&mut self, key: Symbol, value: Value) -> Option<Value> {
        note_env_key(key.as_str());
        self.insert_sym(key, value)
    }

    #[inline]
    pub fn insert_sym(&mut self, key: Symbol, value: Value) -> Option<Value> {
        if key == file_key() {
            self.file_sym = file_sym_of(&value);
        }
        // Latch the "someone rebound `&return`" flag here (see
        // [`RETURN_REBOUND_SEEN`]): this is the single funnel every env insert
        // passes through, so no creation site can slip past it.
        if key == crate::symbol::wk::rebound_return() {
            RETURN_REBOUND_SEEN.store(true, Ordering::Relaxed);
        }
        self.untombstone(key);
        self.note_frame_write(key);
        self.cow_mut().insert(key, value)
    }

    /// Assign a *value* to `key`, writing **through** a shared `ContainerRef`
    /// cell if the name is currently bound to one. [`insert`](Self::insert)
    /// replaces the binding itself, which is right for `:=` and for a fresh
    /// `my`, but wrong for the runtime writeback paths (`$o.attr = v`,
    /// `$s.substr-rw(...) = v`, ...): those assign to the container the name
    /// already denotes, so replacing a cell with a bare value silently
    /// un-shares every alias of it.
    ///
    /// This is what made a `supply` block's lexical revert between `whenever`
    /// invocations: the block's `my $request` is promoted to a cell so all the
    /// callbacks share one binding (`share_supply_block_lexicals`), but
    /// `$request.method = ...` in the callback body replaced that cell with a
    /// plain value in the callback's own env. A nested `sub`'s later
    /// `$request = Request.new` then wrote the cell nobody read any more, so
    /// the next invocation saw the previous request object and appended the
    /// second request's headers to it (Cro could not serve pipelined requests).
    pub fn insert_through(&mut self, key: String, value: Value) {
        note_env_key(&key);
        self.insert_through_sym(Symbol::intern(&key), value);
    }

    /// Symbol-keyed [`insert_through`](Self::insert_through).
    pub fn insert_through_sym(&mut self, key: Symbol, value: Value) {
        if let Some(existing) = self.get_sym(key)
            && let crate::value::ValueView::ContainerRef(cell) = existing.view()
        {
            let cell = cell.clone();
            if key == file_key() {
                self.file_sym = file_sym_of(&value);
            }
            *cell.lock().unwrap() = value;
            return;
        }
        self.insert_sym(key, value);
    }

    pub fn remove(&mut self, key: &str) -> Option<Value> {
        self.remove_sym(Symbol::intern(key))
    }

    pub fn remove_sym(&mut self, key: Symbol) -> Option<Value> {
        if key == file_key() {
            // Flat: the key is gone. Scoped: the tombstone below stops the
            // parent tier shadowing through. Either way nothing is visible.
            self.file_sym = None;
        }
        // Absent from a flat env's overlay: nothing to remove and no parent tier
        // to tombstone, so the result is `None` either way. Bail before
        // `cow_mut()`, whose `Arc::make_mut` costs an atomic RMW -- and a full
        // map clone whenever the env is shared (a live closure/`saved_env` holds
        // a handle). Every `my` declaration speculatively removes several
        // metadata keys that the common program never creates, so this no-op
        // removal is on the hottest declaration path in the VM.
        if self.parent.is_none() && !self.inner.contains_key(&key) {
            return None;
        }
        self.note_frame_write(key);
        let from_overlay = self.cow_mut().remove(&key);
        // Scoped env: if the key still exists in the parent/base tier, record a
        // tombstone so it stops shadowing through. The visible value before
        // removal is the overlay value if present, else the parent/base value.
        if self.parent.is_some() {
            // Chain-aware lookup (covers the base tier at the chain tail).
            let parent_val = self
                .parent
                .as_ref()
                .and_then(|p| p.get_sym(key))
                .or_else(|| global_base().and_then(|b| b.get(&key)))
                .cloned();
            if parent_val.is_some() {
                let visible = from_overlay.or(parent_val);
                self.tombstones
                    .get_or_insert_with(rustc_hash::FxHashSet::default)
                    .insert(key);
                return visible;
            }
        }
        from_overlay
    }

    pub fn get_mut(&mut self, key: &str) -> Option<&mut Value> {
        self.get_mut_sym(Symbol::intern(key))
    }

    pub fn get_mut_sym(&mut self, key: Symbol) -> Option<&mut Value> {
        // Promote a parent-tier or base-only key into the overlay before handing
        // out a mutable reference, so a write to a caller lexical (scoped env) or
        // a built-in constant lands in this frame's overlay and is not silently
        // lost. Common case (overlay hit, or absent) pays nothing extra. A
        // tombstoned key is deleted in this scope, so it is not promoted.
        if !self.inner.contains_key(&key) {
            if self.is_tombstoned(key) {
                return None;
            }
            // Promote from the parent chain (chain-aware get_sym already covers
            // the base tier at the chain tail) so a write to a caller lexical
            // lands in this frame's overlay and is not silently lost.
            let promote = self
                .parent
                .as_ref()
                .and_then(|p| p.get_sym(key))
                .or_else(|| global_base().and_then(|b| b.get(&key)))
                .cloned();
            if let Some(v) = promote {
                self.untombstone(key);
                self.cow_mut().insert(key, v);
            }
        }
        // Handing out `&mut` is a write to this key whatever the caller does
        // with it, so it is logged like an `insert` (see `frame_writes`).
        self.note_frame_write(key);
        self.cow_mut().get_mut(&key)
    }

    pub fn retain<F>(&mut self, f: F)
    where
        F: FnMut(&Symbol, &mut Value) -> bool,
    {
        self.cow_mut().retain(f);
        // Not loggable key-by-key: drop the frame-write record rather than
        // leave it stale (see `flattened_for_frame`).
        self.frame_writes = None;
        self.refresh_file_sym();
    }

    pub fn iter(&self) -> std::collections::hash_map::Iter<'_, Symbol, Value> {
        self.inner.iter()
    }

    pub fn keys(&self) -> std::collections::hash_map::Keys<'_, Symbol, Value> {
        self.inner.keys()
    }

    pub fn values(&self) -> std::collections::hash_map::Values<'_, Symbol, Value> {
        self.inner.values()
    }

    pub fn values_mut(&mut self) -> std::collections::hash_map::ValuesMut<'_, Symbol, Value> {
        // Every value in the map is about to be writable and none of the writes
        // names a key: drop the frame-write record (see `flattened_for_frame`).
        self.frame_writes = None;
        self.cow_mut().values_mut()
    }

    /// Whether this env's overlay map (`inner`) is uniquely owned by this `Env`
    /// handle — i.e. no other `Env` clone (another closure's capture, a saved
    /// call frame, the live interpreter env) shares the same `Arc<SymMap>`.
    ///
    /// GC edge rule (see `value_gc::uniquely_owned`): a `Trace` impl may claim
    /// the map's contained `Gc` handles as its own edges ONLY when it is the
    /// map's sole holder. The map owns each handle once, so N sharers each
    /// tracing it would report N phantom edge sets and over-decrement live
    /// nodes during trial deletion — a false reclaim that empties live data
    /// (observed: `$*PROGRAM`'s IO::Path attrs wiped in
    /// roast/S11-modules/require.t under `MUTSU_GC=on`).
    #[inline]
    pub(crate) fn gc_overlay_uniquely_owned(&self) -> bool {
        Arc::strong_count(&self.inner) == 1
    }

    /// GC root enumeration (ADR-0001/0002, `docs/gc-level1-detailed-design.md`
    /// §2.2/§11 step 1): visit every `Value` reachable from this env, including
    /// the `parent` overlay chain. Unlike `iter`/`keys`/`values` (overlay-only,
    /// by design — see the `flatten` doc comment above), a GC root scan must see
    /// the whole chain: a scoped child env's parent tier can be the only
    /// reference keeping a container alive.
    ///
    /// Only called from `Interpreter::visit_roots` tests for now (GC Level 1a
    /// step 1); becomes a live production call once the collector (step 4)
    /// lands.
    #[allow(dead_code)]
    pub(crate) fn visit_values(&self, visitor: &mut dyn crate::gc::RootVisitor) {
        for v in self.inner.values() {
            visitor.visit_value(v);
        }
        if let Some(parent) = &self.parent {
            parent.visit_values(visitor);
        }
    }

    /// Full name->value snapshot, merging the immutable base tier under the
    /// overlay (overlay shadows base). Used where a complete view of every
    /// reachable name is required (serialization / cross-context copy), unlike
    /// `iter`/`keys`/`values`, which expose only the mutable overlay.
    pub fn flatten(&self) -> HashMap<String, Value> {
        // Build the parent view first (the chain tail seeds GLOBAL_BASE), then
        // layer this frame's tombstones and overlay on top.
        let mut out: HashMap<String, Value> = match &self.parent {
            Some(parent) => parent.flatten(),
            None => global_base()
                .map(|b| b.iter().map(|(k, v)| (k.resolve(), v.clone())).collect())
                .unwrap_or_default(),
        };
        if let Some(tomb) = &self.tombstones {
            for k in tomb {
                out.remove(&k.resolve());
            }
        }
        for (k, v) in self.inner.iter() {
            out.insert(k.resolve(), v.clone());
        }
        out
    }

    /// Every name VISIBLE from this env whose resolved key satisfies `keep`,
    /// walking the parent chain and the immutable base tier the way
    /// [`Self::get`] does — unlike [`Self::keys`], which exposes only this
    /// tier's overlay.
    ///
    /// Key-only, so it costs no `Value` clones: [`Self::flatten`] answers the
    /// same question but deep-copies every value, which is far too expensive
    /// for a per-`EVAL` snapshot.
    pub fn visible_keys_where(&self, keep: impl Fn(&str) -> bool + Copy) -> HashSet<String> {
        let mut out: HashSet<String> = match &self.parent {
            Some(parent) => parent.visible_keys_where(keep),
            None => global_base()
                .map(|b| {
                    b.keys()
                        .map(|k| k.resolve())
                        .filter(|k| keep(k))
                        .collect::<HashSet<String>>()
                })
                .unwrap_or_default(),
        };
        if let Some(tomb) = &self.tombstones {
            for k in tomb {
                out.remove(&k.resolve());
            }
        }
        for k in self.inner.keys() {
            let name = k.resolve();
            if keep(&name) {
                out.insert(name);
            }
        }
        out
    }

    pub fn len(&self) -> usize {
        self.inner.len()
    }

    #[allow(dead_code)]
    pub fn is_empty(&self) -> bool {
        self.inner.is_empty()
    }

    /// Insert only if key is not present (in overlay or the base tier).
    pub fn entry_or_insert(&mut self, key: String, value: Value) {
        self.entry_or_insert_sym(Symbol::intern(&key), value);
    }

    /// Insert only if key is not present, keyed directly by an interned Symbol.
    /// Avoids the `resolve()` (Symbol -> String) + re-intern round trip that
    /// `entry_or_insert` pays when the caller already holds a Symbol.
    pub fn entry_or_insert_sym(&mut self, key: Symbol, value: Value) {
        if !self.contains_key_sym(key) {
            self.insert_sym(key, value);
        }
    }

    /// [`Self::entry_or_insert_sym`] with the value produced only on a miss.
    ///
    /// The closure-call captured-env merge runs this once per captured entry —
    /// ~45 per call on a body whose creating scope was wide — and the eager
    /// `v.clone()` at the call site paid a GC refcount bump *and* the matching
    /// drop for every entry the caller already had (#7571). The lazy form pays
    /// the clone only where it is actually stored.
    pub fn entry_or_insert_sym_with<F: FnOnce() -> Value>(&mut self, key: Symbol, f: F) {
        if !self.contains_key_sym(key) {
            self.insert_sym(key, f());
        }
    }

    /// Insert only if key is not present (lazy value).
    pub fn entry_or_insert_with<F: FnOnce() -> Value>(&mut self, key: String, f: F) {
        let sym = Symbol::intern(&key);
        if !self.contains_key_sym(sym) {
            self.insert_sym(sym, f());
        }
    }

    /// Direct access to the inner HashMap (for bulk mutation).
    #[allow(dead_code)]
    pub(crate) fn inner_mut(&mut self) -> &mut SymMap {
        self.cow_mut()
    }

    /// Direct read access to the inner HashMap (this env's OWN tier only — it
    /// does NOT see the parent chain, unlike `get`/`contains_key_sym`).
    pub(crate) fn inner(&self) -> &SymMap {
        &self.inner
    }

    /// Non-owning identity of this env's tier chain: the address of each tier's
    /// overlay map, leaf first. `None` when the chain is not describable this
    /// way -- it carries tombstones (a plain `FxHashSet` this cannot pin, so a
    /// removal would be invisible to the comparison) or is deeper than
    /// [`MAX_IDENTITY_TIERS`].
    ///
    /// Addresses alone are a *heuristic*: a dropped map's allocation can be
    /// recycled at the same address. Pair them with [`Self::tier_maps`], which
    /// holds each tier's `Arc` and so keeps the addresses from being recycled,
    /// before treating a match as proof that the contents are unchanged. See
    /// `Interpreter::capture_closure_env`.
    pub(crate) fn tier_addrs(&self) -> Option<TierAddrs> {
        let mut addrs = [0usize; MAX_IDENTITY_TIERS];
        let mut len = 0usize;
        let mut cur = self;
        loop {
            if cur.tombstones.is_some() || len == MAX_IDENTITY_TIERS {
                return None;
            }
            addrs[len] = Arc::as_ptr(&cur.inner) as usize;
            len += 1;
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        Some(TierAddrs { addrs, len })
    }

    /// Owning twin of [`Self::tier_addrs`]: an `Arc` handle on every tier's
    /// overlay map, leaf first. Holding these pins the addresses (nothing can
    /// be freed and re-allocated at one of them) AND forces copy-on-write on
    /// the next by-name write to any tier, so an address match proves the
    /// visible contents are byte-for-byte the ones that were there before.
    pub(crate) fn tier_maps(&self) -> Vec<Arc<SymMap>> {
        let mut maps = Vec::new();
        let mut cur = self;
        loop {
            maps.push(Arc::clone(&cur.inner));
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        maps
    }
}

/// Longest tier chain [`Env::tier_addrs`] describes. Deeper chains are simply
/// reported as un-identifiable; [`MAX_OVERLAY_DEPTH`] bounds the chain anyway
/// and ordinary nesting is a handful of tiers.
const MAX_IDENTITY_TIERS: usize = 8;

/// The address of each tier's overlay map in one env chain -- see
/// [`Env::tier_addrs`].
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) struct TierAddrs {
    addrs: [usize; MAX_IDENTITY_TIERS],
    len: usize,
}

impl TierAddrs {
    /// True when `maps` (an owning [`Env::tier_maps`] snapshot) is exactly the
    /// chain these addresses describe.
    pub(crate) fn matches(&self, maps: &[Arc<SymMap>]) -> bool {
        maps.len() == self.len
            && maps
                .iter()
                .zip(&self.addrs[..self.len])
                .all(|(map, addr)| Arc::as_ptr(map) as usize == *addr)
    }
}

impl Default for Env {
    fn default() -> Self {
        Self::new()
    }
}

impl From<HashMap<String, Value>> for Env {
    fn from(map: HashMap<String, Value>) -> Self {
        let sym_map: SymMap = map
            .into_iter()
            .map(|(k, v)| (Symbol::intern(&k), v))
            .collect();
        let file_sym = sym_map.get(&file_key()).and_then(file_sym_of);
        Self {
            inner: Arc::new(sym_map),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym,
            frame_writes: None,
        }
    }
}

impl From<HashMap<Symbol, Value>> for Env {
    fn from(map: HashMap<Symbol, Value>) -> Self {
        let map: SymMap = map.into_iter().collect();
        let file_sym = map.get(&file_key()).and_then(file_sym_of);
        Self {
            inner: Arc::new(map),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym,
            frame_writes: None,
        }
    }
}

impl fmt::Debug for Env {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.inner.fmt(f)
    }
}

impl<'a> IntoIterator for &'a Env {
    type Item = (&'a Symbol, &'a Value);
    type IntoIter = std::collections::hash_map::Iter<'a, Symbol, Value>;

    fn into_iter(self) -> Self::IntoIter {
        self.inner.iter()
    }
}

impl IntoIterator for Env {
    type Item = (Symbol, Value);
    type IntoIter = std::collections::hash_map::IntoIter<Symbol, Value>;

    fn into_iter(self) -> Self::IntoIter {
        Arc::try_unwrap(self.inner)
            .unwrap_or_else(|arc| (*arc).clone())
            .into_iter()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn s(name: &str) -> Symbol {
        Symbol::intern(name)
    }

    #[test]
    fn plain_user_lexical_classification() {
        // Plain user lexicals (droppable by the closure upvalue capture): a
        // lowercase-identifier scalar (stored sigil-less) or @/%/&-sigiled var.
        for k in ["x", "c", "count", "@arr", "%h", "&helper", "longname"] {
            assert!(
                is_plain_user_lexical(k),
                "{k} should be a plain user lexical"
            );
        }
        // System names the capture must keep: self, topic/match/error, compile-time
        // `?` vars, dynamic `*` vars, match captures, &?ROUTINE, types, meta.
        for k in [
            "self",
            "_",
            "/",
            "!",
            "?FILE",
            "?LINE",
            "?CLASS",
            "*REPO",
            "@*ARGS",
            "%*ENV",
            "$*HOME",
            "0",
            "1",
            "<name>",
            "&?ROUTINE",
            "&?BLOCK",
            "@_",
            "Cache",
            "Any",
            "__mutsu_type::x",
        ] {
            assert!(
                !is_plain_user_lexical(k),
                "{k} should be kept (not a plain user lexical)"
            );
        }
        // Empty string is not a plain user lexical.
        assert!(!is_plain_user_lexical(""));
    }

    fn scoped_with(parent: Env, writes: &[(&str, i64)]) -> Env {
        let mut e = Env::scoped_child(parent);
        for (k, v) in writes {
            e.insert(k.to_string(), Value::int(*v));
        }
        e
    }

    #[test]
    fn chain_lookup_reads_through_all_tiers() {
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let mid = scoped_with(root, &[("b", 2)]);
        let leaf = scoped_with(mid, &[("c", 3)]);
        // Each tier's key is visible from the leaf.
        assert_eq!(leaf.get_sym(s("a")), Some(&Value::int(1)));
        assert_eq!(leaf.get_sym(s("b")), Some(&Value::int(2)));
        assert_eq!(leaf.get_sym(s("c")), Some(&Value::int(3)));
        assert!(leaf.get_sym(s("missing")).is_none());
        assert!(leaf.contains_key_sym(s("a")));
        assert!(!leaf.contains_key_sym(s("missing")));
    }

    #[test]
    fn overlay_shadows_parent() {
        let mut root = Env::new();
        root.insert("x".into(), Value::int(1));
        let leaf = scoped_with(root, &[("x", 99)]);
        assert_eq!(leaf.get_sym(s("x")), Some(&Value::int(99)));
    }

    #[test]
    fn iter_is_overlay_only() {
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let leaf = scoped_with(root, &[("b", 2)]);
        let keys: Vec<String> = leaf.iter().map(|(k, _)| k.resolve()).collect();
        // Only the leaf's own write, not the parent's `a`.
        assert_eq!(keys, vec!["b".to_string()]);
    }

    #[test]
    fn tombstone_hides_parent_key_through_chain() {
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let mut leaf = Env::scoped_child(root);
        let removed = leaf.remove("a");
        assert_eq!(removed, Some(Value::int(1)));
        // Hidden in this scope, but not deleted from the parent tier.
        assert!(leaf.get_sym(s("a")).is_none());
        assert!(!leaf.contains_key_sym(s("a")));
        // Re-inserting clears the tombstone.
        leaf.insert("a".into(), Value::int(5));
        assert_eq!(leaf.get_sym(s("a")), Some(&Value::int(5)));
    }

    #[test]
    fn tier_addrs_match_only_while_the_held_maps_are_untouched() {
        // The closure-capture memo reuses a captured env whenever the tier
        // ADDRESSES still match the `Arc`s it holds. That is only sound because
        // holding those `Arc`s makes `cow_mut`'s `Arc::make_mut` clone before
        // any by-name write, so a written tier necessarily moves — which is
        // exactly what this pins.
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let mut leaf = Env::scoped_child(root);
        leaf.insert("b".into(), Value::int(2));

        let held = leaf.tier_maps();
        assert_eq!(held.len(), 2, "leaf overlay plus the root tier");
        let addrs = leaf.tier_addrs().expect("no tombstones, shallow chain");
        assert!(
            addrs.matches(&held),
            "an untouched chain keeps its addresses"
        );
        // A read must not disturb them.
        assert_eq!(leaf.get_sym(s("a")), Some(&Value::int(1)));
        assert!(addrs.matches(&held));

        // Writing to the leaf moves it, so the recorded addresses stop matching.
        leaf.insert("c".into(), Value::int(3));
        let after = leaf.tier_addrs().expect("still no tombstones");
        assert!(
            !after.matches(&held),
            "a by-name write must break the address match"
        );
    }

    #[test]
    fn tier_addrs_refuses_a_tombstoned_chain() {
        // Tombstones are a plain `FxHashSet` the memo cannot pin, so a chain
        // carrying one is reported as un-identifiable rather than compared.
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        let mut leaf = Env::scoped_child(root);
        assert!(leaf.tier_addrs().is_some());
        leaf.remove("a");
        assert!(leaf.tier_addrs().is_none());
    }

    #[test]
    fn flattened_preserves_full_view_and_tombstones() {
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        root.insert("gone".into(), Value::int(7));
        let mid = scoped_with(root, &[("b", 2)]);
        let mut leaf = mid;
        leaf.insert("c".into(), Value::int(3));
        leaf.remove("gone");
        let flat = leaf.flattened();
        assert!(!flat.is_scoped());
        assert_eq!(flat.depth, 0);
        assert_eq!(flat.get_sym(s("a")), Some(&Value::int(1)));
        assert_eq!(flat.get_sym(s("b")), Some(&Value::int(2)));
        assert_eq!(flat.get_sym(s("c")), Some(&Value::int(3)));
        assert!(flat.get_sym(s("gone")).is_none());
    }

    #[test]
    fn empty_tiers_are_reused_not_stacked() {
        // A scoped parent whose overlay never received a write is invisible to
        // lookups; scoped_child must chain over its parent instead of stacking
        // (pure recursion then runs at constant depth — no flatten churn).
        let mut root = Env::new();
        root.insert("root".into(), Value::int(42));
        let mut env = Env::scoped_child(root);
        for _ in 0..(MAX_OVERLAY_DEPTH as usize * 4) {
            env = Env::scoped_child(env);
            assert_eq!(env.depth, 1, "empty tiers must not grow the chain");
        }
        assert_eq!(env.get_sym(s("root")), Some(&Value::int(42)));
        // A written tier is NOT skipped: its entry stays visible in the child.
        let written = scoped_with(env, &[("b", 2)]);
        let child = Env::scoped_child(written);
        assert_eq!(child.depth, 2);
        assert_eq!(child.get_sym(s("b")), Some(&Value::int(2)));
        // A tombstoned (remove-only) tier is NOT skipped either.
        let mut root2 = Env::new();
        root2.insert("a".into(), Value::int(1));
        let mut tomb = Env::scoped_child(root2);
        tomb.remove("a");
        let child2 = Env::scoped_child(tomb);
        assert_eq!(child2.depth, 2);
        assert!(child2.get_sym(s("a")).is_none());
    }

    #[test]
    fn filtered_flat_chain_walk_respects_tombstones_and_shadowing() {
        // `filtered_flat` is the chain-aware enumeration primitive ADR-0035
        // Mechanism 1 relies on: a whole-env walk must see exactly the same
        // entries `Env::get()` would for every key -- outermost tier first,
        // nearer tiers overwriting, tombstoned keys suppressed. This is what
        // `dynamic_pseudo_stash_entries` uses (instead of `iter()`, which is
        // top-overlay-only) to enumerate `PROCESS::`/`DYNAMIC::` dynamics
        // through the whole overlay-parent chain of every stacked caller env.
        let mut root = Env::new();
        root.insert("a".into(), Value::int(1));
        root.insert("shadowed".into(), Value::int(100));
        let mut mid = Env::scoped_child(root);
        mid.insert("shadowed".into(), Value::int(2)); // shadows root's entry
        mid.insert("b".into(), Value::int(3));
        let mut leaf = Env::scoped_child(mid);
        leaf.remove("a"); // tombstoned: must not survive into the merged view
        leaf.insert("c".into(), Value::int(4));

        let merged = leaf.filtered_flat(&|_, _| true);
        assert!(!merged.is_scoped());
        assert!(
            merged.get_sym(s("a")).is_none(),
            "tombstoned key must be suppressed"
        );
        assert_eq!(
            merged.get_sym(s("shadowed")),
            Some(&Value::int(2)),
            "nearer tier must shadow the outer one"
        );
        assert_eq!(merged.get_sym(s("b")), Some(&Value::int(3)));
        assert_eq!(merged.get_sym(s("c")), Some(&Value::int(4)));
        // The merged view agrees with per-key `get_sym` on the source chain
        // for every key -- the coherence property the helper exists for.
        for key in ["a", "b", "c", "shadowed", "missing"] {
            assert_eq!(
                merged.get_sym(s(key)),
                leaf.get_sym(s(key)),
                "filtered_flat must agree with get_sym for {key:?}"
            );
        }
    }

    #[test]
    fn depth_is_bounded_under_deep_nesting() {
        // Chaining far past MAX_OVERLAY_DEPTH must keep the chain length bounded
        // (scoped_child flattens the parent at the limit) while still reading the
        // deepest lexical correctly.
        let mut env = Env::new();
        env.insert("root".into(), Value::int(42));
        for i in 0..(MAX_OVERLAY_DEPTH as i64 * 4) {
            env = scoped_with(env, &[("tmp", i)]);
            assert!(
                env.depth <= MAX_OVERLAY_DEPTH,
                "depth {} exceeded bound {}",
                env.depth,
                MAX_OVERLAY_DEPTH
            );
        }
        // The original root lexical is still reachable through the flattened tiers.
        assert_eq!(env.get_sym(s("root")), Some(&Value::int(42)));
    }

    #[test]
    fn flattened_for_frame_carries_the_tier_writes_as_a_log() {
        // The collapse is what the light-call unwind loses its footing on: after
        // it, "the overlay" and "the whole visible scope" are one map. The log is
        // what keeps the frame's own writes distinguishable (#7630).
        let mut caller = Env::new();
        caller.insert("caller-lex".into(), Value::int(1));
        let leaf = scoped_with(caller, &[("mine", 2)]);
        assert!(
            leaf.frame_writes().is_none(),
            "a scoped tier IS its own log"
        );

        let flat = leaf.flattened_for_frame();
        assert!(!flat.is_scoped(), "the guard needs a flat env");
        assert_eq!(flat.get_sym(s("caller-lex")), Some(&Value::int(1)));
        assert_eq!(flat.frame_writes(), Some(&[s("mine")][..]));
    }

    #[test]
    fn a_write_after_the_flatten_is_logged_too() {
        let mut caller = Env::new();
        caller.insert("caller-lex".into(), Value::int(1));
        let mut flat = scoped_with(caller, &[("before", 2)]).flattened_for_frame();
        flat.insert("after".into(), Value::int(3));
        flat.remove("gone");

        let logged: Vec<String> = flat
            .frame_writes()
            .expect("still recording")
            .iter()
            .map(|k| k.resolve())
            .collect();
        assert!(logged.contains(&"before".to_string()));
        assert!(logged.contains(&"after".to_string()));
        // `remove` of an absent key on a flat env is a no-op, so it is not a write.
        assert!(!logged.contains(&"gone".to_string()));
        // The caller lexical the flatten swept in is NOT this frame's write.
        assert!(!logged.contains(&"caller-lex".to_string()));
    }

    #[test]
    fn retain_frame_writes_drops_only_the_frames_own_names() {
        let mut caller = Env::new();
        caller.insert("caller-lex".into(), Value::int(1));
        caller.insert("?FILE".into(), Value::str("outer.raku".to_string()));
        let mut flat = scoped_with(caller, &[("mine", 2), ("escapes", 3)]).flattened_for_frame();

        let ran = flat.retain_frame_writes(|k| k != "mine");
        assert!(ran, "a flattened frame env reports its log");
        // The frame's own local is gone; its captured-outer write stays, and so
        // does everything the flatten merely swept in from the caller.
        assert!(flat.get_sym(s("mine")).is_none());
        assert_eq!(flat.get_sym(s("escapes")), Some(&Value::int(3)));
        assert_eq!(flat.get_sym(s("caller-lex")), Some(&Value::int(1)));
        assert_eq!(flat.source_file_sym(), Some(s("outer.raku")));
        // The kept name is still logged for the enclosing frame.
        assert_eq!(flat.frame_writes(), Some(&[s("escapes")][..]));
        // An env that never carried a log says so, and is left alone.
        let mut plain = scoped_with(Env::new(), &[("x", 1)]);
        assert!(!plain.retain_frame_writes(|_| false));
        assert_eq!(plain.get_sym(s("x")), Some(&Value::int(1)));
    }

    #[test]
    fn a_bulk_overlay_edit_drops_the_log_rather_than_leaving_it_stale() {
        let mut caller = Env::new();
        caller.insert("caller-lex".into(), Value::int(1));
        let mut flat = scoped_with(caller, &[("mine", 2)]).flattened_for_frame();
        assert!(flat.frame_writes().is_some());
        flat.retain(|_, _| true);
        assert!(
            flat.frame_writes().is_none(),
            "a retain cannot be logged key-by-key, so the log must go"
        );
    }
}
