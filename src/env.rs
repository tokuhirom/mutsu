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
///
/// An env *tier* is this map plus the indexes derived from its key set; see
/// [`crate::env_tier::Tier`], which owns the map and is what `Env::inner`
/// actually holds.
pub(crate) use crate::env_tier::{CaptureWalk, SymMap, Tier};

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

/// True once [`set_global_base`] has run. The per-interpreter base tier copies
/// `GLOBAL_BASE` in (see [`Env::set_dyn_base`]), so its installer checks this
/// rather than silently building a tier that is missing the built-in enums.
pub(crate) fn global_base_installed() -> bool {
    GLOBAL_BASE.get().is_some()
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

/// Monotonic, process-global flag for `__mutsu_scalar_bind_no_container::*`
/// markers (`my $i := 42`, recording that the name owns no Scalar container).
///
/// EVERY scalar `my` declaration used to clear this key speculatively, so that a
/// redeclaration could not inherit an earlier same-named variable's state — an
/// `env_mut()` (and so a possible CoW deep clone of the frame env) per `my $x`,
/// for a key that a program without a single `:=`-to-value bind never holds.
/// Same soundness argument as [`BOUND_SLICE_KEY_SEEN`]: the only creation site
/// goes through a noting insert, the flag is monotonic, and an over-set only
/// makes the (correct) clear run.
static SCALAR_NO_CONTAINER_KEY_SEEN: AtomicBool = AtomicBool::new(false);

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

/// Monotonic, process-global flag for `__mutsu_shaped_array_dims::*` keys (the
/// declared-shape markers of `my @a[2;3]`).
///
/// The probe runs on *every* element write (`@a[i] = x`) -- both fast paths and
/// the full store -- and each miss costs a `format!` plus a `Symbol::intern`ing
/// env lookup for a key a program without a single shaped-array declaration can
/// never hold. Same soundness argument as [`ELEM_INDEX_META_SEEN`]: the only
/// creation sites are String-keyed [`Env::insert`]s (so [`note_env_key`] catches
/// them all), the flag is monotonic, and an over-set only makes the (correct)
/// probe run.
static SHAPED_ARRAY_DIMS_SEEN: AtomicBool = AtomicBool::new(false);

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

/// True if any `__mutsu_scalar_bind_no_container::*` marker may exist in some
/// env. See [`SCALAR_NO_CONTAINER_KEY_SEEN`].
#[inline]
pub(crate) fn scalar_bind_no_container_possible() -> bool {
    SCALAR_NO_CONTAINER_KEY_SEEN.load(Ordering::Relaxed)
}

/// True if any per-element index metadata key may exist in some env. See
/// [`ELEM_INDEX_META_SEEN`].
#[inline]
pub(crate) fn elem_index_meta_possible() -> bool {
    ELEM_INDEX_META_SEEN.load(Ordering::Relaxed)
}

/// True if any `__mutsu_shaped_array_dims::*` marker may exist in some env. See
/// [`SHAPED_ARRAY_DIMS_SEEN`].
#[inline]
pub(crate) fn shaped_array_dims_possible() -> bool {
    SHAPED_ARRAY_DIMS_SEEN.load(Ordering::Relaxed)
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
        } else if key.starts_with("__mutsu_scalar_bind_no_container::") {
            SCALAR_NO_CONTAINER_KEY_SEEN.store(true, Ordering::Relaxed);
        } else if key.starts_with("__mutsu_shaped_array_dims::") {
            SHAPED_ARRAY_DIMS_SEEN.store(true, Ordering::Relaxed);
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
    inner: Arc<Tier>,
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
    /// A closure's captured env, chained UNDER the whole parent chain rather
    /// than merged into this overlay key by key (ADR-0092).
    ///
    /// Name lookups on a frame that has one resolve
    /// `overlay -> parent chain -> GLOBAL_BASE -> fallback`. That is exactly
    /// the precedence the per-key merge in `call_compiled_closure_in_unit`
    /// used to build by hand: its default was `entry_or_insert_sym_with`
    /// (don't overwrite anything already visible from the caller, base tier
    /// included), so the caller chain and the base already won over every
    /// captured name — and the probes inserted *nothing* in the ordinary case,
    /// because the capture had been filtered out of the very chain it was then
    /// compared against
    /// ([#7565](https://github.com/tokuhirom/mutsu/issues/7565)). Removing the
    /// loop is worth ~1 000 instructions per closure call over a ~12-entry
    /// capture; the mechanism here costs most of that back on a chain with no
    /// capture, so the case for it is §2 of the ADR (a probe loop that provably
    /// does nothing should not exist) rather than the number.
    ///
    /// The merge's explicit OVERWRITE cases (a captured `ContainerRef` cell, a
    /// lexical `self`, a non-routine block's topic and `$!`, the authoritative
    /// and owned capture lists, per-instance state) still insert into the
    /// overlay, which is above both.
    ///
    /// Every reader that walks the chain must treat this as a tier below it:
    /// [`Self::get_sym`]/[`Self::contains_key_sym`] consult every fallback in
    /// the chain, not just the leaf's — which is what keeps a *callee* of the
    /// closure resolving captured names, as it did when they lived in the
    /// frame overlay it chains over,
    /// [`Self::flattened`]/[`Self::filtered_flat`]/[`Self::filtered_flat_capture`]
    /// layer it in underneath, and [`Self::tier_addrs`]/[`Self::tier_maps`]
    /// include it so the capture memo cannot mistake two frames with different
    /// captures for one another. Iteration (`iter`/`keys`/`values`/`len`) is
    /// overlay-only as before and so does NOT see it — which is what the exit
    /// writeback wants, since a captured name the body never touched was
    /// never its own write.
    fallback: Option<Arc<Tier>>,
    /// True when this env, or any tier below it, carries a
    /// [`fallback`](Self::fallback).
    ///
    /// The lookup loops need "does this chain have a fallback at all?" *before*
    /// they know the answer is a miss, and asking it by walking the chain would
    /// cost more than the fallback saves. It is a one-way latch maintained by
    /// the two mutators that can make it true ([`Self::scoped_child`] inherits
    /// the parent's, [`Self::set_capture_fallback`] sets it) and is false for
    /// every env built flat — [`Self::flattened`] and the `filtered_flat*`
    /// family fold any fallback into the map they return.
    chain_has_fallback: bool,
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
    /// Index of the overlay keys that are *code env entries* (`&foo` and their
    /// `__mutsu_callable_id::` markers — see [`Symbol::is_code_env_entry`]),
    /// so a block scope can save/restore them in O(routine bindings) instead of
    /// O(whole env). See [`Self::code_env_keys`].
    ///
    /// `None` means "not indexed": the answer is recomputed by one overlay scan
    /// on the next ask. Every env starts that way and stays that way unless
    /// something actually asks — which keeps the maintenance hook in
    /// [`Self::insert_sym`] down to one `is_some()` branch on the envs (nearly
    /// all of them) that never run a block-scope save.
    ///
    /// A *superset* index, never a subset: it may name a key that has since
    /// been removed (removals do not prune it), so every consumer re-reads the
    /// overlay through the key. It must never MISS a present key, which is why
    /// the two bulk paths that can add keys without passing `insert_sym`
    /// ([`Self::inner_mut`]) and every whole-map rebuild (`flattened`,
    /// `filtered_flat`, `From<HashMap>`) reset it to `None` rather than carry a
    /// stale one forward.
    ///
    /// `Arc` for the same reason as `frame_writes`: an env clone stays a
    /// refcount bump.
    code_entries: Option<Arc<Vec<Symbol>>>,
    /// The **per-interpreter** never-copied base tier: the built-in dynamic
    /// variables (`$*OUT`, `$*CWD`, `%*ENV`, `@*ARGS`, `$*REPO`, ...) the
    /// interpreter seeds once at startup. ADR-0086.
    ///
    /// Read exactly like [`GLOBAL_BASE`] — at the chain's tail, after the
    /// overlay and every parent tier — but per interpreter rather than per
    /// process, because these values are not process constants: the IO handles
    /// and `$*PROGRAM`/`@*ARGS` belong to one interpreter (Test::Util's
    /// `is_run` fast path runs a nested one in the same process) and `$*CWD`
    /// is mutable. A write is *promoted* into the writer's own overlay, where
    /// it shadows the base; the base map itself is immutable for the
    /// interpreter's life, so it can be shared by `Arc` with no
    /// copy-on-write.
    ///
    /// Why they are not in the env at all: a closure capture keeps every key
    /// that is not a plain user lexical, so all ~20 of them were rebuilt —
    /// inserted, `Value`-cloned, later dropped — on *every* closure creation,
    /// and the call-time merge (`entry_or_insert_sym_with`, "don't overwrite
    /// what the chain already has") then discarded all ~20 because the live
    /// chain bottoms out at the same interpreter. See
    /// [ADR-0086](../docs/adr/0086-builtin-dynamics-are-not-closure-capture-material.md)
    /// §1.3 and [#7557](https://github.com/tokuhirom/mutsu/issues/7557).
    ///
    /// Carried only by a **flat** env (the chain's tail) and by anything
    /// derived from one: [`Self::scoped_child`] deliberately does not copy it,
    /// so the `Arc` refcount is not touched on the per-call frame path, and a
    /// chain walk reaches the tail's copy anyway.
    dyn_base: Option<Arc<SymMap>>,
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
fn empty_overlay() -> Arc<Tier> {
    empty_overlay_ref().clone()
}

/// Borrowed access to the shared empty overlay singleton, for identity checks
/// that must not pay the `Arc` refcount round-trip a `clone` would cost.
fn empty_overlay_ref() -> &'static Arc<Tier> {
    static EMPTY: std::sync::OnceLock<Arc<Tier>> = std::sync::OnceLock::new();
    EMPTY.get_or_init(|| Arc::new(Tier::default()))
}

/// The tombstones a chain collapse (`flattened`/`filtered_flat`) must carry
/// into its flat result: keys removed somewhere in the chain that are still
/// present in the per-interpreter base tier and were not re-inserted by a
/// nearer tier.
///
/// Without a base tier there is nothing below the merged map for a tombstone to
/// hide, so the answer is `None` — which is also the answer whenever no tier in
/// the chain carries a tombstone, i.e. nearly always. The walk is O(chain
/// depth) and [`MAX_OVERLAY_DEPTH`] bounds that at 16.
fn residual_base_tombstones(
    env: &Env,
    merged: &SymMap,
    base: Option<&SymMap>,
) -> Option<rustc_hash::FxHashSet<Symbol>> {
    let base = base?;
    let mut out: Option<rustc_hash::FxHashSet<Symbol>> = None;
    let mut cur = env;
    loop {
        if let Some(tomb) = &cur.tombstones {
            for k in tomb {
                if base.contains_key(k) && !merged.contains_key(k) {
                    out.get_or_insert_with(rustc_hash::FxHashSet::default)
                        .insert(*k);
                }
            }
        }
        match &cur.parent {
            Some(parent) => cur = parent,
            None => break,
        }
    }
    out
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
            inner: Arc::new(Tier::default()),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym: None,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base: None,
        }
    }

    /// Install the per-interpreter base tier on this (flat) env — see
    /// [`Self::dyn_base`]. Called by `Interpreter::hoist_builtin_dynamics`,
    /// right after the built-in dynamics have been lifted out of the env's own
    /// map — once for an ordinary interpreter, and once more for each thread
    /// clone, which rebuilds some of them and so needs a tier of its own.
    pub(crate) fn set_dyn_base(&mut self, mut base: SymMap) {
        debug_assert!(
            !base.contains_key(&file_key()),
            "the hoist moves `*`-twigil dynamics only, never `?FILE` (whose value is cached on the env)"
        );
        // Absorb [`GLOBAL_BASE`] rather than sit beside it. The tail of a
        // lookup already paid one map probe for the process-wide tier; a
        // *second* probe for the per-interpreter one would make every env miss
        // — which is every metadata key the VM speculatively reads — more
        // expensive than before this tier existed. Merged, the tail still costs
        // exactly one probe (measured: two probes cost `word-count` ~13M Ir in
        // `Env::get_sym` alone). `GLOBAL_BASE` is a `OnceLock` installed by
        // `Interpreter::new` before any `run()`, so this copy is complete, and
        // `or_insert` keeps a hoisted dynamic ahead of it in the impossible
        // case that both hold a key.
        if let Some(global) = global_base() {
            for (k, v) in global.iter() {
                base.entry(*k).or_insert_with(|| v.clone());
            }
        }
        self.dyn_base = Some(Arc::new(base));
    }

    /// This env's per-interpreter base tier, or the one its chain bottoms out
    /// at. `None` for an env that was never given one (`Env::new()` and
    /// friends — those hold no built-in dynamics today either).
    pub(crate) fn dyn_base(&self) -> Option<&Arc<SymMap>> {
        let mut cur = self;
        while let Some(parent) = &cur.parent {
            cur = parent;
        }
        cur.dyn_base.as_ref()
    }

    /// This env's own base-tier entry for `key`: the per-interpreter tier when
    /// it has one (a superset of the process-wide tier — see
    /// [`Self::set_dyn_base`]), else [`GLOBAL_BASE`]. Consults THIS env only,
    /// never the parent chain, so it is the tail half of a lookup.
    #[inline]
    fn base_get(&self, key: Symbol) -> Option<&Value> {
        match &self.dyn_base {
            Some(base) => base.get(&key),
            None => global_base().and_then(|b| b.get(&key)),
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
        //
        // A tier carrying a closure-capture `fallback` is NOT invisible — its
        // captured names resolve through it — so it is never skipped here,
        // however empty its own overlay is.
        if parent.inner.is_empty()
            && parent.tombstones.is_none()
            && parent.fallback.is_none()
            && let Some(gp) = parent.parent.take()
        {
            let mut arc = gp;
            while arc.inner.is_empty()
                && arc.tombstones.is_none()
                && arc.fallback.is_none()
                && let Some(gp) = &arc.parent
            {
                let gp = Arc::clone(gp);
                arc = gp;
            }
            if arc.depth >= MAX_OVERLAY_DEPTH {
                let flat = arc.flattened();
                let (flat_depth, flat_chf) = (flat.depth, flat.chain_has_fallback);
                return Self {
                    inner: empty_overlay(),
                    depth: flat_depth + 1,
                    parent: Some(Arc::new(flat)),
                    tombstones: None,
                    file_sym,
                    fallback: None,
                    chain_has_fallback: flat_chf,
                    frame_writes: None,
                    code_entries: None,
                    dyn_base: None,
                };
            }
            let (arc_depth, arc_chf) = (arc.depth, arc.chain_has_fallback);
            return Self {
                inner: empty_overlay(),
                depth: arc_depth + 1,
                parent: Some(arc),
                tombstones: None,
                file_sym,
                fallback: None,
                chain_has_fallback: arc_chf,
                frame_writes: None,
                code_entries: None,
                dyn_base: None,
            };
        }
        let parent = if parent.depth >= MAX_OVERLAY_DEPTH {
            parent.flattened()
        } else {
            parent
        };
        let (parent_depth, parent_chf) = (parent.depth, parent.chain_has_fallback);
        Self {
            inner: empty_overlay(),
            depth: parent_depth + 1,
            parent: Some(Arc::new(parent)),
            tombstones: None,
            file_sym,
            fallback: None,
            chain_has_fallback: parent_chf,
            frame_writes: None,
            code_entries: None,
            dyn_base: None,
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
            // The capture fallback is the lowest tier (see the field), so it
            // answers only when the whole chain is silent.
            None => self.parent.as_ref().and_then(|p| p.file_sym).or_else(|| {
                self.fallback
                    .as_ref()
                    .and_then(|fb| fb.get(&file_key()))
                    .and_then(file_sym_of)
            }),
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

    /// Keep [`Self::code_entries`] a superset of the overlay's code-var keys
    /// across an `insert`. A no-op — one branch on a field the insert already
    /// touched — for every env that has never been asked for the index, which
    /// is nearly all of them: only [`Self::code_env_keys`] turns the index on,
    /// and only a block-scope save/restore calls that. `Symbol::is_code_env_entry`
    /// (a thread-local memo lookup) is therefore never paid on the general
    /// insert path.
    #[inline(always)]
    fn note_code_entry(&mut self, key: Symbol) {
        if let Some(idx) = &mut self.code_entries
            && key.is_code_env_entry()
            && !idx.contains(&key)
        {
            Arc::make_mut(idx).push(key);
        }
    }

    /// The overlay keys that are code env entries — `&foo` routine bindings and
    /// their `__mutsu_callable_id::` markers (see [`Symbol::is_code_env_entry`]).
    ///
    /// These are exactly the keys a block scope has to snapshot on entry and
    /// restore on exit, so that a block-local `sub`/`my &foo` does not leak into
    /// the caller. Doing that by scanning the whole overlay twice per block was
    /// the measured cost of running a carrier block (#7575): two full env walks
    /// per `<?{ … }>` assertion evaluation, per grammar `token` body, per
    /// `where` clause. The index makes both walks O(routine bindings in scope) —
    /// zero for the overwhelmingly common scope that declares no routines.
    ///
    /// The returned list is a *superset*: a key it names may have been removed
    /// since, so read each one back through [`Self::overlay_get_sym`] rather than
    /// assuming it is present. It never misses a key that IS present.
    ///
    /// Takes `&mut self` because the first ask materializes (and memoizes) the
    /// index with one overlay scan.
    pub(crate) fn code_env_keys(&mut self) -> Arc<Vec<Symbol>> {
        let inner = &self.inner;
        self.code_entries
            .get_or_insert_with(|| {
                Arc::new(
                    inner
                        .keys()
                        .copied()
                        .filter(|k| k.is_code_env_entry())
                        .collect(),
                )
            })
            .clone()
    }

    /// Drop this env's frame-write log, for a caller that has just made a bulk
    /// edit it cannot describe key-by-key (see [`Self::frame_writes`]). A no-op
    /// for the envs — nearly all of them — that carry no log.
    #[inline]
    pub(crate) fn forget_frame_writes(&mut self) {
        self.frame_writes = None;
    }

    /// Drop `key` from this tier's overlay **without** tombstoning it, so a
    /// parent-tier binding of the same name shadows back through.
    ///
    /// This is `retain`'s per-key removal semantics ("filter my own overlay"),
    /// not `remove_sym`'s ("this name is deleted in this scope"). A block-scope
    /// restore wants the former: it undoes writes the block made to THIS tier
    /// and must leave an enclosing tier's routine binding visible.
    pub(crate) fn remove_overlay_sym(&mut self, key: Symbol) -> Option<Value> {
        if !self.inner.contains_key(&key) {
            return None;
        }
        if key == file_key() {
            self.file_sym = None;
        }
        self.note_frame_write(key);
        self.cow_mut().remove(&key)
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
        // A capture fallback is a real tier (see the field), so an env that has
        // one is never "already flat" and never collapses to its parent: the
        // general arm below layers every fallback in the chain under the
        // overlays. The latch, not a chain walk: `flattened` runs per closure
        // creation, and walking the chain to discover that there is no fallback
        // cost 0.8% of `benchmarks/bench-ctor.raku` on its own.
        if self.chain_has_fallback {
            return self.flattened_with_fallbacks();
        }
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
                let mut merged: SymMap = (**cur.inner).clone();
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
                let dyn_base = cur.dyn_base.clone();
                let any_tombstone = {
                    let mut cur = self;
                    let mut any = cur.tombstones.is_some();
                    while let Some(parent) = &cur.parent {
                        cur = parent;
                        any |= cur.tombstones.is_some();
                    }
                    any
                };
                // A tombstone that hides a base-tier dynamic has to survive the
                // collapse: the merged map does not contain the key (nothing
                // ever wrote it into an overlay), so without the tombstone the
                // base would shadow straight back through. `None` whenever the
                // chain carries no tombstone or no base tier, which is nearly
                // always.
                let tombstones = any_tombstone
                    .then(|| residual_base_tombstones(self, &merged, dyn_base.as_deref()))
                    .flatten();
                Self {
                    inner: Arc::new(Tier::new(merged)),
                    parent: None,
                    tombstones,
                    depth: 0,
                    // Flattening preserves every visible value, `?FILE` included.
                    file_sym: self.file_sym,
                    // The merged map is no longer any one frame's tier;
                    // `flattened_for_frame` is what records the collapsed tier's
                    // writes when a light frame needs them (#7630).
                    fallback: None,
                    chain_has_fallback: false,
                    frame_writes: None,
                    code_entries: None,
                    dyn_base,
                }
            }
        }
    }

    /// Push every capture [`fallback`](Self::fallback) in this chain into
    /// `out`, lowest precedence first.
    ///
    /// Precedence among fallbacks is tail-most-wins, matching
    /// [`Self::get_sym_with_fallback`]: a closure created inside another body
    /// chains over it, and the outer capture is the enclosing lexical scope of
    /// the inner one. Writing them leaf-first here lets a later `insert`
    /// overwrite an earlier one and reproduces that.
    fn collect_fallbacks(&self, out: &mut SymMap, base: Option<&SymMap>) {
        let mut cur = self;
        loop {
            if let Some(fb) = &cur.fallback {
                for (k, v) in fb.iter() {
                    if base.is_some_and(|b| b.contains_key(k)) {
                        continue;
                    }
                    out.insert(*k, v.clone());
                }
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => return,
            }
        }
    }

    /// [`Self::collect_fallbacks`] restricted to the entries `keep` accepts.
    /// Returns whether anything was written, which is what tells the chain
    /// walk that follows whether its first tier is still the outermost one.
    ///
    /// A reject suppresses a lower-precedence fallback's entry for the same
    /// key, as it does in the chain walk: `keep` sees the value as well as the
    /// key, so two nested closures capturing one name can disagree about it.
    ///
    /// `base` is the chain tail's base tier, and a key it holds is skipped:
    /// the base beats a fallback while the chain is live ([`Self::get_sym`]
    /// probes it first), but it is carried forward rather than materialized, so
    /// writing the fallback's entry into the flat map would invert that.
    fn collect_fallbacks_filtered<F: Fn(Symbol, &Value) -> bool>(
        &self,
        out: &mut SymMap,
        keep: &F,
        base: Option<&SymMap>,
    ) -> bool {
        let mut wrote = false;
        let mut cur = self;
        loop {
            if let Some(fb) = &cur.fallback {
                for (k, v) in fb.iter() {
                    if base.is_some_and(|b| b.contains_key(k)) {
                        continue;
                    }
                    if keep(*k, v) {
                        out.insert(*k, v.clone());
                        wrote = true;
                    } else if wrote {
                        out.remove(k);
                    }
                }
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => return wrote,
            }
        }
    }

    /// [`Self::flattened`] for a chain that carries at least one capture
    /// fallback: the fallbacks go in first (they lose to every overlay in the
    /// chain), then the chain itself, root-ward first.
    ///
    /// The base tier is carried forward rather than materialized, exactly as
    /// [`Self::flattened`] does, so the fallbacks are collected *minus* the
    /// keys it holds: the base beats a fallback while the chain is live, and a
    /// flattened fallback entry would otherwise invert that.
    fn flattened_with_fallbacks(&self) -> Self {
        let mut tiers: Vec<&Env> = Vec::new();
        let mut cur: &Env = self;
        while let Some(parent) = &cur.parent {
            tiers.push(cur);
            cur = parent;
        }
        let dyn_base = cur.dyn_base.clone();
        let base_map: Option<&SymMap> = match dyn_base.as_deref() {
            Some(base) => Some(base),
            None => global_base(),
        };
        let mut merged: SymMap = SymMap::default();
        self.collect_fallbacks(&mut merged, base_map);
        for (k, v) in cur.inner.iter() {
            merged.insert(*k, v.clone());
        }
        let mut any_tombstone = false;
        for tier in tiers.into_iter().rev() {
            if let Some(tomb) = &tier.tombstones {
                any_tombstone = true;
                for k in tomb {
                    merged.remove(k);
                }
            }
            for (k, v) in tier.inner.iter() {
                merged.insert(*k, v.clone());
            }
        }
        // A tombstone that hides a base-tier dynamic has to survive the
        // collapse -- see `flattened`'s general arm, which this mirrors.
        let tombstones = any_tombstone
            .then(|| residual_base_tombstones(self, &merged, dyn_base.as_deref()))
            .flatten();
        Self {
            inner: Arc::new(Tier::new(merged)),
            parent: None,
            tombstones,
            depth: 0,
            file_sym: self.file_sym,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base,
        }
    }

    /// Install `tier` as this env's closure-capture [`fallback`](Self::fallback).
    ///
    /// Replaces any fallback already there: a frame env belongs to exactly one
    /// call, and the merge installs it once, before the body runs.
    pub(crate) fn set_capture_fallback(&mut self, tier: Arc<Tier>) {
        self.fallback = Some(tier);
        self.chain_has_fallback = true;
        // A captured `?FILE` is visible through the fallback exactly as an
        // overlay one is, and `source_file_sym` is an O(1) mirror of that
        // answer, so it has to be re-derived here like any other mutator.
        if self.file_sym.is_none() {
            self.refresh_file_sym();
        }
    }

    /// This env viewed as ONE tier, for installing it as a closure frame's
    /// [`fallback`](Self::fallback).
    ///
    /// Ordinarily an `Arc` bump: a `SubData`'s env is flat (`clone_env` /
    /// `filtered_flat_capture` both return one), so its overlay already *is*
    /// its whole visible view.
    ///
    /// The exception is a `Sub` built straight from a live scoped env — a
    /// `whenever` callback is (`react_whenever.rs`'s `self.env.clone()`), and
    /// ~80 sites could be. There the overlay alone is not the view, because a
    /// closure frame keeps its own capture in a fallback rather than in its
    /// overlay, so a callback built inside one would silently lose every name
    /// that frame captured. (Pinned by the three-level nested `whenever` in
    /// `t/concurrency/supply/promise-of-supply-completion.t`: `$x` from the
    /// outermost `whenever` reached the second level and not the third.)
    ///
    /// The parent CHAIN is deliberately not folded in. The per-key merge this
    /// replaces iterated the captured env's own tier only — `Env::iter` does
    /// not walk the chain — and reached chain lexicals solely through the
    /// by-name `get_sym` lookups that still stand beside it (`self`, the
    /// authoritative and owned capture lists). Folding it in here would make a
    /// closure capture strictly more than it used to.
    #[inline]
    pub(crate) fn capture_tier(&self) -> Arc<Tier> {
        match &self.fallback {
            None => Arc::clone(&self.inner),
            Some(fb) => Self::capture_tier_merged(&self.inner, fb),
        }
    }

    /// The rare half of [`Self::capture_tier`], kept out of line so the `Arc`
    /// bump stays a load and a refcount increment.
    #[cold]
    fn capture_tier_merged(inner: &Arc<Tier>, fb: &Arc<Tier>) -> Arc<Tier> {
        let mut merged: SymMap = (***fb).clone();
        for (k, v) in inner.iter() {
            merged.insert(*k, v.clone());
        }
        Arc::new(Tier::new(merged))
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
    pub(crate) fn filtered_flat<F: Fn(Symbol, &Value) -> bool>(&self, keep: &F) -> Env {
        /// `outermost` is true for the base of the chain -- the tier processed
        /// first, while `out` still holds nothing. A rejected or tombstoned key
        /// there cannot be shadowing an outer tier's kept entry (there is no
        /// outer tier, and a map yields each of its own keys once), so the
        /// suppressing `out.remove` is a guaranteed miss: a hash and a probe per
        /// rejected key, on the widest tier of the chain. A closure created in a
        /// sub after a bare `use Test` walked 95 visible keys and kept 31, and
        /// all 62 of its removes landed on that one tier (#7565); that capture
        /// now goes through [`Env::filtered_flat_capture`], but the shape of
        /// the saving is the same for any other wide chain this walks.
        fn collect<F: Fn(Symbol, &Value) -> bool>(
            env: &Env,
            out: &mut SymMap,
            keep: &F,
            outermost: bool,
        ) {
            let outermost = match &env.parent {
                Some(parent) => {
                    collect(parent, out, keep, outermost);
                    false
                }
                None => outermost,
            };
            if let Some(tomb) = &env.tombstones
                && !outermost
            {
                for k in tomb {
                    out.remove(k);
                }
            }
            for (k, v) in env.inner.iter() {
                if keep(*k, v) {
                    out.insert(*k, v.clone());
                } else if !outermost {
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
        // The same walk answers two more questions for free: which
        // per-interpreter base tier the chain's tail carries (ADR-0086) and
        // whether any tier holds a tombstone — so neither costs its own pass.
        let mut dyn_base = None;
        let mut any_tombstone = false;
        {
            let mut cur = Some(self);
            while let Some(env) = cur {
                cap += env.inner.len();
                if env.chain_has_fallback {
                    cap += env.fallback.as_ref().map_or(0, |fb| fb.len());
                }
                any_tombstone |= env.tombstones.is_some();
                dyn_base = env.dyn_base.as_ref();
                cur = env.parent.as_deref();
            }
        }
        let dyn_base = dyn_base.cloned();
        let mut out = SymMap::with_capacity_and_hasher(cap, Default::default());
        // The capture fallbacks are the lowest tiers of all, so they go in
        // first and every overlay in the chain layers over them. When one
        // contributes, the chain's own root tier is no longer the first thing
        // written and must start suppressing its rejects (`outermost=false`).
        let base_map: Option<&SymMap> = match dyn_base.as_deref() {
            Some(base) => Some(base),
            None => global_base(),
        };
        let outermost =
            !self.chain_has_fallback || !self.collect_fallbacks_filtered(&mut out, keep, base_map);
        collect(self, &mut out, keep, outermost);
        // `keep` may have rejected `?FILE`, so re-derive rather than inherit.
        let file_sym = out.get(&file_key()).and_then(file_sym_of);
        // The per-interpreter base tier rides along by reference, exactly as
        // `GLOBAL_BASE` does: it is what stops the built-in dynamics from being
        // rebuilt into every closure capture (ADR-0086). `keep` never sees them
        // and so cannot reject them — which is the point, since a capture that
        // dropped them would have to prove the caller's chain re-supplies them.
        let tombstones = any_tombstone
            .then(|| residual_base_tombstones(self, &out, dyn_base.as_deref()))
            .flatten();
        Self {
            inner: Arc::new(Tier::new(out)),
            parent: None,
            tombstones,
            depth: 0,
            file_sym,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base,
        }
    }

    /// [`Self::filtered_flat`] specialized for the closure capture: the keys no
    /// capture can ever keep are skipped **without being visited**, by walking
    /// each tier's [`Tier::capture_candidates`] memo instead of its whole map.
    ///
    /// `keep` therefore only has to decide the part of the capture filter that
    /// depends on which closure is being created; the key-only part lives in
    /// [`crate::env_tier::capture_never_keeps`], which is what the memo caches.
    ///
    /// Skipping a non-candidate outright — rather than rejecting it and then
    /// suppressing an outer tier's entry with `out.remove`, as
    /// [`Self::filtered_flat`] must — is sound precisely because that verdict is
    /// a pure function of the key: a key rejected on one tier was rejected on
    /// every tier, so it was never inserted into `out` and there is nothing to
    /// suppress. The candidate list is a *superset* (a removal does not prune
    /// it), so each key is looked up rather than assumed present.
    ///
    /// After a bare `use Test` this is the difference between visiting 96 keys
    /// and visiting 47 for the same 31-entry result, on every closure creation
    /// in the importing file (#7565).
    pub(crate) fn filtered_flat_capture<F: Fn(Symbol, &Value) -> bool>(&self, keep: &F) -> Env {
        fn collect<F: Fn(Symbol, &Value) -> bool>(
            env: &Env,
            out: &mut SymMap,
            keep: &F,
            outermost: bool,
        ) {
            let outermost = match &env.parent {
                Some(parent) => {
                    collect(parent, out, keep, outermost);
                    false
                }
                None => outermost,
            };
            if let Some(tomb) = &env.tombstones
                && !outermost
            {
                for k in tomb {
                    out.remove(k);
                }
            }
            // The body is spelled out per arm rather than shared through a
            // closure: a capturing `FnMut` here is not inlined, and that cost
            // more per key than skipping the rejected ones saved (#7565).
            //
            // `keep` is the WHOLE filter in both arms, never-keeps included.
            // Hoisting the key-only half out of it and testing it separately
            // here looks like an obvious saving and is not: both halves start
            // by loading the key's memoized flags word, so splitting them made
            // every walked key pay that load twice — measured at +880
            // instructions per closure creation on an import-free program,
            // which is more than the memo saves on one that imports (#7565).
            match env.inner.capture_walk() {
                CaptureWalk::Entries => {
                    for (k, v) in env.inner.iter() {
                        if keep(*k, v) {
                            out.insert(*k, v.clone());
                        } else if !outermost {
                            out.remove(k);
                        }
                    }
                }
                CaptureWalk::Candidates(keys) => {
                    for &k in keys {
                        // Superset index: the key may have been removed since
                        // the memo was built.
                        let Some(v) = env.inner.get(&k) else {
                            continue;
                        };
                        if keep(k, v) {
                            out.insert(k, v.clone());
                        } else if !outermost {
                            out.remove(&k);
                        }
                    }
                }
            }
        }
        // Pre-size from the candidate counts rather than the whole chain's key
        // count: the rejected families are exactly what makes the two diverge
        // (36 against 96 after a bare `use Test`), and over-reserving cost a
        // 2 KiB allocation and its zeroing per closure creation.
        let mut cap = 0usize;
        // As in [`Self::filtered_flat`], the same walk also answers which
        // per-interpreter base tier the chain's tail carries (ADR-0086) and
        // whether any tier holds a tombstone.
        let mut dyn_base = None;
        let mut any_tombstone = false;
        {
            let mut cur = Some(self);
            while let Some(env) = cur {
                cap += env.inner.capture_upper_bound();
                if env.chain_has_fallback {
                    cap += env
                        .fallback
                        .as_ref()
                        .map_or(0, |fb| fb.capture_upper_bound());
                }
                any_tombstone |= env.tombstones.is_some();
                dyn_base = env.dyn_base.as_ref();
                cur = env.parent.as_deref();
            }
        }
        let dyn_base = dyn_base.cloned();
        let mut out = SymMap::with_capacity_and_hasher(cap, Default::default());
        // A closure created inside a closure body sees the outer capture
        // through its frame's fallback tier, not through an overlay, so the
        // walk has to start there or the inner closure loses every name the
        // outer one captured (ADR-0092 §4). Gated on the latch: this runs per
        // closure creation, and a chain walk that finds nothing is pure loss.
        let base_map: Option<&SymMap> = match dyn_base.as_deref() {
            Some(base) => Some(base),
            None => global_base(),
        };
        let outermost =
            !self.chain_has_fallback || !self.collect_fallbacks_filtered(&mut out, keep, base_map);
        collect(self, &mut out, keep, outermost);
        // `keep` may have rejected `?FILE`, so re-derive rather than inherit.
        let file_sym = out.get(&file_key()).and_then(file_sym_of);
        // The per-interpreter base tier rides along by reference, exactly as
        // `GLOBAL_BASE` does: the built-in dynamics are not in any map for this
        // walk to visit, which is what stops them being rebuilt into every
        // capture (ADR-0086).
        let tombstones = any_tombstone
            .then(|| residual_base_tombstones(self, &out, dyn_base.as_deref()))
            .flatten();
        Self {
            inner: Arc::new(Tier::new(out)),
            parent: None,
            tombstones,
            depth: 0,
            file_sym,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base,
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
        // One predictable branch for every env in the process that has no
        // capture below it, which is nearly all of them. The fallback walk is
        // a separate copy of this loop rather than a flag inside it, so the
        // common path pays no per-tier test.
        if self.chain_has_fallback {
            return self.get_sym_with_fallback(key);
        }
        // Walk the parent chain (each tier may itself be scoped) as a LOOP, not
        // a recursion. It used to be a self-call in tail position, which the
        // optimizer turned into this loop for free — but a capture `fallback`
        // has to be consulted *after* the chain misses, and the smallest
        // expression of that ("recurse, then check my own fallback") puts work
        // after the call and costs a real stack frame per tier. Measured at
        // +1 591 instructions per iteration of #7565's loop, which was most of
        // what dropping the per-key capture merge had just saved. Spelling the
        // walk out keeps the chain cost exactly what it was and moves the
        // fallback onto its own pass.
        let mut cur = self;
        loop {
            if let Some(v) = cur.inner.get(&key) {
                return Some(v);
            }
            // A tombstoned key is deleted in this scope: do not fall through to
            // the parent, the base tier, or a fallback.
            if cur.is_tombstoned(key) {
                return None;
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        // Chain tail: one base-tier probe, on the tier the loop walked to
        // rather than on `self`. The per-interpreter tier (built-in dynamics,
        // ADR-0086) is a superset of the process-wide one, so an env that has
        // it needs no second look at `GLOBAL_BASE` — see
        // [`Self::set_dyn_base`].
        cur.base_get(key)
    }

    /// [`Self::get_sym`] for a chain that carries a capture
    /// [`fallback`](Self::fallback), which is only ever a frame executing a
    /// closure.
    ///
    /// The fallback tiers are below the base, so this cannot answer from one
    /// until the whole chain and the base have missed — but it collects them in
    /// the SAME walk rather than making a second pass, because a miss is the
    /// common outcome here (every speculative metadata probe) and walking twice
    /// for it cost more than the capture merge this replaces ever did.
    fn get_sym_with_fallback(&self, key: Symbol) -> Option<&Value> {
        let mut cur = self;
        // Tail-most wins, so keep overwriting: a closure created inside another
        // closure's body chains over it, and the outer capture is the enclosing
        // lexical scope of the inner one.
        let mut found = None;
        loop {
            if let Some(v) = cur.inner.get(&key) {
                return Some(v);
            }
            if cur.is_tombstoned(key) {
                return None;
            }
            if let Some(fb) = &cur.fallback
                && let Some(v) = fb.get(&key)
            {
                found = Some(v);
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        cur.base_get(key).or(found)
    }

    #[inline]
    pub fn contains_key(&self, key: &str) -> bool {
        self.contains_key_sym(Symbol::intern(key))
    }

    #[inline]
    pub fn contains_key_sym(&self, key: Symbol) -> bool {
        if self.chain_has_fallback {
            return self.get_sym_with_fallback(key).is_some();
        }
        // Loop, not recursion — see `get_sym` for why.
        let mut cur = self;
        loop {
            if cur.inner.contains_key(&key) {
                return true;
            }
            if cur.is_tombstoned(key) {
                return false;
            }
            match &cur.parent {
                Some(parent) => cur = parent,
                None => break,
            }
        }
        // Chain tail: one base-tier probe, on the tier the loop walked to --
        // see `get_sym`.
        cur.base_get(key).is_some()
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
    fn cow_mut(&mut self) -> &mut Tier {
        if crate::vm::vm_stats::enabled() && Arc::strong_count(&self.inner) > 1 {
            crate::vm::vm_stats::record_env_deep_copy(self.inner.len());
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
        self.note_code_entry(key);
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
        //
        // Unchanged by the per-interpreter base tier (ADR-0086): a flat env's
        // `remove` still never tombstones, so it costs no base probe here. The
        // effect on a key the tier also holds is to *un-shadow* it — the
        // overlay entry goes and the seeded value below shows through — which
        // is what a tier below an overlay means, and what `GLOBAL_BASE` has
        // always done for a flat env's enum constants.
        if self.parent.is_none() && self.fallback.is_none() && !self.inner.contains_key(&key) {
            return None;
        }
        self.note_frame_write(key);
        let from_overlay = self.cow_mut().remove(&key);
        // Scoped env: if the key still exists in the parent/base tier, record a
        // tombstone so it stops shadowing through. The visible value before
        // removal is the overlay value if present, else the parent/base value.
        // A capture fallback is a tier below the chain, so it needs the same
        // treatment: without a tombstone, removing a captured name would leave
        // it visible.
        if self.parent.is_some() || self.fallback.is_some() {
            // Chain-aware lookup (covers the base tier at the chain tail and
            // any capture fallback). The overlay entry is already gone, so this
            // is exactly "what would still be visible without a tombstone".
            let parent_val = self.get_sym(key).cloned();
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
            // Promote from wherever the name is visible — the parent chain,
            // the base tier (`base_get`, at the chain tail), or a capture
            // fallback — so a write to a caller lexical or a captured one lands
            // in this frame's overlay instead of being silently lost. `get_sym`
            // is the single definition of "visible", and re-probing this
            // overlay (a known miss) is one hash on a path that is about to
            // clone a value anyway.
            let promote = self.get_sym(key).cloned();
            if let Some(v) = promote {
                self.untombstone(key);
                // A promotion ADDS a key to this overlay, so it has to reach the
                // code-entry index exactly as an `insert` does.
                self.note_code_entry(key);
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
        // The capture fallback is a live tier of this env, reachable by name
        // from the running frame, so it is a root like any other tier.
        if let Some(fb) = &self.fallback {
            for v in fb.values() {
                visitor.visit_value(v);
            }
        }
        if let Some(parent) = &self.parent {
            parent.visit_values(visitor);
            return;
        }
        // Chain tail: the per-interpreter base tier's built-in dynamics are
        // roots too (ADR-0086) — they hold the IO handles, `%*ENV` and
        // `$*REPO`. `GLOBAL_BASE` is deliberately not visited: it is a
        // process-lifetime `OnceLock` that is never reclaimed.
        if let Some(base) = &self.dyn_base {
            for v in base.values() {
                visitor.visit_value(v);
            }
        }
    }

    /// The per-interpreter base-tier entries this **flat** env still exposes:
    /// the built-in dynamics neither shadowed by its own map nor tombstoned.
    ///
    /// For the consumers that present an env as *the visible environment*
    /// rather than look one name up in it — the `DYNAMIC::` and `PROCESS::`
    /// pseudo-stashes — which would otherwise lose `$*OUT` and friends when
    /// they moved out of the map (ADR-0086 §4).
    pub(crate) fn visible_base_dynamics(&self) -> Vec<(Symbol, Value)> {
        let Some(base) = self.dyn_base() else {
            return Vec::new();
        };
        base.iter()
            .filter(|(k, _)| !self.inner.contains_key(k) && !self.is_tombstoned(**k))
            .map(|(k, v)| (*k, v.clone()))
            .collect()
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
            None => match &self.dyn_base {
                Some(base) => base.iter().map(|(k, v)| (k.resolve(), v.clone())).collect(),
                None => global_base()
                    .map(|b| b.iter().map(|(k, v)| (k.resolve(), v.clone())).collect())
                    .unwrap_or_default(),
            },
        };
        // The capture fallback is the lowest tier: `or_insert` so anything the
        // chain (or the base) already provided keeps winning.
        if let Some(fb) = &self.fallback {
            for (k, v) in fb.iter() {
                out.entry(k.resolve()).or_insert_with(|| v.clone());
            }
        }
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
            None => {
                let tier = match self.dyn_base.as_deref() {
                    Some(base) => Some(base),
                    None => global_base(),
                };
                tier.map(|b| {
                    b.keys()
                        .map(|k| k.resolve())
                        .filter(|k| keep(k))
                        .collect::<HashSet<String>>()
                })
                .unwrap_or_default()
            }
        };
        if let Some(fb) = &self.fallback {
            for k in fb.keys() {
                let name = k.resolve();
                if keep(&name) {
                    out.insert(name);
                }
            }
        }
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
    ///
    /// The one write path that can add a key without passing through
    /// [`Self::insert_sym`], so it drops the code-entry index rather than let a
    /// bulk insert make it miss a key — see [`Self::code_entries`].
    #[allow(dead_code)]
    pub(crate) fn inner_mut(&mut self) -> &mut SymMap {
        self.code_entries = None;
        self.cow_mut().map_mut()
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
            // A capture fallback is a tier of this chain and can differ
            // between two frames whose overlays are identical (two closures
            // from one factory, called in turn), so it has to be part of the
            // identity or the capture memo would hand the second one the
            // first one's capture.
            if let Some(fb) = &cur.fallback {
                if len == MAX_IDENTITY_TIERS {
                    return None;
                }
                addrs[len] = Arc::as_ptr(fb) as usize;
                len += 1;
            }
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
    pub(crate) fn tier_maps(&self) -> Vec<Arc<Tier>> {
        let mut maps = Vec::new();
        let mut cur = self;
        loop {
            maps.push(Arc::clone(&cur.inner));
            // Same order as `tier_addrs`, which `TierAddrs::matches` zips
            // against this.
            if let Some(fb) = &cur.fallback {
                maps.push(Arc::clone(fb));
            }
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
    pub(crate) fn matches(&self, maps: &[Arc<Tier>]) -> bool {
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
            inner: Arc::new(Tier::new(sym_map)),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base: None,
        }
    }
}

impl From<HashMap<Symbol, Value>> for Env {
    fn from(map: HashMap<Symbol, Value>) -> Self {
        let map: SymMap = map.into_iter().collect();
        let file_sym = map.get(&file_key()).and_then(file_sym_of);
        Self {
            inner: Arc::new(Tier::new(map)),
            parent: None,
            tombstones: None,
            depth: 0,
            file_sym,
            fallback: None,
            chain_has_fallback: false,
            frame_writes: None,
            code_entries: None,
            dyn_base: None,
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
            .into_map()
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

    /// Every key the code-entry index must name, and the ones it must not,
    /// derived by brute force from the overlay. The index is allowed to be a
    /// *superset* (removals do not prune it), never a subset.
    fn code_entries_are_a_superset(env: &mut Env) {
        let index = env.code_env_keys();
        let actual: Vec<Symbol> = env
            .inner
            .keys()
            .copied()
            .filter(|k| k.is_code_env_entry())
            .collect();
        for k in &actual {
            assert!(
                index.contains(k),
                "index missed a present code entry: {}",
                k.as_str()
            );
        }
        for k in index.iter() {
            assert!(
                k.is_code_env_entry(),
                "index named a non-code key: {}",
                k.as_str()
            );
        }
    }

    #[test]
    fn code_env_key_classification() {
        for k in [
            "&helper",
            "&?BLOCK",
            "&?ROUTINE",
            "__mutsu_callable_id::M::f",
        ] {
            assert!(s(k).is_code_env_entry(), "{k} is a code env entry");
        }
        for k in ["x", "@arr", "%h", "self", "_", "?FILE", "__mutsu_type::x"] {
            assert!(!s(k).is_code_env_entry(), "{k} is not a code env entry");
        }
    }

    #[test]
    fn code_entry_index_tracks_inserts_and_survives_removals() {
        let mut env = Env::new();
        env.insert("x".into(), Value::int(1));
        env.insert("&f".into(), Value::int(2));
        // First ask materializes the index from the overlay.
        code_entries_are_a_superset(&mut env);
        assert_eq!(env.code_env_keys().len(), 1);
        // A later insert extends the live index rather than invalidating it.
        env.insert("__mutsu_callable_id::MAIN::f".into(), Value::int(3));
        env.insert("&g".into(), Value::int(4));
        code_entries_are_a_superset(&mut env);
        assert_eq!(env.code_env_keys().len(), 3);
        // Re-inserting a known key does not duplicate it.
        env.insert("&g".into(), Value::int(5));
        assert_eq!(env.code_env_keys().len(), 3);
        // A removal may leave a stale entry behind (superset), but every key the
        // index names must still be readable back through the overlay or absent.
        env.remove("&g");
        code_entries_are_a_superset(&mut env);
        assert!(env.overlay_get_sym(s("&g")).is_none());
    }

    #[test]
    fn code_entry_index_rebuilt_after_a_whole_map_replacement() {
        let mut src: HashMap<Symbol, Value> = HashMap::new();
        src.insert(s("&f"), Value::int(1));
        src.insert(s("y"), Value::int(2));
        // `From<HashMap>` cannot maintain an index, so it must start unindexed
        // and rebuild on the first ask rather than report an empty one.
        let mut env: Env = src.into();
        assert_eq!(env.code_env_keys().len(), 1);
        code_entries_are_a_superset(&mut env);

        // A flatten merges parent tiers into the overlay, which adds code keys
        // this env's index never saw; it must be rebuilt, not carried over.
        let mut root = Env::new();
        root.insert("&outer".into(), Value::int(1));
        let mut leaf = scoped_with(root, &[("z", 3)]);
        assert_eq!(
            leaf.code_env_keys().len(),
            0,
            "overlay-only, parent excluded"
        );
        let mut flat = leaf.flattened();
        assert_eq!(flat.code_env_keys().len(), 1);
        code_entries_are_a_superset(&mut flat);
    }

    #[test]
    fn remove_overlay_sym_does_not_tombstone_the_parent_tier() {
        let mut root = Env::new();
        root.insert("&f".into(), Value::int(1));
        let mut leaf = scoped_with(root, &[]);
        leaf.insert("&f".into(), Value::int(2));
        assert_eq!(leaf.get_sym(s("&f")), Some(&Value::int(2)));
        // Undoing this tier's write must let the parent's binding shadow back
        // through -- unlike `remove_sym`, which would tombstone the name.
        leaf.remove_overlay_sym(s("&f"));
        assert_eq!(leaf.get_sym(s("&f")), Some(&Value::int(1)));
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

    /// Build the tier a closure capture would install (ADR-0092).
    fn capture_of(writes: &[(&str, i64)]) -> std::sync::Arc<Tier> {
        let mut e = Env::new();
        for (k, v) in writes {
            e.insert((*k).to_string(), Value::int(*v));
        }
        e.capture_tier()
    }

    #[test]
    fn a_capture_fallback_is_the_lowest_tier_of_the_chain() {
        let mut root = Env::new();
        root.insert("shared".into(), Value::int(1));
        let mut frame = scoped_with(root, &[("own", 2)]);
        frame.set_capture_fallback(capture_of(&[("shared", 99), ("captured", 3)]));

        // The chain wins over the capture -- that is the `entry_or_insert_sym_with`
        // don't-overwrite default the tier replaces.
        assert_eq!(frame.get_sym(s("shared")), Some(&Value::int(1)));
        // A name the chain does not provide comes from the capture.
        assert_eq!(frame.get_sym(s("captured")), Some(&Value::int(3)));
        assert!(frame.contains_key_sym(s("captured")));
        assert!(!frame.contains_key_sym(s("missing")));
        // Iteration stays overlay-only, so the exit writeback does not see a
        // captured name the body never touched.
        assert_eq!(frame.keys().copied().collect::<Vec<_>>(), vec![s("own")]);
    }

    #[test]
    fn a_callee_chained_over_a_closure_frame_still_sees_its_capture() {
        let mut frame = Env::scoped_child(Env::new());
        frame.set_capture_fallback(capture_of(&[("captured", 3)]));
        let callee = scoped_with(frame, &[("local", 4)]);
        // The merge used to leave captured names in the closure's overlay, which
        // a callee resolves through its parent chain. The fallback pass has to
        // consult every tier's fallback, not just the leaf's, to keep that.
        assert_eq!(callee.get_sym(s("captured")), Some(&Value::int(3)));
    }

    #[test]
    fn the_tail_most_capture_wins_between_nested_closures() {
        let mut outer = Env::scoped_child(Env::new());
        outer.set_capture_fallback(capture_of(&[("x", 1)]));
        let mut inner = Env::scoped_child(outer);
        inner.set_capture_fallback(capture_of(&[("x", 2)]));
        // The outer closure's frame is the inner one's enclosing lexical scope.
        assert_eq!(inner.get_sym(s("x")), Some(&Value::int(1)));
        // Flattening must reproduce the tiered answer exactly.
        assert_eq!(inner.flattened().get_sym(s("x")), Some(&Value::int(1)));
    }

    #[test]
    fn a_write_shadows_the_capture_and_a_remove_tombstones_it() {
        let mut frame = Env::scoped_child(Env::new());
        frame.set_capture_fallback(capture_of(&[("captured", 3)]));
        // `get_mut` promotes a captured name into the overlay, or the write
        // would land nowhere.
        *frame.get_mut("captured").expect("promoted") = Value::int(7);
        assert_eq!(frame.get_sym(s("captured")), Some(&Value::int(7)));
        // A removal has to tombstone: the capture is a tier below, so dropping
        // the overlay entry alone would make the captured value reappear.
        assert_eq!(frame.remove("captured"), Some(Value::int(7)));
        assert!(frame.get_sym(s("captured")).is_none());
        assert!(!frame.contains_key_sym(s("captured")));
    }

    #[test]
    fn flattening_folds_the_capture_in_under_the_chain() {
        let mut root = Env::new();
        root.insert("shared".into(), Value::int(1));
        let mut frame = scoped_with(root, &[("own", 2)]);
        frame.set_capture_fallback(capture_of(&[("shared", 99), ("captured", 3)]));
        let flat = frame.flattened();
        assert!(!flat.is_scoped());
        assert_eq!(flat.get_sym(s("shared")), Some(&Value::int(1)));
        assert_eq!(flat.get_sym(s("own")), Some(&Value::int(2)));
        assert_eq!(flat.get_sym(s("captured")), Some(&Value::int(3)));
    }

    #[test]
    fn capture_tier_of_a_closure_frame_carries_its_own_capture() {
        // A `Sub` built straight from a live closure frame (`whenever`) keeps
        // that frame's capture in a fallback, not its overlay, so the tier the
        // next call installs has to be the union of the two.
        let mut frame = Env::scoped_child(Env::new());
        frame.insert("own".into(), Value::int(2));
        frame.set_capture_fallback(capture_of(&[("own", 99), ("outer", 1)]));
        let tier = frame.capture_tier();
        assert_eq!(tier.get(&s("outer")), Some(&Value::int(1)));
        // The overlay shadows the fallback within the merged view.
        assert_eq!(tier.get(&s("own")), Some(&Value::int(2)));
    }

    #[test]
    fn a_frame_carrying_a_capture_is_never_skipped_as_an_empty_tier() {
        // `scoped_child`'s empty-tier reuse treats a write-free overlay as
        // invisible. A fallback makes the tier visible even so; skipping it
        // would drop the capture from the chain entirely.
        let mut frame = Env::scoped_child(Env::new());
        frame.set_capture_fallback(capture_of(&[("captured", 3)]));
        assert!(frame.overlay_is_shared_empty());
        let child = Env::scoped_child(frame);
        assert_eq!(child.get_sym(s("captured")), Some(&Value::int(3)));
    }

    #[test]
    fn two_frames_with_different_captures_have_different_tier_identities() {
        // The capture memo keys on `tier_addrs`; without the fallback in it,
        // two closures from one factory called in turn would share a capture.
        let mut a = Env::scoped_child(Env::new());
        a.set_capture_fallback(capture_of(&[("x", 1)]));
        let mut b = a.clone();
        b.set_capture_fallback(capture_of(&[("x", 2)]));
        assert!(a.tier_addrs().expect("describable") != b.tier_addrs().expect("describable"));
        assert!(!a.tier_addrs().expect("describable").matches(&b.tier_maps()));
        assert!(a.tier_addrs().expect("describable").matches(&a.tier_maps()));
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
    fn filtered_flat_outermost_tier_rejections_do_not_leak() {
        // The outermost tier is walked into a still-empty `out`, so its rejected
        // keys take the no-remove fast path (#7565): a rejection there cannot be
        // suppressing an outer tier's kept entry, because there is no outer
        // tier. Pin that the result is exactly what a remove-on-every-rejection
        // walk gives -- a rejected key absent either way, and a nearer tier's
        // kept entry of a name the outermost tier rejected still present.
        let mut root = Env::new();
        root.insert("keep".into(), Value::int(1));
        root.insert("outer-drop".into(), Value::int(2));
        root.insert("shadowed".into(), Value::int(3));
        let mut leaf = Env::scoped_child(root);
        leaf.insert("shadowed".into(), Value::int(4));
        leaf.insert("leaf-drop".into(), Value::int(5));

        let merged = leaf
            .filtered_flat(&|k, _v| k.with_str(|name| name != "outer-drop" && name != "leaf-drop"));
        assert_eq!(merged.get_sym(s("keep")), Some(&Value::int(1)));
        assert!(
            merged.get_sym(s("outer-drop")).is_none(),
            "a key the outermost tier rejects must not survive"
        );
        assert!(
            merged.get_sym(s("leaf-drop")).is_none(),
            "a key a nearer tier rejects must not survive"
        );
        assert_eq!(
            merged.get_sym(s("shadowed")),
            Some(&Value::int(4)),
            "the nearer tier's entry must still shadow the outermost one"
        );
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
