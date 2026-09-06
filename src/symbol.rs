use rustc_hash::FxHashMap;
use serde::{Deserialize, Deserializer, Serialize, Serializer};
use std::cell::RefCell;
use std::fmt;
use std::sync::{OnceLock, RwLock};

/// An interned symbol — a lightweight handle that supports O(1) equality
/// comparison instead of byte-by-byte string comparison.
///
/// Symbol-to-Symbol comparison is O(1) integer comparison.
/// Symbol-to-`&str` comparison falls back to a table lookup (for migration
/// convenience — prefer Symbol-to-Symbol where possible).
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

impl Serialize for Symbol {
    fn serialize<S: Serializer>(&self, serializer: S) -> Result<S::Ok, S::Error> {
        self.as_str().serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for Symbol {
    fn deserialize<D: Deserializer<'de>>(deserializer: D) -> Result<Self, D::Error> {
        let s = String::deserialize(deserializer)?;
        Ok(Symbol::intern(&s))
    }
}

impl PartialEq<&str> for Symbol {
    fn eq(&self, other: &&str) -> bool {
        self.as_str() == *other
    }
}

impl PartialEq<str> for Symbol {
    fn eq(&self, other: &str) -> bool {
        self.as_str() == other
    }
}

/// Interned strings are leaked (`Box::leak`) so a `Symbol` can hand out a
/// `&'static str` without holding the table lock. The table is append-only
/// and lives for the whole process, so the leak is exactly the table's own
/// lifetime — no unbounded growth beyond what `Vec<String>` storage had.
struct SymbolTable {
    str_to_id: FxHashMap<&'static str, Symbol>,
    id_to_str: Vec<&'static str>,
}

static GLOBAL_TABLE: OnceLock<RwLock<SymbolTable>> = OnceLock::new();

/// The two capture-variable name shapes the regex engine parks in the env.
///
/// `$0`, `$1`, ... are stored under all-digit keys and `$<name>` under
/// `<name>` (angle-wrapped), sigil-less like every other env key.
#[derive(Clone, Copy, PartialEq, Eq)]
pub(crate) enum CaptureShape {
    /// An all-digit name: a positional capture (`$0`).
    Numeric,
    /// An angle-wrapped name (`<foo>`): a named capture (`$<foo>`).
    Angle,
}

/// Classify a name by capture shape. Cheap enough to run once per *newly
/// interned* string; never on a repeat intern (the caches short-circuit those).
#[inline]
fn capture_shape_of(s: &str) -> Option<CaptureShape> {
    let bytes = s.as_bytes();
    match bytes.first() {
        Some(b'0'..=b'9') if bytes.iter().all(|b| b.is_ascii_digit()) => {
            Some(CaptureShape::Numeric)
        }
        Some(b'<') if bytes.len() > 2 && bytes[bytes.len() - 1] == b'>' => {
            Some(CaptureShape::Angle)
        }
        _ => None,
    }
}

/// Every capture-shaped symbol the process has ever interned, split by shape.
///
/// `reset_capture_env_vars` has to shadow *stale* capture variables before a
/// new match installs its own, and they may be inherited from a caller frame
/// rather than declared in the callee's own overlay. Scanning the whole visible
/// env for them is O(env) per match with a `String` allocation per key — it was
/// measured at 40% of `bench-string` (5000 matches x ~75k instructions).
///
/// The env cannot hold a key that was never interned, so this registry is a
/// superset of the capture keys any env can contain: iterating it and probing
/// `contains_key_sym` is O(capture names), which is a handful in any real
/// program, and costs no allocation per key. The table is append-only, so an
/// entry recorded here stays valid for the life of the process.
static CAPTURE_SHAPED: OnceLock<RwLock<(Vec<Symbol>, Vec<Symbol>)>> = OnceLock::new();

fn capture_shaped() -> &'static RwLock<(Vec<Symbol>, Vec<Symbol>)> {
    CAPTURE_SHAPED.get_or_init(|| RwLock::new((Vec::new(), Vec::new())))
}

/// Snapshot of the capture-shaped symbols interned so far, as
/// `(numeric, angle)`.
///
/// Returns owned vectors rather than lending the guard out on purpose: the
/// caller mutates the env while iterating, and any interning on that path takes
/// the symbol-table write lock. Cloning two short vectors keeps the lock scope
/// to this function and the lock order trivially acyclic.
pub(crate) fn capture_shaped_symbols() -> (Vec<Symbol>, Vec<Symbol>) {
    let guard = capture_shaped().read().unwrap();
    guard.clone()
}

fn global_table() -> &'static RwLock<SymbolTable> {
    GLOBAL_TABLE.get_or_init(|| {
        RwLock::new(SymbolTable {
            str_to_id: FxHashMap::default(),
            id_to_str: Vec::new(),
        })
    })
}

thread_local! {
    /// Per-thread `str -> Symbol` memo in front of `GLOBAL_TABLE`. Valid for the
    /// whole process because interned ids are append-only and never remapped.
    /// Removes the global-`RwLock` read contention on the intern hot path (see
    /// `Symbol::intern`).
    static INTERN_CACHE: RefCell<FxHashMap<String, Symbol>> = RefCell::new(FxHashMap::default());

    /// Per-thread `id -> &'static str` memo in front of `GLOBAL_TABLE`, the
    /// mirror of `INTERN_CACHE` for the resolve direction. Interned strings are
    /// leaked and ids never remapped, so a cached entry is valid forever.
    /// Keeps `as_str` (the single hottest symbol operation — every dispatch
    /// class-name borrow, `==` compare, `starts_with`, `Display`) off the
    /// globally-shared `RwLock`.
    static RESOLVE_CACHE: RefCell<Vec<Option<&'static str>>> = const { RefCell::new(Vec::new()) };
}

/// Pre-interned symbols for names the VM resolves on hot paths.
///
/// [`Symbol::intern`] hashes the whole string and takes a thread-local borrow,
/// so re-interning a fixed name inside a per-call code path is pure overhead —
/// it showed up as `Symbol::intern` + `LocalKey::with` in a `bench-fib`
/// profile. Each accessor resolves once per process and is a plain load
/// afterwards. Interned ids are global and append-only, so caching them in a
/// `OnceLock` is valid for the life of the process and across threads.
pub(crate) mod wk {
    use super::Symbol;

    macro_rules! well_known {
        ($($(#[$m:meta])* $f:ident => $s:literal;)*) => {$(
            $(#[$m])*
            #[inline(always)]
            pub(crate) fn $f() -> Symbol {
                static CELL: std::sync::OnceLock<Symbol> = std::sync::OnceLock::new();
                *CELL.get_or_init(|| Symbol::intern($s))
            }
        )*};
    }

    well_known! {
        /// The topic `$_`. Env keys are stored sigil-less, so this is `"_"`.
        topic => "_";
        /// The `Any` type object, the value a routine's fresh topic is seeded with.
        any => "Any";
        /// The dynamic `$?FILE`, stored sigil-less with its twigil.
        file => "?FILE";
        /// A lexically rebound `&return`. Probed on EVERY routine return (both
        /// the interpreter's `OpCode::Return` and the JIT's `ret` shim), so it
        /// must never be re-interned there -- the thread-local intern cache is
        /// a string-keyed hash lookup, which showed up as 5.3% of `bench-fib`.
        rebound_return => "&return";
        /// The invocant, `self`. One of the fixed env keys every compiled
        /// method call writes on entry (see the family below).
        self_ => "self";
        /// `$.foo` desugaring's alias for the invocant, written next to `self`.
        anon_state => "__ANON_STATE__";
        /// `::?CLASS`, written on every compiled method entry.
        class_decl => "?CLASS";
        /// `::?ROLE`: written when the method came from a role, removed
        /// otherwise — so a method call touches this key either way.
        role_decl => "?ROLE";
        /// The per-routine `$!`, reset to Nil on entry.
        error_var => "!";
        /// The invocation id a non-local return from an inner block targets.
        callable_id => "__mutsu_callable_id";
        /// The implicit `*%_` named slurpy every method carries.
        named_slurpy => "%_";
    }

    /// Whether `key` is one of the fixed per-call env keys the well-known
    /// method-entry family above covers. Only used by debug assertions and
    /// tests that check the string-keyed and symbol-keyed spellings agree.
    #[cfg(test)]
    pub(crate) fn method_entry_keys() -> [(Symbol, &'static str); 7] {
        [
            (self_(), "self"),
            (anon_state(), "__ANON_STATE__"),
            (class_decl(), "?CLASS"),
            (role_decl(), "?ROLE"),
            (error_var(), "!"),
            (callable_id(), "__mutsu_callable_id"),
            (named_slurpy(), "%_"),
        ]
    }
}

impl Symbol {
    /// The raw interned id. Only for storing a `Symbol` where a plain integer is
    /// needed (an `AtomicU32` slot); pair with [`Symbol::from_raw`].
    pub(crate) fn raw(self) -> u32 {
        self.0
    }

    /// Rebuild a `Symbol` from an id produced by [`Symbol::raw`]. Ids are
    /// append-only and never remapped, so a round trip is always valid.
    pub(crate) fn from_raw(id: u32) -> Symbol {
        Symbol(id)
    }

    /// Intern a string and return its `Symbol`.  If the string has already been
    /// interned, the existing symbol is returned (idempotent).
    pub fn intern(s: &str) -> Symbol {
        // Thread-local memo first: interned symbols are global and append-only
        // (an id, once assigned to a string, is never reused or remapped), so a
        // cached `str -> Symbol` mapping is valid for the life of the process
        // and can never go stale. Serving repeat interns from here keeps the hot
        // path (re-interning the same variable/package names every loop
        // iteration) entirely off the globally-shared `RwLock`, which otherwise
        // read-contends across worker threads and serializes CPU-bound `start`
        // blocks (profiled: `Symbol::intern` was 90% of an 8-thread run, 43% of
        // it in `RwLock::read_contended`).
        if let Some(sym) = INTERN_CACHE.with(|c| c.borrow().get(s).copied()) {
            return sym;
        }
        let sym = Self::intern_global(s);
        INTERN_CACHE.with(|c| {
            c.borrow_mut().insert(s.to_owned(), sym);
        });
        sym
    }

    /// Look a string up *without* interning it: `Some` only if the string has
    /// already been interned by someone. Used by [`crate::value::AttrMap`]'s
    /// string-keyed convenience lookups (`attrs.get("name")`), where a name that
    /// was never interned cannot possibly be a key in the map — so a miss must
    /// not grow the (append-only, leaked) symbol table with a name nothing else
    /// uses.
    pub fn lookup(s: &str) -> Option<Symbol> {
        if let Some(sym) = INTERN_CACHE.with(|c| c.borrow().get(s).copied()) {
            return Some(sym);
        }
        let sym = {
            let table = global_table().read().unwrap();
            table.str_to_id.get(s).copied()
        }?;
        INTERN_CACHE.with(|c| {
            c.borrow_mut().insert(s.to_owned(), sym);
        });
        Some(sym)
    }

    /// Intern via the globally-shared table (the source of truth for id
    /// assignment). Only reached on a thread-local cache miss.
    fn intern_global(s: &str) -> Symbol {
        // Fast path: read lock only.
        {
            let table = global_table().read().unwrap();
            if let Some(&sym) = table.str_to_id.get(s) {
                return sym;
            }
        }
        // Slow path: acquire write lock and insert.
        let mut table = global_table().write().unwrap();
        // Double-check after acquiring write lock.
        if let Some(&sym) = table.str_to_id.get(s) {
            return sym;
        }
        let id = table.id_to_str.len() as u32;
        let sym = Symbol(id);
        let leaked: &'static str = Box::leak(s.to_owned().into_boxed_str());
        table.id_to_str.push(leaked);
        table.str_to_id.insert(leaked, sym);
        // Record the capture shape once, here, where a name becomes a symbol
        // for the first time. Doing it at the intern choke point rather than at
        // the (many) sites that insert a capture into the env is what makes the
        // registry a guaranteed superset: no key can reach an env without
        // passing through here first.
        if let Some(shape) = capture_shape_of(leaked) {
            // Push while the symbol-table write lock is STILL held, so no other
            // thread can observe the name as interned before it is registered:
            // a racing thread that saw the table entry first would use the
            // symbol as an env key that `reset_capture_env_vars` then missed.
            // Lock order is one-way (table write -> registry write; the reader
            // takes the registry lock alone and releases it before touching an
            // env), so there is no cycle to deadlock on.
            let mut shaped = capture_shaped().write().unwrap();
            match shape {
                CaptureShape::Numeric => shaped.0.push(sym),
                CaptureShape::Angle => shaped.1.push(sym),
            }
        }
        sym
    }

    /// Borrow the symbol's string without allocating. The returned `&'static
    /// str` is valid for the whole process (interned strings are never freed),
    /// and no lock is held after this returns — safe to keep across arbitrary
    /// downstream calls, unlike `with_str`'s old lock-scoped borrow.
    pub fn as_str(&self) -> &'static str {
        let idx = self.0 as usize;
        RESOLVE_CACHE.with(|c| {
            let mut cache = c.borrow_mut();
            if let Some(Some(s)) = cache.get(idx) {
                return *s;
            }
            let s = global_table().read().unwrap().id_to_str[idx];
            if cache.len() <= idx {
                cache.resize(idx + 1, None);
            }
            cache[idx] = Some(s);
            s
        })
    }

    /// Resolve the symbol back to its string representation.
    /// Prefer `as_str()` on hot paths — this allocates a fresh `String`.
    pub fn resolve(&self) -> String {
        self.as_str().to_owned()
    }

    /// Return the internal numeric ID of this symbol (unique per interned string).
    pub fn id(&self) -> u32 {
        self.0
    }

    /// Rebuild a `Symbol` from an ID previously obtained via [`Symbol::id`].
    /// For the NaN-boxed `Value` payload round-trip (layer 3b-1) only: the ID
    /// must come from a real interned symbol, or later resolution will index
    /// out of bounds.
    pub(crate) fn from_id(id: u32) -> Symbol {
        Symbol(id)
    }

    /// Execute a closure with a borrowed reference to the underlying string.
    /// The lock is released before the closure runs (see `as_str`).
    pub fn with_str<F, R>(&self, f: F) -> R
    where
        F: FnOnce(&str) -> R,
    {
        f(self.as_str())
    }

    pub fn starts_with(&self, prefix: &str) -> bool {
        self.as_str().starts_with(prefix)
    }

    pub fn ends_with(&self, suffix: &str) -> bool {
        self.as_str().ends_with(suffix)
    }

    pub fn contains_str(&self, needle: &str) -> bool {
        self.as_str().contains(needle)
    }

    pub fn strip_prefix_str(&self, prefix: &str) -> Option<String> {
        self.as_str().strip_prefix(prefix).map(|r| r.to_owned())
    }

    pub fn strip_prefix_char(&self, prefix: char) -> Option<String> {
        self.as_str().strip_prefix(prefix).map(|r| r.to_owned())
    }

    pub fn rsplit_once_str(&self, delimiter: &str) -> Option<(String, String)> {
        self.as_str()
            .rsplit_once(delimiter)
            .map(|(a, b)| (a.to_owned(), b.to_owned()))
    }

    pub fn len(&self) -> usize {
        self.as_str().len()
    }

    pub fn is_empty(&self) -> bool {
        self.as_str().is_empty()
    }
}

/// How many distinct strings the process has interned so far.
///
/// The table is append-only and its entries are leaked for the process
/// lifetime, so this doubles as a leak gauge: in a long-lived process (the
/// language server of ADR-0065) a workload that keeps growing this number while
/// re-analysing the *same* document is manufacturing fresh names, and every one
/// of them costs permanent memory. Pinned by `tests/long_lived_parse.rs`.
pub fn interned_count() -> usize {
    global_table().read().unwrap().id_to_str.len()
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Symbol({}: {:?})", self.0, self.as_str())
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(self.as_str())
    }
}

/// Fixed names the runtime interns over and over on hot paths.
///
/// `Symbol::intern` is already memoized per thread, but the memo is a
/// `FxHashMap<String, Symbol>` — a repeat intern still hashes the whole string
/// and compares it. For a literal that is re-interned on *every* closure
/// creation or method dispatch, that is pure waste: symbol ids are process-global
/// and append-only, so the id can be computed once and copied thereafter.
///
/// Add a name here only when a profile shows the intern on a per-operation hot
/// path; a `LazyLock` read is cheap but not free.
pub(crate) mod well_known {
    use super::Symbol;
    use std::sync::LazyLock;

    /// The empty name every anonymous closure value carries.
    #[inline]
    pub(crate) fn anon() -> Symbol {
        static SYM: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern(""));
        *SYM
    }

    /// Closure-identity metadata: the `WhateverCode` marker.
    #[inline]
    pub(crate) fn callable_type() -> Symbol {
        static SYM: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("__mutsu_callable_type"));
        *SYM
    }

    /// The declared return type a routine's env carries for its own body.
    #[inline]
    pub(crate) fn return_type() -> Symbol {
        static SYM: LazyLock<Symbol> = LazyLock::new(|| Symbol::intern("__mutsu_return_type"));
        *SYM
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    #[test]
    fn intern_is_idempotent() {
        let a = Symbol::intern("hello");
        let b = Symbol::intern("hello");
        assert_eq!(a, b);
    }

    #[test]
    fn different_strings_produce_different_symbols() {
        let a = Symbol::intern("foo_test");
        let b = Symbol::intern("bar_test");
        assert_ne!(a, b);
    }

    #[test]
    fn resolve_roundtrip() {
        let sym = Symbol::intern("roundtrip_test");
        assert_eq!(sym.resolve(), "roundtrip_test");
    }

    #[test]
    fn as_str_borrows_without_alloc() {
        let sym = Symbol::intern("as_str_test");
        let a: &'static str = sym.as_str();
        let b: &'static str = sym.as_str();
        assert_eq!(a, "as_str_test");
        // Same interned symbol hands out the same leaked storage.
        assert!(std::ptr::eq(a, b));
    }

    #[test]
    fn display_shows_string() {
        let sym = Symbol::intern("display_test");
        assert_eq!(format!("{}", sym), "display_test");
    }

    #[test]
    fn debug_shows_id_and_string() {
        let sym = Symbol::intern("debug_test");
        let dbg = format!("{:?}", sym);
        assert!(dbg.contains("debug_test"), "debug output: {}", dbg);
        assert!(dbg.starts_with("Symbol("), "debug output: {}", dbg);
    }

    #[test]
    fn symbol_is_copy() {
        let a = Symbol::intern("copy_test");
        let b = a; // Copy
        let _c = a; // still usable
        assert_eq!(a, b);
    }

    #[test]
    fn symbol_can_be_hash_key() {
        let mut map = HashMap::new();
        let sym = Symbol::intern("hashkey_test");
        map.insert(sym, 42);
        assert_eq!(map.get(&sym), Some(&42));
    }

    #[test]
    fn capture_shape_matches_the_old_env_scan_predicates() {
        // These two must stay exactly equivalent to the predicates
        // `reset_capture_env_vars` used to hand `Env::visible_keys_where`,
        // since the registry replaced that scan.
        let numeric = |s: &str| !s.is_empty() && s.chars().all(|ch| ch.is_ascii_digit());
        let angle = |s: &str| s.len() > 2 && s.starts_with('<') && s.ends_with('>');
        for s in [
            "",
            "0",
            "1",
            "42",
            "007",
            "a",
            "0a",
            "a0",
            "<>",
            "<a>",
            "<ab>",
            "<",
            ">",
            "<a",
            "a>",
            "_",
            "$/",
            "<0>",
            "1<2>",
            "\u{3042}",
            "<\u{3042}>",
            "x<y>",
        ] {
            let shape = capture_shape_of(s);
            assert_eq!(
                matches!(shape, Some(CaptureShape::Numeric)),
                numeric(s),
                "numeric shape disagrees for {s:?}"
            );
            assert_eq!(
                matches!(shape, Some(CaptureShape::Angle)),
                angle(s),
                "angle shape disagrees for {s:?}"
            );
        }
    }

    #[test]
    fn interning_a_capture_shaped_name_registers_it() {
        // The registry is a superset of the capture keys an env can hold, and
        // that only holds if `intern` records every capture-shaped name.
        let num = Symbol::intern("31337");
        let named = Symbol::intern("<capture_shape_registry_probe>");
        let plain = Symbol::intern("capture_shape_registry_plain");
        let (numeric, angle) = capture_shaped_symbols();
        assert!(numeric.contains(&num), "all-digit name was not registered");
        assert!(angle.contains(&named), "angle name was not registered");
        assert!(!numeric.contains(&plain) && !angle.contains(&plain));
        // Re-interning must not duplicate the entry.
        let before = capture_shaped_symbols().0.len();
        let _ = Symbol::intern("31337");
        assert_eq!(capture_shaped_symbols().0.len(), before);
    }

    #[test]
    fn method_entry_well_known_symbols_match_their_strings() {
        // The compiled method-call paths write these env keys symbol-keyed
        // (`insert_sym`) instead of allocating a `String` per call. A typo in
        // one of the literals would not fail to compile -- it would silently
        // write a DIFFERENT env key, so `self` / `?CLASS` / `%_` would read as
        // undefined inside every method body. Pin each against the string
        // spelling the interpreter's other (string-keyed) readers use.
        for (sym, text) in wk::method_entry_keys() {
            assert_eq!(sym, Symbol::intern(text), "well-known symbol {text:?}");
            assert!(
                sym.with_str(|s| s == text),
                "well-known symbol {text:?} resolves to a different string"
            );
        }
    }
}
