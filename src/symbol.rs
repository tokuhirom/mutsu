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

impl Symbol {
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
}
