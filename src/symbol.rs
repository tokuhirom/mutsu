use std::collections::HashMap;
use std::fmt;
use std::sync::{OnceLock, RwLock};

/// An interned symbol — a lightweight handle that supports O(1) equality
/// comparison instead of byte-by-byte string comparison.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct Symbol(u32);

struct SymbolTable {
    str_to_id: HashMap<String, Symbol>,
    id_to_str: Vec<String>,
}

static GLOBAL_TABLE: OnceLock<RwLock<SymbolTable>> = OnceLock::new();

fn global_table() -> &'static RwLock<SymbolTable> {
    GLOBAL_TABLE.get_or_init(|| {
        RwLock::new(SymbolTable {
            str_to_id: HashMap::new(),
            id_to_str: Vec::new(),
        })
    })
}

impl Symbol {
    /// Intern a string and return its `Symbol`.  If the string has already been
    /// interned, the existing symbol is returned (idempotent).
    pub fn intern(s: &str) -> Symbol {
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
        table.id_to_str.push(s.to_owned());
        table.str_to_id.insert(s.to_owned(), sym);
        sym
    }

    /// Look up the original string for this symbol.
    pub fn as_str(&self) -> SymbolStr {
        SymbolStr { sym: *self }
    }

    /// Get the string slice for this symbol.  Requires holding the read lock
    /// briefly, so prefer [`as_str`](Self::as_str) when you need a displayable
    /// value rather than calling this in a tight loop.
    pub fn resolve(&self) -> String {
        let table = global_table().read().unwrap();
        table.id_to_str[self.0 as usize].clone()
    }
}

impl fmt::Debug for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let table = global_table().read().unwrap();
        write!(f, "Symbol({}: {:?})", self.0, &table.id_to_str[self.0 as usize])
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let table = global_table().read().unwrap();
        f.write_str(&table.id_to_str[self.0 as usize])
    }
}

/// A thin wrapper returned by [`Symbol::as_str`] that implements `Display`
/// without requiring the caller to hold a lock manually.
pub struct SymbolStr {
    sym: Symbol,
}

impl fmt::Display for SymbolStr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.sym.fmt(f)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
