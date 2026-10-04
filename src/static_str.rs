//! A `&'static str` that the compiled-bytecode cache can store.
//!
//! Compiled data holds a few fixed strings by `&'static str` (a TRIR
//! parameter's type spelling, a role body's `our`-scope diagnostic). Decoding
//! has nothing to borrow such a string from, so these fields use this newtype:
//! it reads like a `&str`, and the codec (`precomp_codec`) restores it through
//! the symbol table, whose interned strings live for the whole process.

/// A string borrowed for the whole process.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub(crate) struct StaticStr(pub(crate) &'static str);

impl std::ops::Deref for StaticStr {
    type Target = str;
    fn deref(&self) -> &str {
        self.0
    }
}

impl std::fmt::Display for StaticStr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str(self.0)
    }
}

impl PartialEq<&str> for StaticStr {
    fn eq(&self, other: &&str) -> bool {
        self.0 == *other
    }
}
