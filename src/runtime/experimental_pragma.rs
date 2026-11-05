//! `use experimental :pack` (#13346).
//!
//! Rakudo gates `pack` / `unpack` behind the pragma: the routine forms are
//! undeclared without it and the `Blob.unpack` method raises `X::Experimental`.
//! The sub form is judged by the undeclared-routine check
//! (`undeclared_routines`); the method form is dynamic, so the pragma records
//! the source file it appears in and `Blob.unpack` asks whether the code it is
//! called from belongs to such a file. A file is the granularity, not a
//! lexical block: a pragma inside a block enables the whole file, which only
//! errs towards accepting code Rakudo rejects.

use crate::symbol::Symbol;
use std::collections::HashSet;
use std::sync::Mutex;

static PACK_FILES: Mutex<Option<HashSet<Symbol>>> = Mutex::new(None);

/// Record that `file` said `use experimental :pack`.
// Cost: O(1) expected.
pub(crate) fn enable_pack(file: Option<Symbol>) {
    let mut files = PACK_FILES.lock().unwrap_or_else(|e| e.into_inner());
    files.get_or_insert_with(HashSet::new).insert(key(file));
}

/// Whether code of `file` may use `pack` / `unpack`.
// Cost: O(1) expected.
pub(crate) fn pack_enabled(file: Option<Symbol>) -> bool {
    let files = PACK_FILES.lock().unwrap_or_else(|e| e.into_inner());
    files.as_ref().is_some_and(|set| set.contains(&key(file)))
}

fn key(file: Option<Symbol>) -> Symbol {
    file.unwrap_or_else(|| Symbol::intern(""))
}
