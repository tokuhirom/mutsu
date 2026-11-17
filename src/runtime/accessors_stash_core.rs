//! Single-key reads of the `CORE::` / `SETTING::` stash and of the
//! per-revision `CORE::v6c` / `CORE::v6d` / `CORE::v6e` packages, answered
//! without materializing the whole core stash (#17698, #17776).
//!
//! The whole `CORE::` build (`package_stash_value_with`) lists one `&name`
//! entry per built-in routine; that is ~1000 `&name` resolutions plus a
//! `user_declares_routine` probe each, and a program that only wants
//! `CORE::{$rev}` (Rakudo-Type-Introspection reads
//! `CORE::{$rev}.WHO{$type}` for every type it inspects) paid all of it per
//! access. Everything a keyed read can observe is decided by the key alone:
//! the build holds exactly the `&name` entries, and the stash's `AT-KEY`
//! resolves every other core symbol (`v6c`, ...) lazily.
use super::*;

impl Interpreter {
    /// The one entry `package_stash_value(package_name)` holds under `key`
    /// for `CORE` / `SETTING`, or `None` when that build would hold none.
    ///
    /// The build lists `&name` for every built-in routine the program does
    /// not itself declare; no other key is ever in it (`CORE::<v6c>` and the
    /// other core symbols are answered by the stash's lazy `AT-KEY`).
    // Cost: O(1) expected for a non-`&` key or a name that is not a built-in;
    // otherwise one `user_declares_routine` probe plus one `&name` resolution.
    pub(super) fn core_stash_member(&self, package_name: &str, key: &str) -> Option<Value> {
        let bare = key.strip_prefix('&')?;
        if !crate::builtin_functions::is_builtin_function(bare) || self.user_declares_routine(bare)
        {
            return None;
        }
        let value =
            self.resolve_code_var(crate::qualified::qualified_text(package_name, bare).as_str());
        (!value.is_nil()).then_some(value)
    }
}
