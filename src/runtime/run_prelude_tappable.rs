//! The core `Tappable` role, as real Raku source.
//!
//! Rakudo's `Supply` is built on `my role Tappable` (src/core.c/Supply.rakumod):
//! a class that `does Tappable` writes `tap`, `live` and `sane` itself and
//! inherits `serial`. The role is visible to user code, and ecosystem code
//! composes it (the EEL slang's `MatcherSupply`). The stubs below are
//! required methods, exactly as in rakudo: a composing class that omits one
//! fails to compile with "Method 'live' must be implemented by M because it is
//! required by roles: Tappable". The built-in `Supply` keeps its native tap
//! machinery; it is not an instance of a class that composes this role.

use super::source_code_text::CodeText;
use super::*;

/// Measured on rakudo 2026.09: `Tappable.^methods(:local)` is `tap live
/// serial sane`; omitting `tap`, `live` or `sane` is a compile error, omitting
/// `serial` is not, and the inherited `serial` returns the invocant.
const TAPPABLE_ROLE_PRELUDE: &str = r#"
role GLOBAL::Tappable {
    method tap(&emit, &done, &quit, &tap) {...}
    method live() {...}
    method serial() { self }
    method sane() {...}
}
"#;

impl Interpreter {
    /// Prepend [`TAPPABLE_ROLE_PRELUDE`] to a unit that mentions `Tappable`,
    /// unless it declares its own `role Tappable`. Injected for modules too:
    /// the unit that composes the role is usually a module the program only
    /// loads.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_tappable_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("Tappable") || source.contains("role Tappable") {
            return;
        }
        use std::sync::OnceLock;
        static TAPPABLE_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = TAPPABLE_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(TAPPABLE_ROLE_PRELUDE)
                .map(|(s, _)| s)
                .unwrap_or_default()
        });
        if prelude.is_empty() {
            return;
        }
        let mut combined = prelude.clone();
        combined.append(stmts);
        *stmts = combined;
    }
}
