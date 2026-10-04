//! Parsing the Raku source of a builtin prelude.
//!
//! A prelude's statements are parsed once per process and spliced into every
//! program or module that needs them, so the declaration ids and anonymous
//! names its parse mints end up in that module's compiled code. Minted from
//! the process counters they would depend on what the process parsed first,
//! and a module's compile could not be reused by another process (ADR-11756
//! §2.3). So each prelude is parsed under a content-addressed session keyed on
//! its own text: every process mints the same values for it.

use crate::ast::Stmt;
use crate::value::RuntimeError;

/// Salt keeping prelude sessions apart from module sessions, whose unit keys
/// hash a path together with the source.
const PRELUDE_UNIT_SALT: u64 = 0x7072_656c_7564_6521;

/// [`crate::parse_dispatch::parse_source`] for the constant source of a
/// builtin prelude (see the module docs).
// Cost: O(n), n = prelude source bytes.
pub(crate) fn parse_prelude_source(src: &str) -> Result<(Vec<Stmt>, Option<String>), RuntimeError> {
    let unit_key = crate::precomp::content_hash(src.as_bytes()) ^ PRELUDE_UNIT_SALT;
    let session = crate::compiler::compile_session::content_parse_session_id(unit_key, 0);
    crate::anon_names::with_content_unit(session, || crate::parse_dispatch::parse_source(src))
}
