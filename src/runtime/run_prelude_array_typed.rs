//! The `Array::Typed` role, as real Raku source.
//!
//! Rakudo mixes `Array::Typed[T]` into a typed Array (`Array[Str]`) and into
//! user subclasses (`Foo.^mixin(Array::Typed[of])`, the way a `^parameterize`
//! meta-method builds a `Foo[Str]`). mutsu models the built-in typed arrays
//! natively, so the role itself only needs to exist for a program that names
//! it: a parametric role that does `Positional[T]` and answers `.of`.

use super::source_code_text::CodeText;
use super::*;

const ARRAY_TYPED_ROLE_PRELUDE: &str = r#"
role GLOBAL::Array::Typed[::TValue] does Positional[TValue] {
    method of() { TValue }
}
"#;

impl Interpreter {
    /// Prepend [`ARRAY_TYPED_ROLE_PRELUDE`] to a program (or module) that names
    /// `Array::Typed` and does not declare it itself. Parsed once and cached,
    /// like [`Self::inject_iterator_role_prelude`].
    pub(super) fn inject_array_typed_role_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains_name("Array::Typed")
            || source.contains_name("role Array::Typed")
            || source.contains_name("class Array::Typed")
        {
            return;
        }
        use std::sync::OnceLock;
        static ARRAY_TYPED_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = ARRAY_TYPED_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(ARRAY_TYPED_ROLE_PRELUDE)
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
