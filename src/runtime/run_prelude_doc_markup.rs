use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// The two answers of `RakuAST::Doc::Block.allowed-markup` (ADR-14916):
/// `Map` subclasses used as type objects, `OK` allowing every markup letter
/// and `NOK` none. Measured on rakudo 2026.09: `OK<X>` is `True` and
/// `NOK<X>` is `False` for any key.
const DOC_MARKUP_PRELUDE: &str = r#"
class GLOBAL::RakuAST::Doc::Block::OK is Map {
    method AT-KEY(\key) { True }
}
class GLOBAL::RakuAST::Doc::Block::NOK is Map {
    method AT-KEY(\key) { False }
}
"#;

impl Interpreter {
    /// Prepend [`DOC_MARKUP_PRELUDE`] to a unit that asks a block for its
    /// `allowed-markup` or names one of the two classes.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_doc_markup_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !(source.contains("allowed-markup") || source.contains("RakuAST::Doc::Block::"))
            || source.contains("class RakuAST::Doc::Block::")
        {
            return;
        }
        use std::sync::OnceLock;
        static STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(DOC_MARKUP_PRELUDE)
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
