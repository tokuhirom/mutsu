use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// `Formatter::Syntax` (Raku 6.e), the grammar of a `sprintf`-style format
/// string, written in Raku. The rule names and captures are those of
/// Rakudo's `src/core.e/Formatter.rakumod`: a `statement` is a `directive`,
/// a `panic` (an unsupported directive letter) or a `literal` run.
pub(super) const FORMATTER_SYNTAX_PRELUDE: &str = r#"
grammar GLOBAL::Formatter::Syntax {
    token TOP { <statement>* }
    token statement { <?[%]> [ <directive> || <panic> ] || <literal> }
    token literal { <-[%]>+ }
    token idx { \d+ '$' }
    token flags { <[\ +\-0#]> }
    token size { $<star>='*' <idx>? | \d* }
    token precision { '.' <size> }
    proto token directive {*}
    token directive:sym<%> { '%%' }
    token directive:sym<d> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[diD]> }
    token directive:sym<b> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[bB]> }
    token directive:sym<x> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[xX]> }
    token directive:sym<e> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[eE]> }
    token directive:sym<f> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[fF]> }
    token directive:sym<g> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[gG]> }
    token directive:sym<a> { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[aA]> }
    token directive:sym<c> { '%' <idx>? <flags>* <size> <precision>? 'c' }
    token directive:sym<s> { '%' <idx>? <flags>* <size> <precision>? 's' }
    token directive:sym<o> { '%' <idx>? <flags>* <size> <precision>? 'o' }
    token directive:sym<O> { '%' <idx>? <flags>* <size> <precision>? 'O' }
    token directive:sym<u> { '%' <idx>? <flags>* <size> <precision>? 'u' }
    token panic { '%' <idx>? <flags>* <size> <precision>? $<sym>=<[\s\S]> }
}
"#;

impl Interpreter {
    /// Prepend [`FORMATTER_SYNTAX_PRELUDE`] to a program (or module) that
    /// names `Formatter::Syntax`. Skipped when it declares its own.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_formatter_syntax_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("Formatter::Syntax") || source.contains("grammar Formatter::Syntax") {
            return;
        }
        use std::sync::OnceLock;
        static SYNTAX_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = SYNTAX_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(FORMATTER_SYNTAX_PRELUDE)
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
