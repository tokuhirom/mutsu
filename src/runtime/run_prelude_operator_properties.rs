use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// The `OperatorProperties` class `RakuAST` operator nodes answer from
/// `.properties` (ADR-14916). The attributes, their order and the `.raku`
/// rendering are rakudo 2026.09's: an empty string or a false flag is left
/// out, a flag prints as `:name`.
const OPERATOR_PROPERTIES_PRELUDE: &str = r#"
class GLOBAL::OperatorProperties {
    has str $.precedence;
    has str $.sub-precedence;
    has str $.associative;
    has str $.thunky;
    has str $.dba;
    has str $.next-term;
    has int $.iffy;
    has int $.diffy;
    has int $.fiddly;
    has int $.adverb;
    has int $.ternary;
    submethod TWEAK {
        $!iffy = $!iffy.Int;
        $!diffy = $!diffy.Int;
        $!fiddly = $!fiddly.Int;
        $!adverb = $!adverb.Int;
        $!ternary = $!ternary.Int;
    }
    multi method raku(OperatorProperties:U: --> Str) { 'OperatorProperties' }
    multi method raku(OperatorProperties:D: --> Str) {
        my @parts;
        @parts.push(":precedence({$!precedence.raku})") if $!precedence;
        @parts.push(":sub-precedence({$!sub-precedence.raku})") if $!sub-precedence;
        @parts.push(":associative({$!associative.raku})") if $!associative;
        @parts.push(":thunky({$!thunky.raku})") if $!thunky;
        @parts.push(":dba({$!dba.raku})") if $!dba;
        @parts.push(":next-term({$!next-term.raku})") if $!next-term;
        @parts.push(":iffy") if $!iffy;
        @parts.push(":diffy") if $!diffy;
        @parts.push(":fiddly") if $!fiddly;
        @parts.push(":adverb") if $!adverb;
        @parts.push(":ternary") if $!ternary;
        'OperatorProperties.new: ' ~ @parts.join(", ")
    }
    multi method gist(OperatorProperties:D: --> Str) { self.raku }
}
"#;

impl Interpreter {
    /// Prepend [`OPERATOR_PROPERTIES_PRELUDE`] to a unit that names
    /// `OperatorProperties` or asks an operator for its `properties`.
    // Cost: O(n), n = source bytes (one substring search when absent).
    pub(super) fn inject_operator_properties_prelude(
        source: &CodeText<'_>,
        stmts: &mut Vec<Stmt>,
    ) {
        if !(source.contains("OperatorProperties") || source.contains(".properties"))
            || source.contains("class OperatorProperties")
        {
            return;
        }
        use std::sync::OnceLock;
        static STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(OPERATOR_PROPERTIES_PRELUDE)
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
