use super::*;
use crate::ast::Stmt;
use crate::runtime::source_code_text::CodeText;

/// Rakudo's own `RakuAST::Deparse` (ADR-14845), vendored verbatim from the
/// release mutsu's RakuAST tracks (`vendor.lock`, row `rakudo`).
const DEPARSE_CLASS: &str = include_str!("../../vendor/rakudo/src/core.c/RakuAST/Deparse.rakumod");

/// Where the class declares itself in the vendored file. The prelude lifts the
/// declaration into `GLOBAL::` (whatever unit it is injected into), the one
/// textual change; the class body is untouched.
const DEPARSE_CLASS_HEAD: &str = "class RakuAST::Deparse {";

/// `RakuAST::Node.DEPARSE(*@roles)` is this routine (`rakuast_node.rs`):
/// rakudo's `src/Raku/ast/base.rakumod` method, with the role handling it
/// does -- a `Str` names a language (`$core.slang`), a role is mixed in, a
/// class replaces the default deparser.
const DEPARSE_ENTRY: &str = r#"
module MUTSU_DEPARSE {
our sub run(Mu \node, *@roles) {
    my $core := GLOBAL::RakuAST::Deparse;
    my $class := $core;
    for @roles {
        if $_ ~~ Str {
            $class := $class.^mixin($core.slang($_));
        }
        elsif $_.HOW.can('pun') {
            $class := $class.^mixin($_);
        }
        else {
            $class := $_;
        }
    }
    $class.deparse(node)
}
}
"#;

/// The routine the deparser's fallback candidate uses to report an unhandled
/// node (rakudo's core `NYI`), visible to the vendored class only.
const DEPARSE_NYI: &str = r#"
    my sub NYI(Str:D $feature) { X::NYI.new(:$feature) }
"#;

static INJECTED: std::sync::atomic::AtomicBool = std::sync::atomic::AtomicBool::new(false);

impl Interpreter {
    /// Prepend the deparser to a program (or module) that mentions `DEPARSE`
    /// or `RakuAST::Deparse`; the class is global, so the first unit to ask
    /// declares it and every later one finds it. A program that never deparses
    /// pays one substring search.
    // Cost: O(n), n = source bytes (one substring search when absent); O(d)
    // to parse the deparser the first time it is wanted, d = its source size.
    pub(super) fn inject_deparse_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !(source.contains("DEPARSE") || source.contains("RakuAST::Deparse"))
            || source.contains(DEPARSE_CLASS_HEAD)
            || INJECTED.load(std::sync::atomic::Ordering::Acquire)
        {
            return;
        }
        let Some(class) = DEPARSE_CLASS
            .contains(DEPARSE_CLASS_HEAD)
            .then(|| DEPARSE_CLASS.replacen(DEPARSE_CLASS_HEAD, "class GLOBAL::RakuAST::Deparse {", 1))
        else {
            return;
        };
        let program = format!("{{\n{DEPARSE_NYI}\n{class}\n}}\n{DEPARSE_ENTRY}");
        let Ok((prelude, _)) = crate::runtime::prelude_source::parse_prelude_source(&program)
        else {
            return;
        };
        INJECTED.store(true, std::sync::atomic::Ordering::Release);
        let mut combined = prelude;
        combined.append(stmts);
        *stmts = combined;
    }
}
