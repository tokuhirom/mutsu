//! CORE's `X::Comp` role as real Raku source: the compile-time position
//! (`filename`, `line`, `pre`, `post`, ...) a `does X::Comp` exception carries,
//! `SET_FILE_LINE` / `SET_PRE_POST` which set it, and the `===SORRY!===`
//! rendering of `.gist` (#15666; maintainer-approved on the issue).

use super::source_code_text::CodeText;
use super::*;

/// rakudo's `role X::Comp is Exception` (`src/core.c/Exception.rakumod`),
/// measured against rakudo 2026.09. The native, empty marker role of the same
/// name stays registered for programs that never mention it; this definition
/// replaces it where the source does.
const X_COMP_ROLE_PRELUDE: &str = r#"
role GLOBAL::X::Comp is Exception {
    has $.filename;
    has $.line;
    has $.column;
    has @.modules;
    has $.is-compile-time = False;
    has $.pre;
    has $.post;
    has @.highexpect;
    has $.pos;
    has $.directive-filename;

    multi method gist(::?CLASS:D: :$sorry = True, :$expect = True) {
        unless $!is-compile-time {
            my $bt = self.backtrace;
            return $bt.defined ?? "{self.message}\n$bt" !! self.message;
        }
        my $r = $sorry ?? self.sorry_heading() !! "";
        $r ~= self.message ~ "\nat {$!filename}:{$!line}";
        $r ~= "\n------> {$!pre}<HERE>{$!post}" if $!pre.defined;
        if $expect && @!highexpect {
            $r ~= "\n    expecting any of:";
            $r ~= "\n        $_" for @!highexpect;
        }
        for @!modules.reverse[1..*] {
            $r ~= "\n  from module {$_<module>} ({$_<filename>} line {$_<line>})";
        }
        $r
    }

    method sorry_heading() {
        "===SORRY!=== Error while compiling" ~ ($!filename.defined ?? " $!filename" !! ":") ~ "\n"
    }

    method SET_FILE_LINE($file, $line) {
        $!filename = $file;
        $!line = $line;
        $!is-compile-time = True;
    }

    method SET_PRE_POST($pre, $post) {
        $!pre = $pre;
        $!post = $post;
    }

    method at-line(*@lines, :$filename is copy) {
        $filename //= $!filename;
        return "at $filename:" ~ @lines.join(",") if $filename.defined;
        # No file name: rakudo words it by the line count, and says nothing for line 1.
        @lines == 1 ?? (@lines[0] eq '1' ?? "" !! "at line @lines[0]") !! "at lines " ~ @lines.join(", ")
    }
}
"#;

impl Interpreter {
    /// Prepend [`X_COMP_ROLE_PRELUDE`] to a compunit that names `X::Comp` (or
    /// one of its subtypes) without declaring its own `role X::Comp`.
    /// Injected for modules too: `static` composes it from `static.rakumod`,
    /// which the script that loads it never names.
    // Cost: O(n) substring scan of the source, n = its length; the prelude is parsed once.
    pub(super) fn inject_x_comp_prelude(source: &CodeText<'_>, stmts: &mut Vec<Stmt>) {
        if !source.contains("X::Comp") || source.contains("role X::Comp") {
            return;
        }
        use std::sync::OnceLock;
        static X_COMP_STMTS: OnceLock<Vec<Stmt>> = OnceLock::new();
        let prelude = X_COMP_STMTS.get_or_init(|| {
            crate::runtime::prelude_source::parse_prelude_source(X_COMP_ROLE_PRELUDE)
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
