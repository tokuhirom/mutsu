//! The Raku half of `nqp::getcomp("Raku")` (ADR-0122): the compiler object's
//! class and rakudo's core `REPL` class, registered on the first
//! `nqp::getcomp` call (`Interpreter::ensure_repl_compiler_prelude`).
//!
//! Both are ordinary Raku classes on purpose. Every method a caller can see is
//! a real method with a real signature, so `.^methods`, `.can` and overriding
//! work, and the only Rust behind them is the one primitive
//! `__mutsu_compiler_eval` (`runtime::repl_compiler`), which is where the
//! persistent-context work happens.
//!
//! `REPL` is adapted from rakudo's `src/core.c/REPL.rakumod` (Artistic-2.0),
//! keeping the surface ecosystem modules call — `new`, `init`, `repl-eval`,
//! `ctxsave`, `input-incomplete`, `input-toplevel-control`, `compiler` — and
//! leaving out the interactive loop and the line-editor mixins
//! (`Readline`/`Linenoise`/`Terminal::LineEditor`), which need a terminal and
//! are the job of mutsu's own CLI REPL (`src/repl.rs`).

/// Registered once per interpreter, in `GLOBAL`.
pub(super) const REPL_COMPILER_PRELUDE: &str = r#"
class GLOBAL::Perl6::Compiler {
    # `$code` is compiled as its own compilation unit. With `:outer_ctx` it
    # sees the lexicals, subs and operators a previous `.eval` left in that
    # context (one `$*CTXSAVE.ctxsave` handed back); without it, only the
    # setting. `:interactive` and every other adverb are accepted and ignored.
    method eval(Mu $code, Mu :$outer_ctx, *%adverbs) {
        my Mu $saver := $*CTXSAVE // Mu;
        __mutsu_compiler_eval(~$code, $outer_ctx, $saver)
    }
    method version_string(:$shorten-versions, :$no-unicode) {
        my $raku = $no-unicode ?? 'Raku(R)' !! "Raku\x[AE]";
        "Welcome to {$*RAKU.compiler.name} {$*RAKU.compiler.version}.\n"
          ~ "Implementing the $raku Programming Language {$*RAKU.version}."
    }
    method repl-mode() { $*IN.t ?? 'interactive' !! 'process' }
    method language_name() { 'Raku' }
    method language_version() { $*RAKU.version.Str.substr(1) }
    method implementation() { $*RAKU.compiler.name }
    method compiler_progname() { $*RAKU.compiler.name }
}

class GLOBAL::REPL {
    has Mu $.compiler;
    has Bool $!multi-line-enabled;
    has $!save_ctx;

    # Unique internal values for out-of-band eval results
    has $!need-more-input = {};
    has $!control-not-allowed = {};

    method new(Mu \compiler, Mu \adverbs, $skip = %*ENV<RAKUDO_NO_VERSION>) {
        unless $skip {
            say compiler.version_string(
              :shorten-versions,
              :no-unicode($*DISTRO.is-win)
            );
            say '';
        }
        my $self = self.bless();
        $self.init(compiler, !%*ENV<RAKUDO_DISABLE_MULTILINE>);
        $self
    }

    method init(Mu \compiler, $multi-line-enabled --> Nil) {
        if compiler.repl-mode eq 'tty' && not $*IN.t {
            die "Invalid REPL environment: Unable to initialize REPL outside of a TTY";
        }
        $!compiler := compiler;
        $!multi-line-enabled = ?$multi-line-enabled;
    }

    method teardown() { }

    method repl-eval($code, \exception, *%adverbs) {
        CATCH {
            when X::Syntax::Missing {
                return $!need-more-input
                  if $!multi-line-enabled && .pos == $code.chars;
                .throw;
            }
            when X::Comp::FailGoal {
                return $!need-more-input
                  if $!multi-line-enabled && .pos == $code.chars;
                .throw;
            }
            when X::ControlFlow::Return {
                return $!control-not-allowed;
            }
            default {
                exception = $_;
                return;
            }
        }
        CONTROL {
            when CX::Emit | CX::Take { .rethrow; }
            when CX::Warn { .gist.say; .resume; }
            return $!control-not-allowed;
        }
        self.compiler.eval($code, |%adverbs)
    }

    method ctxsave(--> Nil) {
        $*MAIN_CTX := nqp::ctxcaller(nqp::ctx());
        $*CTXSAVE := 0;
    }

    method input-incomplete(Mu $value --> Bool:D) {
        $value === $!need-more-input
    }

    method input-toplevel-control(Mu $value --> Bool:D) {
        $value === $!control-not-allowed
    }
}
"#;
