use v6;
use MONKEY-SEE-NO-EVAL;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test;
use Test::Util;

# ADR-0037 Slice 5: residue coverage the ADR names by name --
# `$*THROWS-LIKE-CONTEXT`, `CALLERS::`, and `EVALFILE` -- alongside the
# targeting itself (Slice 4, pinned in t/eval-context-live-target.t) and the
# acceptance gate (t/throws-like-gather-sink.t, t/emit-done-controlflow.t
# under MUTSU_REAL_TEST=1).

plan 4;

# `CALLERS::` (plural) is stamped identically to `CALLER::` at the same site
# (vm_var_assign_local.rs's CALLER/CALLERS arm), so a live two-deep
# `context => CALLERS::` targets the same frame `CALLER::` would.
{
    my @log;
    sub callers-thrower($code) {
        my $ctx = CALLERS::;
        my $x = EVAL $code, context => $ctx;
        @log.push("thrower saw x=$x");
        return 'thrower-end';
    }
    sub callers-caller() {
        my $x = callers-thrower('return 1');
        @log.push("got: $x");
        return 'caller-end';
    }
    is callers-caller(), 1,
        'context => CALLERS:: targets the live two-deep frame the same way CALLER:: does';
}

# `Test::Util`'s `no-fatal-throws-like` stores its captured `CALLER::` in the
# dynamic variable `$*THROWS-LIKE-CONTEXT`, which the real `Test.rakumod`'s
# `throws-like` reads back (`$*THROWS-LIKE-CONTEXT // CALLER::`) several
# frames deeper, inside its own `subtest { ... }` -- the exact
# capture-now/use-later shape ADR-0037 §2.3's alternative (c) named as the
# reason a stamped-attribute `Stash` value (not a live frame reference) is
# needed. Confirms the stamp survives being threaded through a dynamic
# variable, not just a lexical.
no-fatal-throws-like 'return 1', X::ControlFlow::Return,
    '$*THROWS-LIKE-CONTEXT (captured by no-fatal-throws-like, read back inside throws-like) '
    ~ 'classifies a return the same way a directly-passed context does';

# EVALFILE has no `context` parameter (raku-doc/doc/Type/independent-routines.rakudoc),
# so a `return` in the EVALFILE'd file keeps its plain, uncontextualized
# classification -- unwinding to whichever routine dynamically encloses the
# EVALFILE() call. Unaffected by the classification machinery this ADR added
# (`pending_eval_context_routine` stays `None` for it), but worth pinning so a
# future EVALFILE change cannot silently break it.
sub evalfile-caller() {
    my $x = EVALFILE($*PROGRAM.parent.child('lib/evalfile-return-fixture.raku').Str);
    return "got:$x";
}
# The EVALFILE'd `return 5` is an ordinary (untargeted) non-local return: it
# unwinds straight out of `evalfile-caller` itself (the one enclosing
# routine), so `$x` is never assigned and "got:$x" never runs -- same as
# `sub f() { EVAL 'return 1'; return 2 }` answering `1`, not `2`.
is evalfile-caller(), 5,
    'EVALFILE (no context argument) keeps the plain uncontextualized return semantics';

# And EVALFILE run with no enclosing routine at all still throws
# X::ControlFlow::Return, matching a bare EVAL 'return' at file scope.
{
    my $bare = try EVALFILE($*PROGRAM.parent.child('lib/evalfile-return-fixture.raku').Str);
    is $!.^name, 'X::ControlFlow::Return',
        'EVALFILE with no enclosing routine throws X::ControlFlow::Return, same as EVAL';
}
