use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 1;

# Found investigating `todo/deep/vendor-real-test-module.md`'s `t/` residue
# (`t/warn-resumes-at-the-raise-site.t`), root-caused in
# `todo/deep/control-warn-resume-list-assign-first-target-stale-on-repeat-call.md`.
#
# A `CONTROL { when CX::Warn { ...; .resume } }` sub's list-returned locals go
# stale on the SECOND call when the CALLER reuses the same variable names for
# the list-assigned result: `Interpreter::call_compiled_closure_with_topic`'s
# closure-return env writeback treats a caller lexical whose live value
# happens to equal the closure's OWN capture-time snapshot as an unmutated
# pass-through capture and skips writing it back — a false positive when that
# equality holds because an ANCESTOR frame's CONTROL handler wrote it
# in-band (`try_resume_safe_control_inline`) rather than because nothing
# touched it. On the 2nd call the caller variable already holds the 1st
# call's result, so the 2nd closure's blanket env-capture snapshot picks up
# that same value, and the coincidence trips the skip.
#
# Reproducing this needs the real vendored `Test.rakumod` (`MUTSU_REAL_TEST=1`)
# loaded — a large synthetic module with many declared-but-uncalled subs does
# NOT reproduce it, so the trigger is spawned as a subprocess with the real
# switch on rather than gated at the file level (this file itself always
# runs).
%*ENV<MUTSU_REAL_TEST> = '1';

my $code = q:to/RAKU/;
    use Test;
    sub f(&code) {
        my ($x, $y, $z) = False, '', False;
        code();
        $z = True;
        CONTROL { when CX::Warn { $x = True; $y = .message; .resume } }
        ($x, $y, $z);
    }
    my ($x, $y, $z) = f({ warn "boom" });
    say "first: x=$x y=$y z=$z";
    ($x, $y, $z) = f({ warn "boom2" });
    say "second: x=$x y=$y z=$z";
    RAKU

is_run $code,
    {
        status => 0,
        out    => "first: x=True y=boom z=True\nsecond: x=True y=boom2 z=True\n",
        err    => '',
    },
    'the 2nd call CONTROL-handler write survives the closure return, not just the 1st';
