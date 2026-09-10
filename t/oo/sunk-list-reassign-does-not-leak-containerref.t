use v6;
use Test;
use lib $*PROGRAM.parent(2).add("roast/packages/Test-Helpers/lib");
use Test::Util;

plan 1;

# Found investigating `todo/deep/vendor-real-test-module.md`'s `t/` residue,
# continuing from the (fixed) `control-warn-resume-caller-var-name-collision`
# bug (`t/control-warn-resume-caller-var-name-collision.t`). That fix resolves
# the 2-call repro; this file pins the 3-call one, root-caused and fixed in
# `todo/deep/sunk-list-reassign-leaks-containerref-into-shared-env.md`.
#
# A bare (sunk) statement-level list re-assignment (`($x, $y, $z) = f(...);`,
# as opposed to a `my (...) = ...;` declaration) compiled its own discarded
# rvalue as an aliased list: each target was boxed into a shared
# `ContainerRef` cell and written into the flat, cross-frame `env` store, even
# though the value was immediately popped (`SinkPop`). That stale cell then
# got picked up by the NEXT unrelated closure literal created in the same
# (flat) scope once `reflective_name_access_possible()` made closures capture
# their entire creation-time env rather than just their real free variables —
# the closure's own entry merge then force-overwrote the callee's freshly
# declared locals with the stale cell, corrupting them. It reproduced
# starting from the THIRD call sharing the same variable names (the first
# call is a declaration with no trailing alias-list; the second call's
# reassignment is what leaves the stale cell; the third call's closure
# literal is created while that cell is still live, and is the first to pick
# it up).
#
# Reproducing this needs the real vendored `Test.rakumod` (`MUTSU_REAL_TEST=1`)
# loaded, same as the 2-call sibling pin.
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
    my ($x, $y, $z) = f({ warn "boom1" });
    say "1: x=$x y=$y z=$z";
    ($x, $y, $z) = f({ warn "boom2" });
    say "2: x=$x y=$y z=$z";
    ($x, $y, $z) = f({ warn "boom3" });
    say "3: x=$x y=$y z=$z";
    RAKU

is_run $code,
    {
        status => 0,
        out    => "1: x=True y=boom1 z=True\n2: x=True y=boom2 z=True\n3: x=True y=boom3 z=True\n",
        err    => '',
    },
    'the 3rd (and later) CONTROL-handler write is not lost to a stale sunk-list-assign ContainerRef';
