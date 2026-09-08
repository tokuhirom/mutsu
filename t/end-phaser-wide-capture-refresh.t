use v6;
use Test;

# The post-closure-call END-phaser refresh (`update_end_phaser_envs_for_keys`)
# walks the calling closure's captured env and copies the live value of every
# name that some phaser also captured. Two things decide how much env it walks,
# and neither is under the closure's control:
#
#   * how many names the *creating* scope holds -- a `use` of a module with a
#     wide export list widens every capture made in the importing scope; and
#   * whether the process has latched `reflective_name_access_possible()`, which
#     makes `capture_closure_env` snapshot the WHOLE visible env instead of the
#     closure's own free variables. One `EVAL` anywhere latches it for good.
#
# That refresh used to resolve every one of those names back to a `String` and
# re-intern it three times over (`dead_keys`, the phaser env, the live env),
# which made it ~30% of the hot loop of a program whose only sin was `use Test`
# at the top (#7565). It now works in interned-`Symbol` space and asks first
# whether any phaser watches any of the names at all, so a capture that shares
# no name with any phaser skips the whole-env flatten too.
#
# This file pins the SEMANTICS that rewrite has to preserve under exactly the
# conditions that widen the capture: the refresh must still happen for a name a
# phaser really captured, and must still not happen for anything else.

plan 8;

my $dir = $*TMPDIR.child("mutsu-end-wide-{$*PID}");
$dir.mkdir;
END { try { .unlink for $dir.dir; $dir.rmdir } }

sub run-snippet($name, $source) {
    my $file = $dir.child($name);
    $file.spurt($source);
    my $proc = run($*EXECUTABLE, $file.absolute, :out, :err);
    my $out = $proc.out.slurp(:close);
    $proc.err.slurp(:close);
    $out.trim
}

# A prelude that forces both widening conditions at once: sixty extra mainline
# names for the capture to carry, and an `EVAL` to latch the whole-env snapshot.
my $wide = "use MONKEY-SEE-NO-EVAL;\nmy \$latch = EVAL '1';\n"
    ~ (1..60).map({ "our \$pad$_ = $_;\n" }).join;

# 1. The propagation the refresh exists for still happens when the capture has
#    been widened to a whole-env snapshot.
is run-snippet('wide-same.raku', $wide ~ 'my $x = 1;
END { say "x=", $x }
my $c = { $x = 2 };
$c();
'), 'x=2',
    'a closure mutating the binding an END captured propagates under a wide capture';

# 2. Same, called through another sub rather than invoked directly -- the shape
#    that reaches the closure-call refresh rather than a direct frame return.
is run-snippet('wide-called.raku', $wide ~ 'sub callit(&c) { c() }
my $x = 1;
END { say "x=", $x }
callit { $x = 9 };
'), 'x=9',
    'the same propagation through an intermediate sub call';

# 3. The dead-scope binding still wins: a widened capture drags in every
#    mainline name, so the `dead_keys` guard is doing more work than ever.
is run-snippet('wide-dead.raku', $wide ~ 'sub callit(&c) { c() }
{ my $a = 42; END { say "a=", $a }; }
my $a = 0;
callit { $a };
'), 'a=42',
    'a dead-scope END binding survives a same-named widened capture';

# 4. Two dead-scope phasers sharing a name each keep their own value.
is run-snippet('wide-two-dead.raku', $wide ~ 'sub callit(&c) { c() }
{ my $a = 1; END { say "first a=$a" }; }
{ my $a = 2; END { say "second a=$a" }; }
my $a = 0;
callit { $a };
').lines.sort.join('|'), 'first a=1|second a=2',
    'two same-named dead-scope phasers keep their own bindings under a wide capture';

# 5. The widened names themselves are not phaser business: a phaser that
#    captured none of them must be unaffected by a closure call that carries
#    all sixty.
is run-snippet('wide-untouched.raku', $wide ~ 'my $only = 5;
END { say "only=", $only }
my $c = { $pad7 };
$c();
'), 'only=5',
    'a closure carrying sixty unrelated names leaves an unrelated phaser alone';

# 6. A mainline `our` the phaser DID capture must still refresh -- the widened
#    names are ordinary lexicals, not exempt from the propagation.
is run-snippet('wide-our.raku', $wide ~ 'END { say "pad3=", $pad3 }
my $c = { $pad3 = 99 };
$c();
'), 'pad3=99',
    'a widened name an END captured still refreshes when a closure mutates it';

# 7. No phaser at all: the guard must not change what a closure call does.
is run-snippet('wide-nophaser.raku', $wide ~ 'my $x = 1;
my $c = { $x = 3 };
$c();
say "x=", $x;
'), 'x=3',
    'a closure call with no END phaser registered behaves unchanged';

# 8. The latch alone (no extra names) still refreshes -- the whole-env snapshot
#    path is the one that carries the phaser name in a parent tier.
is run-snippet('latch-only.raku', "use MONKEY-SEE-NO-EVAL;\nmy \$latch = EVAL '1';\n" ~ 'my $x = 1;
END { say "x=", $x }
sub callit(&c) { c() }
callit { $x = 4 };
'), 'x=4',
    'the reflective whole-env capture path still refreshes the phaser';
