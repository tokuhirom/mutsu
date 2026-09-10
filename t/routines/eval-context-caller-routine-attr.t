use v6;
use Test;

plan 6;

# ADR-0037 Slice 2: `CALLER::` now stamps a second hidden attribute
# (`__mutsu_origin_routine`, alongside the existing `__mutsu_origin_package`)
# recording the control-flow identity of the routine that dynamically
# encloses the captured frame. Same invisibility convention as the existing
# package stamp: it is an *attribute* on the pseudo-stash Instance, not a
# `symbols` member, so `.keys`/`.gist`/`.raku` must never expose it.

sub inner() {
    my $c = CALLER::;
    $c;
}
sub outer() {
    inner();
}
my $ctx = outer();

is $ctx.keys.sort.join(','), '',
    'CALLER:: pseudo-stash exposes no keys (both hidden origin attrs stay invisible)';
unlike $ctx.gist, /mutsu_origin/,
    '.gist does not mention the hidden origin-routine attribute';
unlike $ctx.raku, /mutsu_origin/,
    '.raku does not mention the hidden origin-routine attribute';

# A mainline-captured CALLER:: (no enclosing routine) must also stay
# key-less/gist-clean -- it simply stamps no origin-routine attribute at all.
sub thrower() {
    my $c = CALLER::; # names the mainline: no enclosing routine
    $c;
}
my $mainline-ctx = thrower();
is $mainline-ctx.keys.sort.join(','), '',
    'a mainline-captured CALLER:: also exposes no keys';
unlike $mainline-ctx.gist, /mutsu_origin/,
    'a mainline-captured CALLER:: .gist stays clean too';

# The existing package-context mechanism (t/eval-context-package.t) must be
# completely unaffected by adding the parallel routine-identity stamp.
sub run-in-context($code) {
    my $ctx = CALLER::;
    EVAL $code, context => $ctx;
}
sub caller-of-run() {
    run-in-context('my class StampCheck { }; StampCheck.^name');
}
is caller-of-run(), 'StampCheck',
    'context => CALLER:: package classification is unaffected by the routine stamp';
