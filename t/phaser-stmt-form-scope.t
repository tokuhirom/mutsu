use v6;
use Test;

# Statement-form loop phasers share the enclosing block's lexical scope:
# a `state` declared by a statement-form NEXT is visible to a LAST in the
# same block (roast/integration/advent2012-day15.t FIRST/NEXT/LAST example).
# INIT inside a sub body runs at program init time, before the mainline.

plan 4;

# 1. NEXT-declared state readable from LAST (the advent2012-day15 shape).
{
    my $out = '';
    for (3, 2, 42) -> $score {
        FIRST $out ~= "first;";
        NEXT (state $best) max= $score;
        LAST $out ~= "best=$best";
    }
    is $out, 'first;best=42', 'statement-form NEXT state is visible to LAST';
}

# 2. A say before the state declaration must not wipe the variable at loop
#    exit (the premature env-seed regression behind the same test).
{
    my $got;
    for (3, 2, 42) {
        if $_ > 2 { say "# progress $_"; }
        (state $b) max= $_;
        LAST $got = $b;
    }
    is $got, 42, 'say before a state decl does not wipe it at loop exit';
}

# 3. Block-form phaser bodies keep their own scope for my-declarations.
{
    my $x = 'outer';
    for 1..2 {
        NEXT { my $x = 'inner'; }
    }
    is $x, 'outer', 'block-form NEXT my-decl stays in its own scope';
}

# 4. INIT expression inside a sub runs before the mainline.
our $mainline-ran;
$mainline-ran = True;
sub init-probe() {
    my $snap = INIT $mainline-ran.defined;
    return $snap;
}
is init-probe(), False, 'INIT expr in a sub body runs before the mainline';
