# (B)-gate: dynamic-substitution and deferred/compile-time phaser folds.
#
# Under MUTSU_GATE_LOCAL_ENV_WRITE a plain-lexical store skips its env mirror.
# Two roast ON-survey regressions relied on the mirror:
#   * roast S04-declarations/state.t: a `state` var bumped inside a `s///`
#     replacement block updates the var BY NAME through env (the replacement is
#     evaluated re-entrantly), so a static-pattern substitution whose dynamic
#     replacement references the var must keep the frame env-synced, and the
#     closure's state save-back must read env-first for that (env-synced) var.
#   * roast S04-phasers/interpolate.t: a top-level `my` var mutated only through
#     END/BEGIN/CHECK phaser bodies (run deferred / at compile time, reconstructed
#     from env by name) must keep the declaring frame env-synced.
#
# Both fixes are gate-ON-only folds; results are identical with the gate on or off.
use Test;

plan 4;

# state var referenced from an s/// replacement part (state.t #16 shape)
{
    my $str = "abc";
    my $re = { state $a = 0; $str ~~ s/^(.)/{ $a++ }/; };
    $re(); $re(); $re();
    is $str, "2bc", 'state var bumped inside s/// replacement accumulates';
}

# plain state accumulation is unaffected (the #4869 case the fold must not break)
{
    my $c = { state $s = 0; $s = $s + 10; $s };
    is "{$c()},{$c()},{$c()}", "10,20,30", 'plain state accumulation still works';
}

# top-level my mutated through interpolated compile-time + deferred phasers
# (interpolate.t shape: BEGIN/CHECK/INIT accumulate, END reads the total)
my $hist;
"{ BEGIN { $hist ~= 'B' } }";
"{ CHECK { $hist ~= 'C' } }";
"{ INIT  { $hist ~= 'I' } }";
is $hist, 'BCI', 'interpolated BEGIN/CHECK/INIT accumulate into a top-level my';

# a dynamic replacement that interpolates (not a code block) also folds
{
    my $s = "xyz";
    my $n = 7;
    $s ~~ s/^./$n/;
    is $s, "7yz", 's/// with an interpolated replacement sees the live lexical';
}
