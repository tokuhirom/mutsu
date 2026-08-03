use lib 'roast/packages/Test-Helpers/lib';
use Test;
use Test::Util;

# A Pair unpacks as `\(:key(...), :value(...))` — two named parts, no positional
# one — so a candidate written `Pair (:key($k), :value($v))` destructures it.
# Dispatch matching disagreed with the binder on two points and never selected
# such a candidate:
#
#   * the leftover-positional check fired even though an all-named
#     sub-signature consumes no positionals;
#   * a named param's RENAME parens (`:key($plan)`) were recursed into as if
#     they were a destructure, so the candidate had to yield a positional
#     element the value did not have.
#
# `Test::Util`'s `group-of` is exactly this shape, so it lost to mutsu's native
# `group-of` provider even with the real module loaded, and the two kept
# separate test counters.

plan 8;

multi one(Pair (:key($k), :value($v))) { "pair:$k/$v" }
multi one($other)                      { "other" }

is one(2 => 'x'), 'pair:2/x', 'a named Pair destructure is selected';
is one('a' => 'x'), 'pair:a/x', 'a string-keyed Pair too';
is one('plain'), 'other', 'a non-Pair still falls through';

multi nested(Pair (:key($plan), :value($rest))) { "plan=$plan rest={$rest.^name}" }
multi nested(*@a)                               { 'fallback' }

is nested(2 => (3 => 4)), 'plan=2 rest=Pair',
    'a rename target whose value is itself a Pair matches';
is nested(2 => [3, 4]), 'plan=2 rest=Array',
    'a rename target whose value is an Array matches';

# Positional destructuring of a Pair does NOT match (a Pair has no positional
# part), and neither does a sub-signature that leaves one of the two named
# parts unconsumed. Both agree with rakudo.
multi narrow(Pair ($k, $v)) { 'positional' }
multi narrow($other)        { 'other' }
is narrow(2 => 'x'), 'other', 'a Pair has no positional part to destructure';

multi partial(Pair (:key($k))) { 'partial' }
multi partial($other)          { 'other' }
is partial(2 => 'x'), 'other', 'leaving .value unconsumed does not match';

# The end-to-end shape: Test::Util's own group-of must run, not the native one.
group-of 2 => 'the real group-of runs' => {
    ok 1, 'inner a';
    ok 1, 'inner b';
}
