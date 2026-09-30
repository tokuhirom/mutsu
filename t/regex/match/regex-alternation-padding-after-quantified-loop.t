use Test;

# A `||` inside a quantified group does not pad its branches' positional
# slots, but that must stop at the loop: an alternation *after* the loop is
# not quantified, so its short branch still reserves a slot and a following
# capture keeps its number. The tree walk used to leave the "quantified
# alternation" flag set while it ran the rest of the pattern, so `(x)` below
# became `$0`. Expected values read off rakudo 2026.07 (which shows the
# reserved slot as `Mu`; mutsu shows it as `Nil`, hence the `.defined` checks).

plan 12;

for (
    / [ a || b ]+ [ (c) || d ] (x) /,
    / [ a || b ]+? [ (c) || d ] (x) /,
    / [ a || b ] ** 1..3 [ (c) || d ] (x) /,
    / [ a || b ]* [ (c) || d ] (x) /,
).kv -> $i, $rx {
    my $m = "adx" ~~ $rx;
    is $m.list.elems, 2, "pattern $i: the short branch reserves a slot";
    ok !$m[0].defined, "pattern $i: the reserved slot is undefined";
    is ~$m[1], 'x', "pattern $i: the capture after the alternation is \$1";
}
