use v6;
use Test;

# `gather <statement>` / `start <statement>` parse their operand with the
# statement parser, which consumes the terminating `;`. The expression must
# give the terminator back — otherwise the rest of the line was misread as
# an infix on the gather (`my @a = gather do {…}; say "x"` parsed `say` as
# an infix function and the take'd values leaked to stdout).
# Found by the doc-diff sweep (Type/Seq.rakudoc [1]).

plan 6;

my @a = gather do { take "one" }; my $after = "ran";
is-deeply @a, ["one"], 'gather do {...} collects its take';
is $after, "ran", 'the statement after the ; runs as its own statement';

my @b = lazy gather do { take 1; take 2 }; my @got;
@got.push($_) for @b;
is-deeply @got, [1, 2], 'lazy gather do iterates via for';

my @c = gather for ^3 { take $_ * 2 }; my $tail = "ok";
is-deeply @c, [0, 2, 4], 'gather for still works';
is $tail, "ok", 'trailing statement intact after gather for';

my $p = start do { 42 }; my $r = await $p;
is $r, 42, 'start do {...} keeps the statement after the ;';

done-testing;
