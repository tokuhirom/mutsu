# Controls for pool-recursive-start.raku: these all PASS on mutsu today, and
# they are what rules out "shared_vars is a naive name collision".
# Sequential same-name frames are fine; only same-name frames that are
# SIMULTANEOUSLY LIVE across a thread boundary corrupt.
# raku and mutsu agree on every line below.

# Plain recursion, no `start`.
sub g($n) { $n <= 0 ?? "b" !! g($n - 1) ~ "|$n" }
say "plain 1st: ", g(3);        # b|1|2|3
say "plain 2nd: ", g(3);        # b|1|2|3

# Non-recursive `start`, called repeatedly with different arguments.
sub h($n) { start { "v$n" } }
say "h(1): ", await h(1);       # v1
say "h(2): ", await h(2);       # v2

# Two unrelated subs that both name their parameter `$x`, interleaved.
sub a($x) { start { "a=$x" } }
sub b($x) { start { "b=$x" } }
say await a(1);                 # a=1
say await b(2);                 # b=2
say await a(3);                 # a=3

# A plain `my` lexical of a common name, captured by `start`, called 3x.
sub p($v) { my $tmp = $v * 10; start { "p=$tmp" } }
say await p(1);                 # p=10
say await p(2);                 # p=20
say await p(3);                 # p=30
