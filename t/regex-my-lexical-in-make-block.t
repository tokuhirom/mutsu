use Test;

plan 7;

# A regex's own `:my` lexical must be visible to a `make`-bearing code block.
# Such a block does not run inline during matching — it is replayed on the
# reduce walk, after the match state that held the lexical is gone — so its
# value has to travel with the block.

my $m1 = "a" ~~ / :my $x = 9; 'a' { make (0, $x) } /;
is-deeply $m1.ast, (0, 9), "a make block sees a :my initializer";

my $m2 = "a" ~~ / :my $y; 'a' { $y = 42 } { make (1, $y) } /;
is-deeply $m2.ast, (1, 42), "a make block sees what an earlier block assigned";

my $m3 = "a" ~~ / :my $z = 1; 'a' { $z = $z + 1 } { make $z } /;
is $m3.ast, 2, "the assignment is applied on top of the initializer";

# Two make blocks in sequence: the second sees the first's write.
my $m4 = "a" ~~ / :my $w = 1; 'a' { make $w } { $w = 5; make $w } /;
is $m4.ast, 5, "a later make block sees an earlier make block's write";

# Inside an alternation group, and with a non-trivial value.
my $m5 = "b" ~~ / :my $c; [ 'b' { $c = Capture.new(:list(1, 2)) } { make (7, $c) } | 'z' ] /;
is $m5.ast[0], 7, "the group's make block reduces with the right index";
is-deeply $m5.ast[1].list, (1, 2), "and carries the Capture the block built";

# (Whether a `:my` of the same name as an enclosing lexical writes through to it
# is deliberately not asserted here — Rakudo does write through, and that edge is
# not what this file pins.)

# Inline blocks keep working alongside the deferred one.
my @seen;
my $m6 = "a" ~~ / :my $v = 3; 'a' { @seen.push($v) } { make $v } /;
is-deeply (@seen, $m6.ast), ([3], 3), "inline and make blocks agree on the value";
