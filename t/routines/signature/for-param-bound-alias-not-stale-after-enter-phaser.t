use Test;

plan 4;

# `my $x := $b` inside a `for` body leaves the alias metadata of `$x` behind.
# The ENTER phaser's block scope then re-synced the previous iteration's `$x`
# into `$b` when it exited, so `$b` read as its first value on every pass.

sub with-enter { my @a; for [1, 2, 3] -> $b { my $x := $b; ENTER { 1 }; @a.push: $b }; @a }
is-deeply with-enter, [1, 2, 3], 'a bound alias of the loop param does not freeze it under ENTER';

sub with-enter-first { my @a; for [1, 2, 3] -> $b { ENTER { 1 }; my $x := $b; @a.push: $b }; @a }
is-deeply with-enter-first, [1, 2, 3], 'also with the bind after the phaser';

sub reads-both { my @a; for 1, 2, 3 -> $b { my $x := $b; ENTER { 1 }; @a.push: "$b$x" }; @a }
is-deeply reads-both, ['11', '22', '33'], 'the alias and the param agree on every pass';

sub gather-take { gather for [1, 2, 3] -> $b { my $x := $b; ENTER { 1 }; take $b } }
is-deeply gather-take.list, (1, 2, 3), 'and inside gather';
