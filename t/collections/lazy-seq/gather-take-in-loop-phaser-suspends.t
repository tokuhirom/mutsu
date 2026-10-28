use Test;

# A `take` run from a LEAVE/KEEP phaser of a `for` body suspends the gather like
# a take in the body proper, so a gather over an infinite source stays lazy
# (annotations' Buffer.STAMP, #15296).

plan 5;

is (gather for 1..* -> $v { LEAVE { take $v } }).head(2), (1, 2), 'LEAVE take over an infinite range';
is (gather for 1..* -> $v { KEEP { take $v }; 1 }).head(3), (1, 2, 3), 'KEEP take over an infinite range';
is (gather for 1...* -> $v { LEAVE { take $v * 10 } }).head(2), (10, 20), 'LEAVE take over an infinite sequence';
is (gather for 1..5 -> $v { LEAVE { take $v } }).list, (1, 2, 3, 4, 5), 'a finite source still collects everything';

my @seen;
my $seq = gather for 1..* -> $v { @seen.push: $v; LEAVE { take $v } };
$seq.head(2).sink;
ok @seen.elems < 10, 'the body ran only as far as the pull needed';
