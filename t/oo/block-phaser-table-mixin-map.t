use Test;
use nqp;

# A role mixed into a Block is iterated by `.map` as the Block itself (its
# `last`, `NEXT`, `FIRST`, `LAST`), unless the role composes `CALL-ME`; and a
# phaser table bound on such a mixin through `nqp::bindattr($b, Block,
# '$!phasers', ...)` replaces the block's loop phasers. ParaSeq's `BlockMapper`
# relies on both to run a hypered `map` batch by batch (#17337).

plan 8;

my role Marker { }

is-deeply (1..10).map({ last if $_ == 3; $_ } but Marker).List, (1, 2), 'last ends the map';
my $nexts = 0;
is-deeply (1..3).map({ NEXT $nexts++; $_ } but Marker).List, (1, 2, 3), 'NEXT runs';
is $nexts, 3, 'once per iteration';
my $log = '';
(1..4).map({ FIRST $log ~= 'F'; LAST $log ~= 'L'; $_ } but Marker).eager;
is $log, 'FL', 'FIRST and LAST run once';

my role Caller { method CALL-ME(|) { 42 } }
is-deeply (1..3).map({ $_ } but Caller).List, (42, 42, 42), 'a role with CALL-ME is called';

my role Table { }
my $lasts = 0;
my $m := ({ LAST $lasts++; $_ * 2 } but Table);
my $table := nqp::clone(nqp::getattr($m, Block, '$!phasers'));
ok nqp::existskey($table, 'LAST'), 'the table lists the block LAST';
my $replaced = 0;
nqp::bindkey($table, 'LAST', nqp::list({ $replaced++ }));
nqp::bindattr($m, Block, '$!phasers', $table);
is-deeply (1..3).map($m).List, (2, 4, 6), 'a bound table keeps the block value';
is "$lasts $replaced", '0 1', 'and its LAST replaces the block LAST';
