use Test;

# A trailing LEAVE/KEEP/UNDO/PRE/POST phaser is a block's last statement, so
# the block's value is Nil (rakudo), whatever value the statement before it
# had; a trailing ENTER's value IS the block's value. Before #10468 unified
# "the last value statement of a block", a `do` block or a closure answered
# the previous statement's value and so ran KEEP where rakudo runs UNDO.

plan 14;

sub v { 42 }

ok (do { v(); LEAVE { v() } }) === Nil, 'do block, trailing LEAVE';
ok (do { v(); POST { True } }) === Nil, 'do block, trailing POST';
ok (do { v(); PRE { True } }) === Nil, 'do block, trailing PRE';
is (do { v(); ENTER { 7 } }), 7, 'do block, trailing ENTER gives its value';
is (do { v(); LEAVE { v() }; 43 }), 43, 'a phaser before the tail is not the tail';

my $c = { v(); LEAVE { v() } };
ok $c() === Nil, 'closure, trailing LEAVE';
my $e = { v(); ENTER { 7 } };
is $e(), 7, 'closure, trailing ENTER gives its value';
ok (1, 2).map({ v(); LEAVE { v() } }).List eqv (Nil, Nil), 'map block, trailing LEAVE';

sub s1 { v(); LEAVE { v() } }
ok s1() === Nil, 'sub, trailing LEAVE';

{
    my @log;
    my $v = do { v(); KEEP { @log.push: 'keep' }; UNDO { @log.push: 'undo' } };
    is @log, ['undo'], 'do block with trailing KEEP/UNDO runs UNDO';
    ok $v === Any, '... and its value is Nil';
}

{
    my @log;
    { v(); KEEP { @log.push: 'keep' }; UNDO { @log.push: 'undo' } }
    is @log, ['undo'], 'bare block with trailing KEEP/UNDO runs UNDO';
}

{
    my @log;
    { KEEP { @log.push: 'keep' }; UNDO { @log.push: 'undo' }; v() }
    is @log, ['keep'], 'a defined tail after the phasers runs KEEP';
}

{
    sub m { my \x = v() }
    is m(), 42, 'a sigil-less declaration tail is the value (markers skipped)';
}
