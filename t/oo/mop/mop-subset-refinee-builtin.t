use Test;

# `.^refinee` of the builtin subset `UInt` is `Int`, so walking a subset chain
# down to its nominal type ends there (Red's regex SQL cast reads it, t/109).

plan 3;

is UInt.^refinee.^name, 'Int', 'UInt refines Int';
subset Small of UInt where * < 10;
is Small.^refinee.^name, 'UInt', 'a user subset of UInt refines UInt';
my $type = Small;
$type = $type.^refinee while $type.HOW ~~ Metamodel::SubsetHOW;
is $type.^name, 'Int', 'the chain ends at Int';
