use Test;

plan 9;

my @arr := Array.new(:shape(2;2));
ok @arr[0]:exists, 'first row exists in shaped array';
nok @arr[0;0]:exists, 'cell does not exist before assignment';

@arr[0;0] = 'x';
is @arr[0;0], 'x', 'multidim assignment/read works';
ok @arr[0;0]:exists, 'cell exists after assignment';

throws-like { @arr[0] = 'y' }, X::NotEnoughDimensions,
    operation => 'assign to', got-dimensions => 1, needed-dimensions => 2;

# raku throws X::OutOfRange here (verified 2026-07-23); the old expectation
# (`is-deeply (try ...), @arr.default`) compared try's Nil against Any and
# only passed through the Nil-eqv-Any crutch.
throws-like { @arr[2;0]:delete }, Exception, message => /'out of range'/,
    'out-of-bounds delete on a shaped array throws';

@arr[1;1] := 42;
dies-ok { @arr[1;1] = 99 }, 'bound multidim cell is read-only';
dies-ok { @arr[3;0] := 1 }, 'bind out of bounds dies';

is @arr.shape, (2;2), 'shape method returns original dimensions';
