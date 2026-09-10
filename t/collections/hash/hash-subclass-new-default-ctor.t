use Test;
plan 9;

# `class Bar is Hash {}` (no user-defined `new`) used to construct a REAL
# Instance (`.^name` correctly reported `Bar`), but had no delegation
# subsystem at all for Associative-protocol methods -- not even a working
# `AT-KEY`/subscript read. See todo/deep/hash-subclass-instance-has-no-
# method-delegation.md.
class Bar is Hash {
    method AT-KEY($k) { nextwith $k.lc }
}

my $h = Bar.new(a => 1, b => 2);
is $h.^name, 'Bar', 'named-arg new tags the subclass';
is $h.elems, 2, 'named args populate the backing storage';
is $h<a>, 1, 'subscript read delegates to the backing storage';
is $h<b>, 2, 'subscript read delegates to the backing storage (2)';

# A custom Associative override calling nextwith reaches the native Hash
# base, even though the class has no OTHER user candidate in the MRO for
# AT-KEY.
is $h.AT-KEY("A"), 1, 'nextwith from an Associative override reaches the native Hash base';

# `Bar.new` (no args at all) still produces a real Instance, and subscript
# assignment on it does not silently degrade the variable to a plain Hash.
my $h1 = Bar.new;
is $h1.^name, 'Bar', 'no-args new still tags the subclass';
$h1{"x"} = 1;
is $h1.^name, 'Bar', 'subscript assignment does not clobber the instance with a plain Hash';
is $h1<x>, 1, 'subscript assignment writes the backing storage';

my %g is Bar = a => 1, b => 2;
is %g.^name, 'Bar', 'the "is" trait on a %-sigil variable blesses as the user Hash subclass';
