use Test;

# Assigning to a sigilless name bound to an object with a user `STORE` calls
# that `STORE` (rakudo's assignment falls back to `.STORE` on a non-Scalar
# target), and `name.=meth` on a sigilless name parses as a call argument
# (#9551; FixedInt's `is(foo.=ror(2), 131, ...)`).

plan 12;

class Fixed {
    has $!var handles <Str Numeric gist> = 0;
    method STORE($val) { $!var = $val +& 255 }
    method ror(Int $bits = 1) { $!var +> $bits +| ($!var +& 255 +< (8 - $bits)) }
}
sub f(*@a) { @a.map(*.Str).join(",") }

my \foo = Fixed.new;
is (foo -= 1), 255, 'compound assignment in expression position calls STORE';
foo += 15;
is +foo, 14, 'statement compound assignment calls STORE';
is (foo.=ror(2)), 131, '.= in expression position calls STORE';
is f(foo.=ror(1), 7), '193,7', '.= as a call argument followed by another argument';
foo = 300;
is +foo, 44, 'plain assignment calls STORE';
isa-ok foo, Fixed, 'the name still holds the same object';
sub g() { foo = 3; foo.=ror(1); +foo }
is g(), 129, 'assignment through the name from a closure calls STORE';

my \plain = 5;
throws-like { plain = 6 }, X::Assignment::RO, 'a sigilless value without STORE stays immutable';

my $c = -3;
my \alias = $c;
is (alias.=abs), 3, '.= on a sigilless alias of a container, as an expression';
is $c, 3, '... writes through to the aliased container';

my \bar = Fixed.new;
is f(bar -= 1, 255, 'm'), '255,255,m', 'a compound assignment argument stops at the comma';
is f(bar = 3, 4), '2', 'a plain assignment argument is list assignment (STORE gets the list)';
