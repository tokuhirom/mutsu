use Test;

# A `}` that ends its line ends the statement, so a next line that opens with
# a junction-operator character starts a new statement instead of joining the
# block as `&`/`|`/`^` (#9552; User::Timezone binds two subs this way).

plan 5;

package P {
    &OUR::a := sub a { 1 }
    &OUR::b := sub b { 2 }
}
is &P::a(), 1, 'first `&OUR::a := sub {...}` bind';
is &P::b(), 2, 'second bind on the next line is its own statement';

my $s = sub { 3 }
|| 0;
isa-ok $s, Sub, 'a `sub {...}` ending its line is not joined to the next line';

my $slip = sub { 4 }
|(5, 6);
isa-ok $slip, Sub, '`|` opening the next line is a prefix slip, not an any-junction';

my $j = sub { 7 }( ) & 8;
isa-ok $j, Junction, 'on one line `&` is still the all-junction infix';
