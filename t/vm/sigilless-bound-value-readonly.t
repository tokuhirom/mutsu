use Test;

# A sigilless term bound at run time to a plain value (a Map element is a
# value, not a container) is readonly: `+=` and an `is rw` argument refuse
# it, as plain assignment already did (#17677).

plan 12;

my $map = Map.new((a => 1));
sub isrw($x is rw) { $x = 7 }

sub in-sub() {
    my \h = $map<a>;
    throws-like { isrw(h) }, X::Parameter::RW, 'an is rw argument refuses it (in a sub)';
    throws-like { h += 1 }, X::Assignment::RO, '+= refuses it';
    throws-like { h = 3 }, X::Assignment::RO, '= refuses it';
    is h, 1, 'its value is untouched';
}
in-sub();

my \g = $map<a>;
throws-like { g += 1 }, X::Assignment::RO, '+= refuses it at the top level';
throws-like { isrw(g) }, X::Parameter::RW, '... and so does an is rw argument';
throws-like { (g = 5) }, X::Assignment::RO, 'an assignment in expression position too';
is g, 1, 'the value is untouched';

# A term bound to a writable container still writes through.
my %hh = a => 1;
my \e = %hh<a>;
e += 1;
is %hh<a>, 2, '+= through a term bound to a Hash element writes the element';
isrw(e);
is %hh<a>, 7, 'an is rw argument writes the element';
my $v = 10;
my \t = $v;
t += 5;
is $v, 15, 'a term bound to a variable writes the variable';
isrw(t);
is $v, 7, '... through an is rw argument too';
