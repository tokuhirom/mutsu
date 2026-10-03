use Test;

plan 8;

# An `is rw` routine handing back a missing element returns a location; a
# subscript store or `++` through it creates the element in the original
# container (Math::Symbolic's MultiHash `.elem(...)[0]++`).
my %h;
sub hk() is rw { %h<k> }
hk()[0] = 7;
is-deeply %h<k>, [7], 'an element store vivifies an Array in the entry';
hk()[1]++;
is-deeply %h<k>, [7, 1], '++ on the next element';

my %g;
sub gk() is rw { %g<c> }
gk()<k> = 1;
is-deeply %g<c>, {k => 1}, 'an associative store vivifies a Hash';

my %q;
sub qa() is rw { %q<a> }
qa()[0]++;
qa()[0]++;
is %q<a>[0], 2, 'repeated ++ through the routine accumulates';

my @a;
sub a2() is rw { @a[2] }
a2()[0]++;
is-deeply @a[2], [1], 'an array element location vivifies too';

my %o{Any};
my %key = x => 1;
sub ok-elem() is rw { %o{ $%key } }
ok-elem()[0]++ for ^3;
is %o.values[0][0], 3, 'an object-hash entry keyed by a Hash';

class C { has %.h; method el($k) is rw { %!h{$k} } }
my $c = C.new;
$c.el("a")[0]++ for ^2;
is $c.h<a>[0], 2, 'through an rw method with an argument';

my %b;
my $t := %b<a>;
$t[0]++;
$t[0]++;
is-deeply %b<a>, [2], 'a variable bound to a missing element';
