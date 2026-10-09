use Test;

# From Math::NIntegrate (Rule::ClenshawCurtis): `submethod BUILD(:@!ab = Empty)`
# must leave an Array in the attribute, so a later `self.ab = |@x` stores an
# Array rather than a Slip.
plan 6;

class G { has @.ab is rw; submethod BUILD(:@!ab = Empty) { }; method fill { self.ab = |[1, 2, 3] } }
my $g = G.new;
isa-ok $g.ab, Array, 'Empty default of :@!attr is an Array';
$g.fill;
isa-ok $g.ab, Array, 'slip assignment afterwards stays an Array';
is $g.ab.elems, 3, 'elements kept';
is ($g, $g).map(*.ab).elems, 2, 'map over accessors does not flatten';

class H { has @.ab is rw; submethod BUILD(:@!ab = ()) { } }
isa-ok H.new.ab, Array, 'List default of :@!attr is an Array';

sub f(@a = Empty) { @a.WHAT }
is f().^name, 'Slip', 'plain sub parameter default is untouched';
