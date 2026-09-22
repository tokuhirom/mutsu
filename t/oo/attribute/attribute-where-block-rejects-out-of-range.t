use Test;

plan 4;

# From the ecosystem Date::Event distribution (0.0.12, t/5-lat-lon.t): an
# attribute declared with both a type and an inline `where` block predicate
# reading `$_` directly (as opposed to the implicit-topic method-call shape
# in attribute-where-implicit-topic.t) must reject an out-of-range value both
# at construction and when reassigned through a later method call.
class Coord {
    has Numeric $.lat where { -90 <= $_ <= 90 };
    method lat(Numeric $v?) {
        if $v.defined { $!lat = $v } else { return $!lat }
    }
}

dies-ok { Coord.new(lat => 999) },
    'a where-block attribute constraint rejects an out-of-range value at construction';

my $c = Coord.new(lat => 10);
is $c.lat, 10, 'a within-range value is accepted at construction';

$c.lat: 50;
is $c.lat, 50, 'reassigning through a method accepts an in-range value';

dies-ok { $c.lat: 999 },
    'reassigning through a method rejects an out-of-range value';
