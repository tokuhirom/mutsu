use Test;
plan 3;

# A method declaring a raw invocant arms the raw-invocant arrival probe, which
# resolves the call's method without running it. A candidate's `where` clause
# reading a private attribute must see `self` during that probe.
class Raw { method touch(\SELF:) is raw { SELF } }

class Registry {
    has %!coerce is built;
    multi method parse(Str:D $name where (%!coerce{.lc}:exists), $val) { "coerce:$name" }
    multi method parse(Str:D $name, $val) { "plain:$name" }
}

my $r = Registry.new(:coerce{ border => 1 });
is $r.parse('Border', 'x'), 'coerce:Border', 'where reading %!attr picks the first candidate';
is $r.parse('other', 'x'), 'plain:other', 'falls through to the plain candidate';
my \m = Registry.new(:coerce{ a => 1 });
is m.parse('a', 1), 'coerce:a', 'sigilless invocant';
