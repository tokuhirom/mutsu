use Test;

plan 3;

throws-like { Int(Str).^coerce(3.141592653589793) },
    X::Coerce::Impossible,
    '.^coerce rejects unacceptable source value';

# The shape roast/S12-coercion/coercion-types.t exercises (rakudo #1800): a `%`
# parameter whose sub-signature binds a coercive named. The bare `-> (:Str(Any)
# :$suffix)` this test used to write is not valid Raku at all — `raku` rejects it
# with "Missing block" — so it was pinning a mutsu-only parse.
my %unit-multipliers = :s(1);
my $mapped = ({ suffix => 's' },).map(-> % ( Str(Any) :$suffix ) { %unit-multipliers{$suffix} });
is $mapped[0], 1, 'map callback with named coercive parameter binds pair payload';

my class SubCo {...}
my class Co {
    method SubCo() { SubCo.new }
    method invocant(SubCo(Co) \SELF:) { SELF }
}
my class SubCo is Co { }
isa-ok Co.invocant, SubCo, 'coercive invocant binds to coerced subclass';
