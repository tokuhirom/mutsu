use Test;

plan 3;

throws-like { Int(Str).^coerce(3.141592653589793) },
    X::Coerce::Impossible,
    '.^coerce rejects unacceptable source value';

my %pairs = :suffix("s");
my %unit-multipliers = :s(1);
my $mapped = %pairs.map(-> (:Str(Any) :$suffix) { %unit-multipliers{$suffix} });
is $mapped[0], 1, 'map callback with named coercive parameter binds pair payload';

my class SubCo {...}
my class Co {
    method SubCo() { SubCo.new }
    method invocant(SubCo(Co) \SELF:) { SELF }
}
my class SubCo is Co { }
isa-ok Co.invocant, SubCo, 'coercive invocant binds to coerced subclass';
