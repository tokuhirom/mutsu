use Test;

plan 1;

# A later parameter in a multi-parameter pointy block can carry a
# parenthesised hash sub-signature. Red::Driver::Mock uses this shape for the
# value half of `%when-str.kv`: `-> Str $str, % (:$counter = 0, :$times, |)`.
my %queries = select => %(counter => 1, times => 2);
my @seen;
for %queries.kv -> Str $name, % (:$counter = 0, :$times, |) {
    @seen.push: "$name:$counter:$times";
}

is @seen, ['select:1:2'],
    'a later pointy parameter accepts a parenthesised hash sub-signature';
