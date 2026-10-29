use Test;

# Data::Dump::Tree (LayHorizontal `match_target`): `-> $t is copy` copies the
# Scalar, not the Array/Hash it holds, so `$t === $outer` stays true.
plan 4;

my %h = a => 1;
my @a = 1, 2;
my $list = (%h, @a);

for ($list[0],) -> $t is copy {
    ok $t === %h, 'Hash identity kept by a $ is copy loop parameter';
}
for ($list[1],) -> $t is copy {
    ok $t === @a, 'Array identity kept by a $ is copy loop parameter';
}
for ($list[0],) -> $t is copy {
    $t = 5;
    is %h<a>, 1, 'assigning the copy does not touch the source';
}
for (@a,) -> @t is copy {
    @t.push: 3;
    is @a.elems, 2, '@ is copy still copies the Array';
}
