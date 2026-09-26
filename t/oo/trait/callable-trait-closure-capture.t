use Test;

plan 2;

# Regression extracted from Array::Circular 0.0.7: a callable variable trait
# argument can be retained by the role it composes.
# The closure must therefore capture the mutable outer scalar in a shared cell,
# even though the trait application itself happens during declaration setup.
role DynamicLimit[&limit] {
    method effective-limit { limit }
}

multi sub trait_mod:<is>(Variable:D \v, :$circular! is raw) {
    trait_mod:<does>(v, DynamicLimit[$circular]);
}

my $limit = 3;
my @values is circular({ $limit });
is @values.effective-limit, 3,
    'callable variable trait uses its initial closure value';

$limit = 5;
is @values.effective-limit, 5,
    'callable variable trait keeps the closure live after mutation';
