use Test;

plan 3;

throws-like { my $seq = Any ... * }, X::Method::NotFound,
    method => 'succ', typename => 'Any',
    'a type-object seed without succ fails instead of becoming zero';

class Seed {
    method succ { 42 }
}

is (Seed ... *)[1], 42, 'a type-object seed uses its compiled succ method';
is (Seed ... *)[2], 43, 'generation continues from the method result';
