use Test;

# From ecosystem Trait::Env: a variable trait handler assigns the declared
# variable through `$v.var = ...`.
plan 3;

multi sub trait_mod:<is>(Variable $v, :$seven!) {
    $v.var = do given $v.var.WHAT { when Int { 7 }; default { 'seven' } };
}

my Int $i is seven;
my $s is seven;
is $i, 7, 'typed scalar assigned through .var';
is $s, 'seven', 'untyped scalar assigned through .var';
is-deeply [$i, $s], [7, 'seven'], 'the values stay put';
