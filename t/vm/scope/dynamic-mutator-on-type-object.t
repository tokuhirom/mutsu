use Test;

# A mutator called on a bare type word refuses it the same way whether the
# method name is literal or dynamic (`List."$m"(1)`, #13888): `List` is
# immutable, and an `Array` type object with no container to vivify binds no
# `STORE` candidate.

plan 6;

my $m = 'push';
throws-like { List."$m"(1) }, X::Immutable, 'List."push"(1) by a dynamic name';
throws-like { List.push(1) }, X::Immutable, 'List.push(1)';
throws-like { Array."$m"(1) }, X::Multi::NoMatch, 'Array."push"(1) by a dynamic name';
throws-like { Array.push(1) }, X::Multi::NoMatch, 'Array.push(1)';

my Array $t;
$t."$m"(2);
is-deeply $t, [2], 'a typed scalar holding Array still vivifies';
my @a;
@a."$m"(3);
is-deeply @a, [3], 'a dynamic mutator on an array variable';
