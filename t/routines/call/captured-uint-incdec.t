use Test;

plan 8;

# `++`/`--` on a typed variable a closure captured (#17337): the step keeps
# the variable's declared type.

my uint $u = 0;
my &dec = { $u-- };
dec();
is $u, 18446744073709551615, 'a captured uint wraps below zero';
my &inc = { ++$u };
is inc(), 0, 'and wraps back to zero above its maximum';

my int8 $b = 127;
my &bump = { $b++ };
bump();
is $b, -128, 'a captured int8 wraps at its width';

my int $i = 5;
my &twice = { ++$i; ++$i };
is twice(), 7, 'prefix ++ on a captured int answers the new value';
my &post = { $i-- };
is post(), 7, 'postfix -- on a captured int answers the old value';
is $i, 6, 'and stores the stepped value';

subset Pos of Int where * > 0;
my Pos $p = 1;
my &down = { $p-- };
throws-like { down() }, X::TypeCheck::Assignment,
  'a captured subset-typed variable still checks its constraint';

my Int $n = 41;
my &up = { $n++ };
up();
is $n, 42, 'a captured Int steps';
