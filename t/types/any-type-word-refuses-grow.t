use Test;

# The bare `Any` word is a type object with no container, so the four growing
# methods refuse it as Rakudo does (X::Assignment::RO) instead of growing a
# detached temporary (#14107).

plan 12;

throws-like { Any.push(1) }, X::Assignment::RO, 'Any.push';
throws-like { Any.append(1) }, X::Assignment::RO, 'Any.append';
throws-like { Any.unshift(1) }, X::Assignment::RO, 'Any.unshift';
throws-like { Any.prepend(1) }, X::Assignment::RO, 'Any.prepend';
throws-like { my \s = Any; s.push(1) }, X::Assignment::RO, 'a sigilless binding of Any';
throws-like { Array.push(1) }, X::Multi::NoMatch, 'Array.push is unchanged';

# A variable still autovivifies, including one that happens to be named `Any`.
my $x;
$x.push(2);
is-deeply $x, [2], 'an undefined scalar autovivifies';
my $Any = Any;
$Any.push(1);
is-deeply $Any, [1], 'a scalar named $Any autovivifies';
my Any $typed;
$typed.append(3);
is-deeply $typed, [3], 'a scalar typed Any autovivifies';

# Other calls on the keyword are unaffected.
is Any.gist, '(Any)', 'Any.gist';
is Any.^name, 'Any', 'Any.^name';
nok Any.defined, 'Any.defined';

done-testing;
