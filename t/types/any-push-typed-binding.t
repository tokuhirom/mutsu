use Test;

# `Any`'s growing methods autovivify a binding that holds any type object, and
# the vivification is an assignment, so it obeys the binding's type (#13823).
# Expected values are the `raku` (v2026.09) output.

plan 7;

my Int $x;
throws-like { $x.push(3) }, X::TypeCheck::Assignment,
    message => 'Type check failed in assignment to $x; expected Int but got Array ([])',
    'a typed scalar refuses the vivified Array';

my Str $s;
throws-like { $s.unshift(1) }, X::TypeCheck::Assignment, 'unshift too';

my $y = Int;
$y.append(1, 2);
is $y.raku, '$[1, 2]', 'an untyped binding holding a type object vivifies';

class Foo {}
my $f = Foo;
$f.push(2);
is $f.raku, '$[2]', '... a user class type object as well';

my Any $a;
$a.prepend(4);
is $a.raku, '$[4]', 'an Any-typed binding takes the Array';

my Cool $c;
$c.push(1);
is $c.raku, '$[1]', 'an Array is Cool, so a Cool binding takes it';

throws-like { Int.push(1) }, X::Assignment::RO, 'a type object with no binding is read-only';
