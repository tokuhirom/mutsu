use Test;

plan 5;

# An `is rw` accessor store obeys the attribute's `where` clause, not only
# its type: rakudo dies with `expected <anon> but got Int (-3)`.
class C {
    has Int $.x is rw where * > 0 = 1;
    has $.y is rw where { .chars < 3 } = 'a';
}

my $c = C.new;
$c.x = 5;
is $c.x, 5, 'an rw accessor store accepts a value satisfying the where clause';
throws-like { $c.x = -3 }, X::TypeCheck::Assignment,
    message => /'expected <anon> but got Int (-3)'/,
    'an rw accessor store rejects a value failing the where clause';
is $c.x, 5, 'the rejected store leaves the attribute unchanged';

$c.y = 'ok';
is $c.y, 'ok', 'an untyped where-constrained rw attribute accepts a valid value';
dies-ok { $c.y = 'too long' },
    'an untyped where-constrained rw attribute rejects an invalid value';
