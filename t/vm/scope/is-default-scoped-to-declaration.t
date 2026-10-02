use Test;

# #10796: an `is default(...)` belongs to one declaration, not to the name.
# It must neither outlive the scope that declared it nor be erased by a later,
# unrelated same-named declaration.

plan 10;

{
    my $x = 1;
    { my $x is default(3) = 5 }
    $x = Nil;
    ok $x === Any, 'a block-scoped default does not leak into an outer same-named $x';
}

{
    sub f { my $y is default(7); -> { $y = Nil; $y } }
    my &g = f();
    my $y = 1;
    is g(), 7, 'a later same-named declaration does not erase a captured default';
}

{
    my $x is default(3);
    { my $x; $x = Nil; ok $x === Any, 'an inner redeclaration does not inherit the default' }
    $x = Nil;
    is $x, 3, 'the outer variable keeps its default after the inner scope';
}

{
    my @a is default(9);
    { my @a; ok @a[1] === Any, 'an inner @a does not inherit the outer default' }
    is @a[2], 9, 'the outer @a keeps its default';
}

{
    my $z is default(5) = 1;
    my &c = { $z = Nil };
    c();
    is $z, 5, 'a closure store of Nil still uses the default';
}

# The BEGIN makes the class body run in two halves (ADR-0134).
{ my $q is default(3) = 5 }
class B10796 { my $q = 5; $q = Nil; our $r = $q.raku }
BEGIN 1;
is $B10796::r, 'Any', 'a default does not leak into a class body split by a BEGIN prologue';

class C10796 { has $.v is default(42) is rw; method r { $!v = Nil; $!v } }
is C10796.new.r, 42, 'attribute defaults still apply inside methods';
my $o = C10796.new(v => 1);
$o.v = Nil;
is $o.v, 42, 'attribute defaults still apply through the accessor';
