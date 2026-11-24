use Test;

# Assigning Nil to an element stores the container's default, also when a
# closure captures the container (its storage is then a shared cell) (#19109).

plan 6;

{
    my @a = 1, 2; my $c = { @a }; @a[1] = Nil;
    is @a.raku, '[1, Any]', 'captured @a: element = Nil stores Any';
}
{
    my @b = 1, 2; @b[1] = Nil;
    is @b.raku, '[1, Any]', 'uncaptured @b: element = Nil stores Any';
}
{
    my %h = a => 1; my $c = { %h }; %h<a> = Nil;
    is %h.raku, '{:a(Any)}', 'captured %h: element = Nil stores Any';
}
{
    my @a is default(42) = 1, 2; my $c = { @a }; @a[1] = Nil;
    is @a.raku, '[1, 42]', 'captured @a is default(42): Nil stores 42';
}
{
    my Int @a = 1, 2; my $c = { @a }; @a[1] = Nil;
    is @a.raku, 'Array[Int].new(1, Int)', 'captured Int @a: Nil stores Int';
}
{
    my @a = 1, 2; my $c = { @a[0] = Nil }; $c();
    is @a.raku, '[Any, 2]', 'Nil stored from inside the closure stores Any';
}
