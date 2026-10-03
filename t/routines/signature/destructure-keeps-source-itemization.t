use Test;

# List destructuring stages its RHS in a temp that neither adds nor removes
# element itemization (#9898, ADR-0040 §11, ADR-0079 §6 rows 5-6): a List
# literal's bare hash flattens into a `%` target, while an Array's element is
# a Scalar holder and stays one item. Measured against rakudo.

plan 12;

my %h = a => 1, b => 2;

{
    my @a = 1, %h;
    throws-like { my ($y, %r) = @a }, X::Hash::Store::OddNumber,
        'an Array element hash is one item: a % target dies';
}
{
    my ($y, %r) = 1, %h;
    is-deeply %r, %h, 'a List literal\'s bare hash flattens into a % target';
}
{
    my ($y, %r) = (1, %h);
    is-deeply %r, %h, '... also parenthesized';
}
{
    my @a = 1, %h;
    my ($y, @r) = @a;
    is @r.elems, 1, 'an Array element hash stays one element of an @ target';
}
{
    my ($y, @r) = 1, %h;
    is @r.elems, 1, 'a List literal\'s hash is one element of an @ target';
}
{
    my ($a, @b) = 1, $[1, 2];
    is @b.raku, '[[1, 2],]', 'an explicitly itemized array stays one item';
}
{
    my ($a, @b) = 1, [1, 2];
    is @b.raku, '[[1, 2],]', 'an array literal is one element of an @ target';
}
{
    my @x = 1, [1, 2];
    my ($a, @b) = @x;
    is @b.raku, '[[1, 2],]', 'an Array element array stays one item';
}
{
    sub f { 1, {a => 1} }
    my ($a, %r) = f();
    is-deeply %r, %(a => 1), 'a List returned by a sub flattens its hash';
}
{
    sub g { [1, {a => 1}] }
    throws-like { my ($a, %r) = g() }, X::Hash::Store::OddNumber,
        'an Array returned by a sub keeps its element holder';
}
{
    my @a = [1, 2], [3, 4];
    my ($p, $q) = @a;
    is-deeply $p, [1, 2], 'a $ target reads one element of an Array';
}
{
    my ($g, %rest) = 1, a => 2, b => 3;
    is-deeply %rest, %(a => 2, b => 3), 'pairs slurp into a % target';
}
