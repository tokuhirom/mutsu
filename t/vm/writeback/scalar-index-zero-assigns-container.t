use Test;

# rakudo's `Any.ASSIGN-POS` stores through `AT-POS`, and a defined scalar's
# `AT-POS(0)` is its own container: a positional store at index 0 into a
# variable or element holding a defined scalar assigns that container, and any
# other index is out of range `0..0` (#18952).

plan 18;

{
    my $x = 5;
    $x[0] = 3;
    is $x, 3, '$x[0] = v assigns the variable';
    is ($x[0] = 4), 4, 'the store answers the value';
    is $x, 4, 'and the variable holds it';
}

throws-like { my $x = 5; $x[1] = 3 }, X::OutOfRange,
    message => 'Index out of range. Is: 1, should be in 0..0', '$x[1] is out of range';

throws-like { my Int $x = 5; $x[0] = "a" }, X::TypeCheck::Assignment,
    'index 0 type-checks the value like $x = v';

{
    my $x = 5;
    my $c = { $x };
    $x[0] = 9;
    is $c(), 9, 'a closure over the variable sees the store';
}

sub f { my $x = 5; $x[0] = 7; $x }
is f(), 7, 'a routine-local scalar';

{
    my @a = 1, 2;
    @a[0][0] = 3;
    is-deeply @a, [3, 2], '@a[0][0] = v replaces the element';
}

throws-like { my @a = 1, 2; @a[0][1] = 3 }, X::OutOfRange,
    message => 'Index out of range. Is: 1, should be in 0..0', '@a[0][1] is out of range';

{
    my $a = [1, 2];
    $a[0][0] = 3;
    is-deeply $a, [3, 2], 'also through an itemized array';
}

{
    my %h = a => 1;
    %h<a>[0] = 9;
    is-deeply %h, %(a => 9), '%h<a>[0] = v replaces the value';
}

throws-like { my %h = a => 1; %h<a>[2] = 9 }, X::OutOfRange,
    message => 'Index out of range. Is: 2, should be in 0..0', '%h<a>[2] is out of range';

throws-like { my @a = 1, 2; @a[0]<k> = 3 }, X::AdHoc,
    message => 'Type Int does not support associative indexing.', 'an associative store is still refused';

{
    my @a;
    @a[0][0] = 1;
    is-deeply @a, [[1],], 'an absent element still autovivifies';
}

# A Pair is not Positional: the same rule (#18960).
{
    my $x = (a => 1);
    $x[0] = 9;
    is $x, 9, '$x[0] = v on a Pair assigns the variable';
}

throws-like { my $x = (a => 1); $x[1] = 9 }, X::OutOfRange,
    message => 'Index out of range. Is: 1, should be in 0..0', '$x[1] on a Pair is out of range';

{
    my %h = k => (a => 1);
    %h<k>[0] = 7;
    is-deeply %h, %(k => 7), '%h<k>[0] = v over a Pair replaces the entry';
}

{
    my $p = (c => [1, 2]);
    $p<c> = [3, 4];
    is-deeply $p.value, [3, 4], 'an associative store through a Pair still reaches its value';
}
