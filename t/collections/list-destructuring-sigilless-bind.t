use v6;
use Test;

plan 18;

# A sigilless target of a list-destructuring BIND is an alias of the
# corresponding RHS container, exactly like the single-variable
# `my \a := $x` form. It used to be declared as a value plus two readonly
# marks, so `a = 10` died with "Cannot assign to an immutable value".

{
    my ($x, $y) = 1, 2;
    my (\a, \b) := ($x, $y);
    a = 10;
    is $x, 10, 'writing the alias writes the source scalar';
    is $y, 2, 'the untouched source is unaffected';
    is b, 2, 'reading the second alias still works';
}

{
    my ($x, $y) = 1, 2;
    my (\a, \b) := ($x, $y);
    a = 10;
    b = 20;
    is-deeply ($x, $y), (10, 20),
        'list destructuring binds sigilless names to scalar lvalues';
}

{
    my ($x, $y) = 1, 2;
    my (\a, \b) := ($x, $y);
    $x = 7;
    is a, 7, 'writing the source is visible through the alias';
}

{
    # A `$`-sigiled target of a bind is a read-only COPY in Raku, not an alias.
    my ($x, $y) = 1, 2;
    my ($a, $b) := ($x, $y);
    $x = 7;
    is $a, 1, 'a $ target of := does not alias its source';
    dies-ok { $a = 10 }, 'a $ target of := is read-only';
}

{
    # Mixed sigilless / sigilled targets.
    my ($x, $y) = 1, 2;
    my (\a, $b) := ($x, $y);
    a = 10;
    is $x, 10, 'sigilless target aliases even next to a $ target';
    is $b, 2, 'the $ target still reads its value';
}

{
    # The RHS need not be a parenthesised list of variables.
    my @z = 1, 2;
    my (\a, \b) := @z;
    a = 10;
    is-deeply @z, [10, 2], 'binding an array aliases its elements';
}

{
    my ($x, $y, $z) = 1, 2, 3;
    my (\a, \b, \c) := ($x, $y, $z);
    c = 30;
    is "$x $y $z", '1 2 30', 'the third target aliases the third source';
}

{
    # The alias survives into a closure.
    my ($x, $y) = 1, 2;
    my (\a, \b) := ($x, $y);
    my $f = { a = 99 };
    $f();
    is $x, 99, 'a closure writing the alias reaches the source';
}

{
    # A non-container element stays immutable.
    my (\a, \b) := (5, 6);
    is a + b, 11, 'binding plain values still reads correctly';
    dies-ok { a = 10 }, 'binding a plain value stays immutable';
}

dies-ok {
    my (\a, \b) := (1 + 1, 2);
    a = 10;
}, 'binding a computed value still produces a readonly sigilless term';

{
    # List ASSIGNMENT (`=`) to sigilless targets is not a bind.
    my (\a, \b) = (5, 6);
    is a, 5, 'list assignment to a sigilless target reads back';
    dies-ok { a = 10 }, 'list assignment to a sigilless target is immutable';
}

{
    # Regression guard: a non-slurpy `@` target of a bind still binds through.
    my @x = 1, 2;
    my (@a,) := (@x,);
    @a.push(3);
    is-deeply @x, [1, 2, 3], 'an @ target of := still binds its element';
}
