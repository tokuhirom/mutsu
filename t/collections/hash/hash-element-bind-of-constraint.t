use v6;
use Test;

plan 4;

# `%h<k> := $y` makes the element BE `$y`'s container, so that container keeps
# `$y`'s declared `of` constraint and `is default`: a `Nil` stored through a
# sigilless alias of the element resets `$y` to its default or type object,
# not to `Any` (#11618).

{
    my Int $y = 1;
    my %h;
    %h<k> := $y;
    for %h.values -> \w { w = Nil }
    is $y.raku, 'Int', 'hash element bind: Nil through an alias resets to the type object';
}

{
    my Int $y = 1;
    my @a;
    @a[0] := $y;
    for @a -> \w { w = Nil }
    is $y.raku, 'Int', 'array element bind: Nil through an alias resets to the type object';
}

{
    my Int $y is default(7) = 1;
    my %h;
    %h<k> := $y;
    for %h.values -> \w { w = Nil }
    is $y, 7, 'the source variable\'s `is default` is honored too';
}

{
    my Str $y = 'x';
    my %h;
    %h<k> := $y;
    for %h.values -> \w { w = Nil }
    is $y.raku, 'Str', 'any declared type, not only Int';
}
