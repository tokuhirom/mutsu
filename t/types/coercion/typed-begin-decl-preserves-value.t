use v6;
use Test;

# A BEGIN assignment creates the lexical container before the mainline
# declaration is initialized. A typed bare container declaration must preserve
# that value instead of replacing it with its synthesized empty default.

plan 6;

{
    my Int @numbers;
    BEGIN @numbers = (10, 20);
    is @numbers.elems, 2, 'typed array keeps its BEGIN-time elements';
    is @numbers.join(','), '10,20', 'typed array keeps its BEGIN-time order';
}

{
    my Int %counts;
    BEGIN %counts = (ten => 10);
    is %counts<ten>, 10, 'typed hash keeps its BEGIN-time value';
    is %counts.keys.join(','), 'ten', 'typed hash keeps its BEGIN-time key';
    %counts<hundred> = 100;
    is %counts.keys.sort.join(','), 'hundred,ten',
        'typed hash remains writable after the BEGIN-time assignment';
}

{
    my Str @words;
    BEGIN { @words = <first second> };
    is @words.join(' '), 'first second',
        'a block-bodied BEGIN also preserves a typed array';
}
