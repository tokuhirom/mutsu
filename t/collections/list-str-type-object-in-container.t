use Test;

# A type object stringifies to the empty string in Str context even when the
# list holds it through a variable's container: `(11, $T).Str` is "11 ", not
# "11 (Any)" (the gist of the type object).

plan 4;

{
    my $T = Any;
    is (11, $T).Str, '11 ', 'a type object held in a variable stringifies empty in a list';
    is (11, $T).join('|'), '11|', '.join looks through the container too';
    is "{(11, $T)}", '11 ', 'interpolation of the list agrees';
}

for 1, Str -> $a, $T {
    is (11, $T).Str, '11 ', 'a multi-param loop variable holding a type object';
}
