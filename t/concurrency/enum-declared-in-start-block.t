use Test;

# A `my enum` declared in a routine that runs on several threads at once, as
# Cro's request and response parsers do in their supply bodies. Each run on a
# thread interpreter registers the enum again; the type metadata it writes is
# copied per type, not as one whole table (#12981), and every thread must see
# the same enum.

plan 5;

sub parse($i) {
    my enum Expecting <RequestLine Header Body>;
    (Header.value, Expecting.^name, Body.Str, $i)
}

my @r = await (^8).map: -> $i { start { parse($i) } };

is-deeply @r.map(*[0]).unique.List, (1,), 'every thread sees the variant values';
is-deeply @r.map(*[1]).unique.List, ('Expecting',), 'and the type name';
is-deeply @r.map(*[2]).unique.List, ('Body',), 'and the variant names';
is-deeply @r.map(*[3]).sort.List, (^8).List, 'every run completed';

# Type metadata written by the main thread after the workers ran is intact.
class Point is rw { has $.x }
ok Point.^rw, 'a class trait written to the type metadata still reads back';
