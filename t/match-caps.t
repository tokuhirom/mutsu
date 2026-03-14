use Test;

plan 13;

{
    "abc" ~~ /(a)/;
    my @caps = $/.caps;
    is @caps.elems, 1, '.caps returns one positional capture';
    isa-ok @caps[0].key, Int, '.caps positional key is Int';
    is @caps[0].key, 0, '.caps positional key value is 0';
    isa-ok @caps[0].value, Match, '.caps positional value is Match';
    is @caps[0].value.Str, 'a', '.caps positional value stringifies to the capture';
}

{
    "abc" ~~ /$<x>=(a)/;
    my @caps = $/.caps;
    is @caps.elems, 1, '.caps returns one named capture';
    isa-ok @caps[0].key, Str, '.caps named key is Str';
    is @caps[0].key, 'x', '.caps named key value is the capture name';
    isa-ok @caps[0].value, Match, '.caps named value is Match';
}

{
    "abcdef" ~~ /(a)b(c)d(e)f/;
    my @chunks = $/.chunks;
    is @chunks.elems, 6, '.chunks interleaves captures with unmatched text';
    isa-ok @chunks[0].key, Int, '.chunks positional key is Int';
    is @chunks[1].key, '~', '.chunks gap key is ~';
    is @chunks[1].value, 'b', '.chunks gap value is unmatched text';
}

done-testing;
