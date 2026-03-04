use Test;

plan 14;

# Basic positional caps
{
    "abcdef" ~~ /(a)b(c)d(e)f/;
    my @caps = $/.caps;
    is @caps.elems, 3, '.caps returns 3 positional captures';
    is @caps[0].key, '0', 'first cap key is 0';
    is @caps[0].value, 'a', 'first cap value is a';
    is @caps[1].key, '1', 'second cap key is 1';
    is @caps[1].value, 'c', 'second cap value is c';
    is @caps[2].key, '2', 'third cap key is 2';
    is @caps[2].value, 'e', 'third cap value is e';
}

# Named captures
{
    "abc123" ~~ /$<a>=(\w+)$<b>=(\d+)/;
    my @caps = $/.caps;
    is @caps.elems, 2, '.caps returns 2 named captures';
    ok @caps[0].key eq 'a' || @caps[0].key eq 'b', 'named cap has correct key';
}

# Chunks with gaps
{
    "abcdef" ~~ /(a)b(c)d(e)f/;
    my @chunks = $/.chunks;
    is @chunks.elems, 6, '.chunks returns 6 elements (3 captures + 3 gaps)';
    is @chunks[0].key, '0', 'first chunk is positional capture 0';
    is @chunks[1].key, '~', 'second chunk is gap with key ~';
    is @chunks[1].value, 'b', 'gap text is b';
}

# No captures
{
    "abc" ~~ /abc/;
    my @caps = $/.caps;
    is @caps.elems, 0, '.caps with no captures returns empty list';
}

done-testing;
