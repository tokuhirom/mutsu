use Test;

# An array subscript with an exclusive-start Range (`a ^.. b`, `a ^..^ b`)
# addresses the same window as `a+1 .. b` / `a+1 ..^ b`. mutsu had no array
# arm for those two Range shapes and returned Nil. Reduced from
# Cro::WebSocket's message-parser test, which splits a payload with
# `@random-data[75^..^173]`.

plan 8;

my @r = ^10;
is-deeply @r[2^..^5], (3, 4), 'a ^..^ b';
is-deeply @r[2^..5], (3, 4, 5), 'a ^.. b';
is-deeply @r[7^..*], (8, 9), 'a ^.. *';
is-deeply @r[5^..^5], (), 'empty a ^..^ a';
is-deeply @r[5^..^6], (), 'empty a ^..^ a+1';
is-deeply @r[0^..2], (1, 2), 'starting after index 0';
is @r[8^..^20].elems, 11, 'past the end pads with Any, like a+1 ..^ b';
my @data = (^256).list;
is Blob.new(@data[25^..^75]).elems, 49, 'Blob from an exclusive slice';
