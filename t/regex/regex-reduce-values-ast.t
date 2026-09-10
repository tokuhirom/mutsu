use v6;
use Test;

# A parent rule's trailing `{ … }` action sees `$/` with its child captures'
# produced (`.made`) values via `$/.hash` / `$/.values`, not only via `$<name>`.
# The reduce-time `$/` was built with an empty capture hash, so `$/.values` was
# empty and `{ make $/.values[0].ast }` produced Nil — while `$<child>.made`
# worked. YAMLish's `Schema::JSON` TOP is exactly `{ make $/.values[0].ast }`.

plan 4;

grammar G {
    token TOP { <element> { make $/.values[0].ast; } }
    token element { \d+ { make $/.Str.Int } }
}
is G.parse("42").ast, 42, '$/.values[0].ast carries the child rule''s made value';

grammar H {
    token TOP { <element> { make $/<element>.ast; } }
    token element { \d+ { make $/.Str.Int } }
}
is H.parse("7").ast, 7, '$/<name>.ast carries the child made value';

grammar J {
    token TOP { <element> { make $<element>.made; } }
    token element { \w+ { make $/.uc } }
}
is J.parse("abc").ast, "ABC", '$<name>.made still works';

# `$/.hash` at reduce time exposes the child capture.
grammar K {
    token TOP { <a> <b> { make $/.hash.keys.sort.join(",") } }
    token a { \d+ }
    token b { \w+ }
}
is K.parse("1x").ast, "a,b", '$/.hash exposes named captures at reduce time';
