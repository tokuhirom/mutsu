use Test;

# Regression pin for `@0` / `@1` (numbered positional-capture array
# variables) as an element inside a `[...]` array literal.
#
# `@N` is `$N.list`; the parser's bare-`@` (anonymous array) branch treated
# any non-identifier-start next character as "no name here", which included
# digits. So `@0` parsed as an anonymous array, leaving the `0` as a stray
# unconsumed token -- harmless at statement level (it became a separate sunk
# statement) but a hard "Confused: Two terms in a row" parse error as an
# element inside `[...]`, where there is no statement boundary to absorb it.
#
# Discovered via Font::AFM.rakumod (a Text::CSV/CSV::Table dependency):
# `my Array $bbox = [ @0».Int ];` -- see
# todo/tickets/numbered-capture-array-var-in-array-literal.md.

plan 6;

"abc" ~~ / (\d+) /;
is [ @0 ].gist, '[(Any)]', '[ @0 ] parses -- $/ is Nil after a failed match, so @0 is [Nil.list] == [(Any)]';

"abc123" ~~ / (\d+) /;
is-deeply [ @0 ], [], '[ @0 ] is empty for a single (non-repeated) capture group';
is-deeply [ @0.Int ], [0], '[ @0.Int ] parses -- .Int chains onto @0 inside the literal';
is-deeply [@0], [], '[@0] (no surrounding spaces) parses the same way';

my @vals = [ @0».Int ];
is @vals.elems, 0, '[ @0».Int ] (the Font::AFM.rakumod shape) parses and evaluates';

"a1b2c3" ~~ / (\d)+ /;
is-deeply [ @0 ].map(*.Str).list, ('1',), '@1-repetition capture: @0 lists the repeated matches';
