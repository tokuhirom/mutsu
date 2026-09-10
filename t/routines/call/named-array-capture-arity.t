use Test;

# `@<name>=` array-sigil named capture only yields a List (Array) when the
# aliased atom is itself a capturing construct: a capture group `(...)` or a
# subrule call `<alpha>`. Aliasing a plain atom (metachar, non-capturing group,
# or char class `<[...]>`) matches once and yields a single Match, exactly like
# the scalar `$<name>=` form.

plan 16;

# --- non-capturing atoms => single Match ---
"abc" ~~ /@<c>=\w+/;
is $<c>.^name, 'Match', 'metachar quantified @<c>=\w+ is a Match';
is $<c>.Str, 'abc', '  ... with the whole span as its value';

"abc" ~~ /@<c>=[\w]+/;
is $<c>.^name, 'Match', 'non-capturing group @<c>=[\w]+ is a Match';

"abc" ~~ /@<c>=<[a..z]>/;
is $<c>.^name, 'Match', 'char class @<c>=<[a..z]> is a Match';
is $<c>.Str, 'a', '  ... matching one char';

"abc" ~~ /@<c>=<[a..z]>+/;
is $<c>.^name, 'Match', 'quantified char class @<c>=<[a..z]>+ is a Match';
is $<c>.Str, 'abc', '  ... spanning the whole run';

# --- capturing constructs => Array (List) ---
"abc" ~~ /@<c>=(\w)/;
is $<c>.^name, 'Array', 'capture group @<c>=(\w) is an Array';
is $<c>.elems, 1, '  ... with one element';

"abc" ~~ /@<c>=(\w)+/;
is $<c>.^name, 'Array', 'quantified capture group @<c>=(\w)+ is an Array';
is $<c>.elems, 3, '  ... one Match per iteration';
is $<c>[1].Str, 'b', '  ... elements preserve per-iteration matches';

"abc" ~~ /@<c>=<alpha>/;
is $<c>.^name, 'Array', 'subrule @<c>=<alpha> is an Array';

"abc" ~~ /@<c>=<alpha>+/;
is $<c>.^name, 'Array', 'quantified subrule @<c>=<alpha>+ is an Array';

# --- scalar `$<name>=` alias is unaffected: always a single Match ---
"abc" ~~ /$<c>=\w+/;
is $<c>.^name, 'Match', 'scalar $<c>=\w+ stays a Match';
"abc" ~~ /$<c>=(\w)/;
is $<c>.^name, 'Match', 'scalar $<c>=(\w) is a single Match (group aliased)';
