use Test;

# A `&name` reference to a slurpy (`*@args`) builtin reports arity 0, matching
# raku — `&warn`, `&note`, `&say`, ... take no *required* positionals. mutsu
# previously defaulted an unknown builtin Routine to one positional (arity 1),
# which sent Template::Mustache's logger — keyed on `&warn.?arity == 2` via
# `.?arity // 0 == 2` — down the wrong branch.

plan 7;

is &warn.arity,  0, '&warn.arity is 0';
is &note.arity,  0, '&note.arity is 0';
is &say.arity,   0, '&say.arity is 0';
is &print.arity, 0, '&print.arity is 0';
is &put.arity,   0, '&put.arity is 0';

# `.?arity // 0 == 2` (the Mustache logger guard) must be false for &warn so it
# takes the formatted-message branch, not the 2-arg one. (`==` binds tighter than
# `//`, so this is `&warn.?arity // (0 == 2)` = `0 // False` = 0 = falsy.)
nok (&warn.?arity // 0 == 2), 'the Mustache &warn.?arity guard is false';

# A genuinely 1-ary builtin still reports its arity.
is &chr.arity,   1, '&chr.arity is 1 (single required positional)';
