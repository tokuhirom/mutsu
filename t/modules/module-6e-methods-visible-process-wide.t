use Test;
use lib 't/fixtures/module-6e-methods/lib';
use Rev6eMod;

# A module that declares `use v6.e.PREVIEW` runs its routines with the 6.e
# methods (.snip, .snitch, Str.nomark, IO::Path.stem, .can) whatever revision
# the importing program has, and rakudo makes those methods visible to every
# unit of the process once a 6.e unit loaded. Identical under raku.
plan 7;

is t-snitch(), 'snitch ok', 'Any.snitch inside the 6.e module';
is t-nomark(), 'e', 'Str.nomark inside the 6.e module';
is t-stem(), 'b', 'IO::Path.stem inside the 6.e module';
is t-can(), True, '.can finds a 6.e method inside the 6.e module';
is t-snip(), '((1, 2), (5,)).Seq', 'Any.snip inside the 6.e module';
is (1, 2).can('snip').so, True, 'the 6.d main program sees the 6.e method too';
is (1, 2, 3).snip(* < 2).raku, '((1,), (2, 3)).Seq', 'and can call it';
