use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

# `t/lib/AugmentOnceInExport.rakumod` augments `Any` from inside its own
# `sub EXPORT` hook -- `Logic::Ternary`'s real shape (locked as Logic::Ternary
# on the ecosystem-dist-roulette lock board while fixing this). mutsu
# re-invokes `sub EXPORT` on every `use` of an already-loaded module
# (`apply_module_export`'s doc comment: "Raku runs `sub EXPORT` on every
# import, not once per process"), and until this fix it re-ran the `augment`
# statement as an ordinary runtime statement too, so the SECOND `use` died
# with `X::Redeclaration` ("Package 'Any' already has a method
# 'AugmentOnceGreet'") -- every one of Logic::Ternary's five roast files
# `use`s it, and its own export test (`t/04-export.rakutest`) does it eight
# times.
#
# Real Raku elaborates `augment` at COMPILE time of the enclosing code, once,
# however many times that code is later invoked; only the rest of the hook's
# body (here, nothing else) re-runs per `use`. Each `use` below happens
# inside a SEPARATE `EVAL`, matching how `Logic::Ternary`'s own test isolates
# each import -- a plain repeated `use Module;` in one compunit does not
# re-invoke `sub EXPORT` at all if nothing else in that compunit imports
# differently, so the isolation is load-bearing for reproducing the bug.

plan 2;

use lib 't/lib';

is EVAL(q:to/CODE/), 'hi', 'first use: augment applies';
use lib 't/lib';
use AugmentOnceInExport;
Any.AugmentOnceGreet
CODE

is EVAL(q:to/CODE/), 'hi', 'second use: augment is a no-op, not a redeclaration error';
use lib 't/lib';
use AugmentOnceInExport;
Any.AugmentOnceGreet
CODE
