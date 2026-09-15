use v6;
use Test;

# A module that computes its exports through a run-time `sub EXPORT` hook has
# no `is export` traits for the static scan to find (ADR-0087). The parser
# approximates a routine's export with the module's own unit-scope routines,
# but that leaves a second idiom uncovered: `t/lib/ExportHookValueTerm.rakumod`
# declares its exports LOCALLY inside the hook's own body (French/lizmat's
# "hand-built Map" shape) rather than drawing them from `UNIT::`, and one of
# them -- `my \vrai = True;` -- is a plain VALUE term, not a routine.
#
# Without registering it, `vrai` was an unknown bareword to this file's parse.
# An unknown bareword defaults to a listop-call head, so `vrai et 2` misparsed
# as `vrai(et, 2)` and died evaluating `et` as if it were a zero-arg call
# ("Unknown function: et") -- the ecosystem `French` distribution's whole
# operator suite (12/30 assertions) hit exactly this, via `vrai et vrai`.
#
# https://github.com/tokuhirom/mutsu/issues/7884 (locked as French while
# fixing this)

plan 4;

use lib 't/lib';
use ExportHookValueTerm;

ok (vrai et vrai), 'a value term declared LOCALLY inside the EXPORT hook parses as a bare term';
ok (vrai ou faux), 'ou operator, same shape';
nok (vrai et faux), 'et operator answers the right value, not just "parses"';

# CONTROL: a later, locally-declared term of the same name still shadows the
# imported one for the rest of this file — the two registries (`term_symbols`
# for a local `my \x`, `imported_value_terms` for this fix's new one) must
# stay genuinely separate rather than one clobbering the other.
{
    my \vrai = 42;
    is (vrai + 1), 43, 'a local re-declaration of the same term name still wins';
}
