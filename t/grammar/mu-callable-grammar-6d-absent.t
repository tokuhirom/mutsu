use Test;

# Under 6.d none of the 6.e additions exist.
plan 4;

nok 42.^can("Callable"), 'Mu.Callable is absent';
grammar G { token TOP { a } }
is G.^can("typed_exception").elems, 0, 'typed_exception is absent';
is G.^can("SETFAIL").elems, 0, 'SETFAIL is absent';
is G.^can("locprepost").elems, 0, 'locprepost is absent';
