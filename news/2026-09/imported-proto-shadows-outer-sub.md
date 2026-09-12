# Imported proto families shadow enclosing routines

mutsu now gives a block-scoped import of a proto/multi routine family the
same lexical shadowing behavior as Rakudo. Imported candidates no longer merge
with an enclosing same-named my sub and recurse into it; the enclosing family
is restored when the import scope exits.
