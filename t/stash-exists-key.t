use v6;
use Test;

# A package Stash supports `.EXISTS-KEY` (the Associative membership method).
# Regression pin for the head-skip-tail dist, whose EXPORT sub does
# `CORE::.EXISTS-KEY('&head')` to detect a symbol clash before exporting —
# previously died with "No such method 'EXISTS-KEY' for invocant of type 'Stash'".

module Foo {
    our sub bar() { 42 }
    our $baz = 5;
}

ok Foo::.EXISTS-KEY('&bar'), 'EXISTS-KEY finds an our-scoped sub symbol';
ok Foo::.EXISTS-KEY('$baz'), 'EXISTS-KEY finds an our-scoped scalar symbol';
nok Foo::.EXISTS-KEY('&nope'), 'EXISTS-KEY is False for a missing symbol';

# Unlike AT-KEY, EXISTS-KEY does NOT sigil-fallback: the bare name is a distinct
# (absent) key from the scalar-sigil symbol.
nok Foo::.EXISTS-KEY('baz'), 'EXISTS-KEY does not sigil-fallback (bare name absent)';

# The CORE pseudo-stash answers EXISTS-KEY for a symbol it does not have.
nok CORE::.EXISTS-KEY('&this_symbol_does_not_exist_xyz'),
    'CORE::.EXISTS-KEY is False for an unknown symbol';

done-testing;
