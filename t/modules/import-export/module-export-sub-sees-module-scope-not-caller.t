use v6;
use Test;

# A module's custom `sub EXPORT` is part of the module's own closure, so a
# bareword term inside it (e.g. NativeLibs' `Map.new('NativeCall' => NativeCall,
# ...)`) must resolve against what the module's OWN mainline could see, not
# against the (already-restored) importing scope. mutsu ran EXPORT against the
# caller's env instead: by the time EXPORT ran, `NativeCall` had already been
# stripped back out of `env` (owned by no one but the module's own transitive
# `use NativeCall;`), so the bareword silently degraded to the plain string
# "NativeCall" -- which then shadowed the real package for every importer.
#
# Reproduces the "independent gap" from
# https://github.com/tokuhirom/mutsu/issues/7806 with no EVAL involved at all
# (a plain `use`, matching NativeLibs' actual shape: `use NativeCall;` then a
# custom `sub EXPORT` referencing it, with `unit module` declared afterward).

plan 3;

use lib 't/lib/Issue7806';
use Issue7806Export;

my \exported = ::('NativeCall');
isnt exported.^name, 'Str',
    "the module's custom EXPORT map passes a real package through, not its stringified name";
is exported.^name, 'NativeCall',
    "::('NativeCall') resolves to the NativeCall package itself";
ok ::('NativeCall::EXPORT::ALL') !~~ Failure,
    "the transitively-used module's own EXPORT::ALL stash is still reachable";
