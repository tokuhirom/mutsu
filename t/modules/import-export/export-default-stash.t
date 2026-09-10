use Test;

# Raku models a used module's exported symbols as a real nested package,
# `Mod::EXPORT::DEFAULT`, reachable like any other package -- including through
# symbolic lookup, and callable through it.
#
# `todo/deep/export-default-package-not-symbolically-navigable.md` said mutsu
# "never materializes the `ModuleName::EXPORT::DEFAULT` package itself".
# Measured 2026-09-06, that was only true of `Test`, which was then a NATIVE
# provider -- it ran no `is export` declarations, so nothing populated
# `exported_subs` for it. That provider was retired in #7566 and `use Test`
# loads rakudo's own module now, so `Test` reaches this the same way every
# source module always did.
#
# Every row measured against raku v2026.07; this file passes verbatim there too.

plan 9;

# `Test` -- the ticket's own repro.
{
    ok ::("Test::EXPORT::DEFAULT::&ok").defined,
        'Test exposes its exports through EXPORT::DEFAULT';
    is ::("Test::EXPORT").WHO.keys.sort, ('ALL', 'DEFAULT'),
        'and its EXPORT package lists the tags';
    ok ::("Test::EXPORT::DEFAULT").WHO<&ok>.defined,
        'the tag stash carries the routine, not just its name';
}

# The resolved value is the routine itself, not a placeholder.
{
    is ::("Test::EXPORT::DEFAULT::&ok").name, 'ok',
        'the routine reached through the stash is the real one';
}

# A module loaded from source -- unaffected, and pinned because the ticket
# claimed it was broken too.
use lib 't/lib';
use ExportStashMod;
is ::("ExportStashMod::EXPORT").WHO.keys.sort, ('ALL', 'DEFAULT', 'extra'),
    'a source module lists every export tag';
is ::("ExportStashMod::EXPORT::DEFAULT").WHO.keys.sort, ('&greet',),
    'its DEFAULT tag holds only the untagged exports';
ok ::("ExportStashMod::EXPORT::DEFAULT::&greet").defined,
    'and the symbol resolves through the qualified path';
is ::("ExportStashMod::EXPORT::DEFAULT::&greet")(), 'hi',
    'and is callable through it';
is ExportStashMod.WHO.keys.sort, ('EXPORT',),
    "and the module's own stash lists EXPORT";
