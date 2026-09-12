use v6;
use Test;
use lib 't/lib';
use UnitMyConstantCollide;

# An importer's `my @x`/`my %x` reads empty when a used `unit module` has a
# file-scope `my constant @x`/`%x` of the same bare name (#8027).
#
# A module body runs against the loading scope's `env`, so a compunit's own
# `my constant @collide-arr` and the importer's own `my @collide-arr` share
# ONE bare env key. A plain `my` file-scope lexical is protected from this by
# `unit_lexicals` (consulted before `env`), but `my constant` was excluded
# from that store (only a bare `constant`/`our constant` gets a
# package-qualified global that happens to sidestep the collision). So
# calling the module's own accessor from inside a declaration that shadows
# the constant's bare name read back whatever the importer's own
# (not-yet-initialized, empty) declaration currently held — not the
# module's constant.

plan 4;

my @collide-arr = get-arr();
is-deeply @collide-arr, ['a', 'b', 'c'], 'importer @x reads the call result, not its own empty declaration';
is @collide-arr.elems, 3, 'importer @x has all 3 elements';

my %collide-hash = get-hash();
is-deeply %collide-hash.Hash, {a => 1, b => 2}, 'importer %x reads the call result, not its own empty declaration';
is %collide-hash.elems, 2, 'importer %x has both elements';
