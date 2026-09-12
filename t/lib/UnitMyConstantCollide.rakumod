# Fixture for t/modules/import-export/unit-my-constant-name-collision.t.
#
# A `my constant @x`/`%x` in a `unit module` is readable from the module's own
# routines through a bare env key that the loading scope shares — see
# UnitMyConstant.rakumod. Nothing protected that bare key from a same-named
# `my @x`/`my %x` the IMPORTER declares AFTER `use`ing this module: the
# importer's own fresh (empty) declaration and the module's constant collide
# on the identical bare name, and whichever ran last wins the shared slot.
unit module UnitMyConstantCollide;

my constant @collide-arr = 'a', 'b', 'c';
my constant %collide-hash = (a => 1, b => 2);

our sub get-arr()  is export { @collide-arr }
our sub get-hash() is export { %collide-hash }
