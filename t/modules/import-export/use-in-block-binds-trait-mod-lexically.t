use Test;

# ADR-8751 stage 2 (#8751): a block-scoped `use` of a module that exports
# `multi trait_mod:<is>` candidates binds the family in the block's lexical
# import set, as for any other imported multi family, instead of aliasing the
# candidates into the routine registry for the block's dynamic extent.
#
# The traits apply to every declaration in the block (routine, variable,
# attribute), the core traits keep working beside them, and a routine whose
# body has such a `use` works on every call. That `use` re-runs on every call:
# before this, each run re-inserted the candidates into the registry and the
# routine's exit took them out again, flushing every routine-resolution cache
# per call. After the block the import is gone and the program's own candidate
# is back. Every expectation was checked against rakudo.

plan 9;

use lib 't/lib';

multi sub trait_mod:<is>(Routine:D $r, :$btml-own!) { $r.wrap(-> |c { 'own ' ~ callsame }) }

{
    use BlockTraitModLexical;
    sub btml-sub is btml-tagged { 'sub' }
    is btml-sub(), 'tagged sub', 'an imported trait applies to a routine';
    my $btml-var is btml-tagged;
    is $btml-var, 'tagged var', 'an imported trait applies to a variable';
    class BtmlClass { has $.btml-attr is btml-tagged }
    is BtmlClass.^attributes[0].btml-tag, 'tagged attr', 'an imported trait applies to an attribute';
    sub btml-rw($x is rw) { $x = 5 }
    my $y = 1;
    btml-rw($y);
    is $y, 5, 'core traits still apply in the block';
}

# A routine whose body `use`s the module, called many times, interleaved with
# plain calls that must keep resolving.
sub btml-mk($n) {
    use BlockTraitModLexical;
    my $v is btml-tagged;
    sub btml-inner is btml-tagged { $n }
    "$v/{btml-inner()}"
}
sub btml-plain($x) { $x + 1 }
my @got;
my $acc = 0;
for ^60 { @got.push: btml-mk($_); $acc = btml-plain($acc) }
is @got[0], 'tagged var/tagged 0', 'the importing routine applies the traits on its first call';
is @got[59], 'tagged var/tagged 59', '... and on its sixtieth';
is $acc, 60, 'plain calls interleaved with the importing routine resolve';

# After the block, the program's own candidate applies and the imported one
# is not there.
sub btml-after is btml-own { 'after' }
is btml-after(), 'own after', "the program's own trait candidate applies after the block";
throws-like q[sub btml-late is btml-tagged { }], Exception,
    'the imported trait is not visible after the block';
