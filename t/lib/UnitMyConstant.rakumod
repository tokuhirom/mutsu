# Fixture for t/modules/unit-my-constant-visible-to-own-routines.t.
#
# A `unit` compunit's file-scope `constant`s are package symbols of the
# compunit, not names the importer sees bare — but the compunit's OWN routines
# must still read them. Both spellings (`my constant` and a bare `constant`)
# have to behave identically; mutsu used to divert the `my constant` spelling
# into the per-compunit lexical store, taking it out of the store its own
# routines read and making every such read answer `Nil`.
unit module UnitMyConstant;

my constant @MY-ARRAY  = 10, 20, 30;
my constant %MY-HASH   = (a => 1, b => 2);
my constant $MY-SCALAR = 99;
my constant MY-BARE    = 'sigilless';

constant @BARE-ARRAY  = 40, 50, 60;
constant %BARE-HASH   = (c => 3);
constant $BARE-SCALAR = 77;

our sub my-array()   is export { @MY-ARRAY }
our sub my-hash()    is export { %MY-HASH }
our sub my-scalar()  is export { $MY-SCALAR }
our sub my-bare()    is export { MY-BARE }

our sub bare-array()  is export { @BARE-ARRAY }
our sub bare-hash()   is export { %BARE-HASH }
our sub bare-scalar() is export { $BARE-SCALAR }
