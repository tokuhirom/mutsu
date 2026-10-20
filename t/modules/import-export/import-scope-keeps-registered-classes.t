use Test;
use lib 't/lib';

# A block's `use` is lexical: the names it imports leave with the block
# (ADR-8751), and an exported type is a term of the block's pad. The class
# table itself is never rolled back on the block's exit: every class an
# import scope registers is either the loaded module's own or the program's.
# These pin that each kind stays usable, and that the imported names do not.

plan 7;

{ use UseInBlockTypePositions; class BlockOwnClass { method hi { 'own' } } }
is BlockOwnClass.new.hi, 'own', 'a class the block declares outlives it';
nok (try EVAL 'UibtThing'), 'an imported type name does not outlive the block';

my $escaped = do { use UseInBlockTypePositions; UibtThing.new };
is $escaped.who, 'module thing', 'an instance of an imported class keeps working';
is $escaped.^name, 'UseInBlockTypePositions::UibtThing', 'under its qualified name';

{ use ScanCacheHelper; is ScanCacheThing.new.label, 'thing', 'first block sees the import' }
{ use ScanCacheHelper; is ScanCacheThing.new.label, 'thing', 'a second block re-imports it' }

sub minted() {
    use UseInBlockTypePositions;
    my $c := Metamodel::ClassHOW.new_type(name => 'MintedInRoutine');
    $c.^add_parent(UibtThing);
    $c.^compose;
    $c
}
is minted().new.who, 'module thing', 'a type minted in a routine keeps its imported parent';
