use lib 't/lib';
use Test;
use UnitClassUseBeforeDeclOperator::Consumer;

# Raku makes a file-level `use` visible to the whole compilation unit even
# when it appears before a later `unit class` declaration. This is the same
# shape used by Arithmetic::PaperAndPencil, whose methods call operators
# exported by its Number module. The importer package must therefore be the
# unit class, not GLOBAL, while the first `use` is running.

plan 1;

is UnitClassUseBeforeDeclOperator::Consumer.new.apply, 12,
    'a unit class method can call an operator imported before the unit declaration';
