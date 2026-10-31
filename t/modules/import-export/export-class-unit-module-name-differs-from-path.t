use v6;
use lib 't/lib';
use Test;

# From Qt::QtWidgets: `use Qt::QtWidgets` loads a file whose `unit module` is
# named `QtWidgets:ver<..>:auth<..>:api<..>`. Its `is export` classes must
# still reach the importer (the version adverbs lower to a synthetic block
# around the package, which hid the declared unit name).

plan 4;

use UnitNameMismatchFile;

is MismatchObj.new.hi, 'obj', 'an exported class is imported under its short name';
is MismatchRole.^name, 'DifferentlyNamed::MismatchRole', 'and an exported role';
is mismatch-helper(), 'helped', 'an exported sub still is';
class Child is MismatchObj { }
is Child.new.hi, 'obj', 'the imported class can be inherited from';
