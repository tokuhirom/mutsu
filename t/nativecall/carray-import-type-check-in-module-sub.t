use Test;
use lib $*PROGRAM.parent(2).add('lib');
use CArrayImport::Child;

# From Gnome::Glib (Gnome::Glib::Variant.new(:parse(...))): `CArray` imported
# by `use NativeCall` is held in a container cell once a routine of the module
# has used `CArray[Str].new`, and `my CArray[T] $x` then wrongly died with
# "Type 'CArray[T]' is not declared".
plan 1;
is CArrayImport::Child.new.n, 1, 'my CArray[ImportedClass] in a module sub type-checks';
