use lib 't/lib';
use Test;
use QualifiedEnumExport :Endian;

# An exported enum with a qualified name is reachable by its last name part.
# `Endian` is also the name of the built-in endianness enum, which the import
# must replace (Native::Packing's README spells `Endian::Vax`).

plan 4;

is Endian::Vax.Int, 1, 'Endian::Vax is the imported enum value';
is Endian::Network.Int, 0, 'Endian::Network is the imported enum value';
ok Endian.^name.starts-with('QualifiedEnumExport::'),
    'Endian names the imported type, not the built-in one';
is Vax.Int, 1, 'the bare value is still imported';
