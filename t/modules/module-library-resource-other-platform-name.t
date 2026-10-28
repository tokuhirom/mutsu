use Test;
use lib $*PROGRAM.parent(2).add('fixtures/library-resource-dll/lib');
use LibResDll;

# Distribution: PDF::Native, which ships resources/libraries/pdf.dll and reads
# `%?RESOURCES<libraries/pdf>` from an uninstalled checkout.
plan 1;

is library-file(), 'fixture.dll',
    'a libraries/ resource resolves to the library file the directory ships, whatever its platform naming';
