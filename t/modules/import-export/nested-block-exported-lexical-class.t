use Test;

# A lexical exported class declared in a nested block of a `unit module` is
# exported like a top-level one (the export-marker scan walks the whole unit,
# ADR-0137), so the importer sees the class rather than a bareword.

use lib 't/lib';
use NestedExportedLexicalClass;

plan 2;

is NestedExported.^name, 'NestedExportedLexicalClass::NestedExported',
    'the nested exported class is visible';
is NestedExported.hi, 'hi from nested', 'and its methods are callable';
