use Test;

# A module whose BEGIN merges another unit's `EXPORTHOW` into its own (a call
# of a routine it imported, CompUnit::Util's `re-exporthow`) hands its
# importers that unit's package declarators, which the importer's parse must
# know (#17557). Checked against rakudo.

plan 5;

use lib $?FILE.IO.parent(2).sibling('lib').Str;

{
    use ExportHowReexporter;
    my critter Pikachu { method hi { 'pika' } }
    is Pikachu.hi, 'pika', 'a declarator merged at BEGIN declares a type';
    ok Pikachu.HOW ~~ Metamodel::ClassHOW, 'with the merged meta-object';
}

{
    use ExportHowReexporter2;
    lives-ok { my critter Raichu { } }, 'another re-export of it in another block';
}

use MONKEY-SEE-NO-EVAL;
throws-like q|use ExportHowReexporter; use ExportHowReexporter2;|, X::EXPORTHOW::Conflict,
    'two imports of the declarator into one scope conflict';
throws-like q|{ use ExportHowReexporter; }; my critter Pichu { }|, Exception,
    'the declarator ends with the block of its use';
