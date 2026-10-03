use Test;
use lib 't/lib';
use ExportConstNestedType;

plan 3;

# An exported `my constant Atom` of `unit class ExportConstNestedType` is
# imported as the constant, not as the nested class
# `ExportConstNestedType::Atom` bound under the same qualified key.
is Atom.raku, 'FF::Atom', 'the exported constant, not the nested class';
is RSS2.raku, 'FF::RSS2', 'a sibling constant with no clashing type';
ok Atom ~~ ExportConstNestedType::Fmt::FF, 'it is the enum value';
