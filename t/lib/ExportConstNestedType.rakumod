# Syndicate's shape: a unit class exports a `my constant Atom` while a nested
# class `ExportConstNestedType::Atom` (bound under the same qualified key)
# and a re-exported `our constant Atom` are also in play.
use ExportConstNestedType::Fmt;
use ExportConstNestedType::Atom;
use ExportConstNestedType::Parse;
unit class ExportConstNestedType;
my constant Atom is export = ExportConstNestedType::Fmt::FF::Atom;
my constant RSS2 is export = ExportConstNestedType::Fmt::FF::RSS2;
