use Test;

# An `is export` subset from a `use`d module is a type name in the importer, so
# `when Bin { ... }` must not parse as a routine call that gobbled the block
# ("needs parens to avoid gobbling block"). Came from the FHIR distribution
# (`subset Base64Binary of Buf is export` in FHIR::Base, matched in
# FHIR::JsonSerdes). The module scan ignored `subset` declarations.

plan 4;

use lib 't/lib';
use ExportedSubsetWhen;

sub classify($x) {
    given $x {
        when Bin   { "bin" }
        when Small { "small" }
        default    { "other" }
    }
}

is classify(Buf.new(1)), "bin", "exported Buf subset matches in when";
is classify(5), "small", "exported where-subset matches in when";
is classify(50), "other", "non-member falls through";
is classify("a"), "other", "other type falls through";
