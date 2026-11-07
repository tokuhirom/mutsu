use v6;
use lib 't/lib-trait-mod-unit-name-mismatch';
use Test;
use Pkg::Other;
use Pkg::Core;

# Three exporters of `multi trait_mod:<is>` candidates: `Test`, `Pkg::Other`
# (declares the unit module it is loaded as) and `Pkg::Core`, which declares
# `unit module Core` (its candidates live in GLOBAL while it loads). Importing
# the last used to drop its candidate, so `is aliased-by` was an unknown trait.
# Found with Intl::CLDR (`use Test; use Intl::CLDR::Core`). Identical under raku.

plan 3;

class Foo {
    has Str $.x is aliased-by<xx> = "x";
}
is Foo.new.xx, "x", "attribute trait from the name-mismatched unit module applies";
my $sigs = &trait_mod:<is>.candidates.map(*.signature.gist).join("\n");
ok $sigs.contains(':$aliased-by!') && $sigs.contains(':$other!') && $sigs.contains(':$test-assertion!'),
    "all three exporters' candidates are in the multi";
lives-ok { EVAL 'class Baz { has Int $.m is aliased-by<mm> = 1 }' }, "also from EVAL";
