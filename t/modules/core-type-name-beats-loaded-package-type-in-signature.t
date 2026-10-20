use Test;
use lib $*PROGRAM.parent(2).add('lib-core-type-vs-pkg');

# From the AttrX::Mooish distribution: `unit module AttrX::Mooish` does
# `use AttrX::Mooish::Attribute` and declares `trait_mod:<is>(Attribute:D $attr,
# :$mooish!)`. The signature's `Attribute` was qualified to the loaded role, so
# `has $.x is mooish` died with "Can't use unknown trait 'is' -> 'mooish'".
plan 3;

use Pk::Mod;
my class Foo { has $.bar is tagged; }
pass 'the trait is found on an Attribute';
is Foo.^attributes.head.marker, 'role marker', 'the trait ran on the core Attribute';
is &trait_mod:<is>.candidates.map(*.signature.gist).grep(*.contains('tagged')).head,
    '(Attribute:D $attr, :$tagged!)', 'the signature names the core type';
