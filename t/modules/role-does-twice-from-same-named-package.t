use v6;
use lib 't/lib';
use Test;
use DoesTwiceApplier;

plan 3;

# Mixing a parameterized role into an object that already is a mixin type
# (`Attribute+{R[Block]}`) keeps that type as the new type's base. Its name
# was read as a role application and qualified by the package running the
# `does` (`DoesTwiceApplier::Rel[Block]...`), then parsed as source: Red
# t/017's `require` of a module whose models were already loaded died with
# "Confused" at the program's first line.
class Holder does DoesTwiceApplier::Rel {}
my $attr = Attribute.new(:name<$!x>, :type(Int), :package(Any));
Holder.apply($attr, "a");
is $attr.rel-name, "a", 'the first mixin';
Holder.apply($attr, "b");
is $attr.rel-name, "b", 'the second mixin over the first';
is $attr.^name, 'Attribute+{DoesTwice::Rel[Block]}+{DoesTwice::Rel[Block]}', 'named as rakudo names it';
