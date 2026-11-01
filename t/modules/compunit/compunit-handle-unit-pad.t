use Test;
use lib $?FILE.IO.parent.parent.parent.add("lib");
use nqp;

# CompUnit::Handle.unit is the unit's lexical pad: nqp::existskey, nqp::atkey and
# nqp::iterator take it, and CompUnit answers it too (it is its handle's pad).

plan 9;

need CompUnitHandleUnitDep;
my $cu = $*REPO.need(CompUnit::DependencySpecification.new(:short-name<CompUnitHandleUnitDep>));
my $handle = $cu.handle;

isa-ok $handle, CompUnit::Handle, 'the unit has a handle';
my $unit := $handle.unit;
ok nqp::existskey($unit, '$=pod'), 'the pad has the unit\'s Pod';
ok nqp::existskey($unit, '$?PACKAGE'), 'the pad has $?PACKAGE';
nok nqp::existskey($unit, 'nothing-here'), 'a missing key is absent';
ok nqp::atkey($unit, '$=pod')[0] ~~ Pod::Block, 'the Pod entry is a Pod::Block';
ok nqp::atkey($handle.unit, '$=pod')[0] === nqp::atkey($handle.unit, '$=pod')[0],
    'two asks answer the same Pod objects';
ok nqp::existskey($cu.unit, '$=pod'), 'CompUnit.unit is its handle\'s pad';

my Mu $iter := nqp::iterator($handle.unit);
my @keys;
while $iter {
    my $i := nqp::shift($iter);
    @keys.push(nqp::iterkey_s($i));
}
ok '$=pod' ∈ @keys, 'the pad iterates with nqp::iterator';
ok '$?PACKAGE' ∈ @keys, 'and lists every entry';
