use Test;

# #14539 (CompUnit::Util t/01-utils.rakutest): `$*REPO.need` of a unit a `use`
# already loaded hands back one compunit without running its body again, also
# when the `need` runs inside a routine.
use NativeCall;

plan 4;

sub load(Str:D $short-name) {
    $*REPO.need(CompUnit::DependencySpecification.new(:$short-name));
}

my $first;
lives-ok { $first = load('NativeCall') }, 'need of a unit already loaded by use';
isa-ok $first, CompUnit, 'it answers a CompUnit';
ok $first === load('NativeCall'), 'a second need in another call answers the same compunit';
is $first.short-name, 'NativeCall', 'with the unit short-name';
