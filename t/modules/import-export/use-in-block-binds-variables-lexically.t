use Test;

# A `use` in a block installs the variables it imports in the block's
# lexical pad (ADR-8751): there the import hides an outer variable of the
# same name, a closure taken in the block keeps it, and a routine written
# outside the block does not see it. The binding is the module's own `our`
# variable, so a write through either name is seen through the other.
# Checked against rakudo.

plan 16;

use lib 't/lib';

our $uibv-scalar = 'outer';
my @uibv-list = <o>;
my %uibv-hash = k => 'outer';
sub outer-reader { $uibv-scalar }

my &kept;
{
    use UseInBlockVars;
    is $uibv-scalar, 'module', 'the import hides an outer our variable in the block';
    is @uibv-list.join(','), 'a,b', 'and an outer my array';
    is outer-reader(), 'outer', 'a routine written outside the block does not see it';
    &kept = -> { $uibv-scalar };
    $uibv-scalar = 'written';
    is $UseInBlockVars::uibv-scalar, 'written', 'a write in the block is the module variable';
    uibv-bump();
    is $uibv-scalar, 'bumped', 'and so is a write the module makes';
    is @uibv-list.join(','), 'a,b,c', 'an array is the module array';
}
is kept(), 'bumped', 'a closure taken in the block keeps the import';
is $uibv-scalar, 'outer', 'the outer our variable is untouched after the block';
is @uibv-list.join(','), 'o', 'and so is the outer array';

{
    use UseInBlockVars :hash;
    is %uibv-hash<k>, 'v', 'a tag imports the variables it names';
    %uibv-hash<added> = 1;
    is $uibv-scalar, 'outer', 'and not the others';
    is $uibv-mandatory, 'm', 'a MANDATORY variable comes with any tag';
}
is %uibv-hash.keys.sort.join(','), 'k', 'an element stored in the block went to the module hash, not the outer one';

sub in-routine($n) { use UseInBlockVars; "$uibv-scalar/$n" }
is (1..3).map({ in-routine($_) }).join(','), 'bumped/1,bumped/2,bumped/3',
    'a routine body imports on every call';

for 1..2 { use UseInBlockVars; @uibv-list.push("l$_") }
is @uibv-list.join(','), 'o', 'a loop body import leaves the outer array alone';
is (do { use UseInBlockVars; @uibv-list.elems }), 5, 'the loop body pushed onto the module array';
