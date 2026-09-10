use Test;

plan 9;

# rakudo installs a package symbol when the compunit is COMPILED, so an `our`
# declaration's slot exists (undefined) even in a branch that never runs.

if False { our $o = 4 }
is OUR::<$o>.^name, 'Any', 'a dead-branch our is installed, undefined';

if False { our @a; our %h }
is OUR::<@a>.elems, 0, 'a dead-branch our array is an empty container';
is OUR::<%h>.elems, 0, 'a dead-branch our hash is an empty container';

# Reached declarations are unaffected: the assignment overwrites the seed.
if True { our $run = 9 }
is OUR::<$run>, 9, 'a reached our still holds its assigned value';
our $top = 'x';
is OUR::<$top>, 'x', 'a top-level our is unaffected';

# The declaring package decides where the symbol lands.
class Foo { if False { our $c = 1 } }
is Foo::<$c>.^name, 'Any', 'a dead-branch our in a class lands in that class';

module M { if False { our $m = 1 } }
is M::<$m>.^name, 'Any', 'a dead-branch our in a module lands in that module';

class Bar { our $b = 3 }
is Bar::<$b>, 3, 'a reached our in a class is unaffected';

sub never() { if False { our $s = 1 } }
is OUR::<$s>.^name, 'Any', 'a dead-branch our inside an uncalled sub lands in its enclosing package';
