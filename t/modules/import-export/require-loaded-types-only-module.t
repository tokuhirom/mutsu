use v6;
use Test;
use lib 't/lib';

# #17228: `require` of a module that is already loaded and declares only
# types (a role, a class) must not parse and run it again -- the body ran once
# per `require`, ~14 ms each.
plan 4;

use RequireTypesOnly;
is @RequireTypesOnly::LOADS.elems, 1, 'the module body ran once for the use';

for ^5 { require RequireTypesOnly }
is @RequireTypesOnly::LOADS.elems, 1, 'five requires of the loaded module do not rerun its body';

my $name = 'RequireTypesOnly';
for ^3 { require ::($name) }
is @RequireTypesOnly::LOADS.elems, 1, 'a symbolic require does not rerun it either';

isa-ok RequireTypesOnly::Box.new(v => 1), RequireTypesOnly::Box, 'the module still works';
