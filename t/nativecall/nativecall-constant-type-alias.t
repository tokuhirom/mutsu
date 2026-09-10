use Test;
use lib 't/lib';
use NCTypeAliasMod;

# `constant my_bool = int8;` must be followed to the type it names when a
# native signature uses it. Before this, an aliased parameter/return type was
# unmappable, the whole declaration skipped native registration, and the call
# hit the stub `{ * }` body: "No such method 'free' for invocant of type 'Mem'".

plan 6;

my $m = alloc(64);
ok $m.defined, 'a native sub with an aliased (chained) parameter type is callable';
ok $m.malloc_usable_size >= 64, 'an aliased return type marshals the C result';
isa-ok $m.malloc_usable_size, Int, 'the aliased return type yields a value, not a type object';

lives-ok { $m.free }, 'a native method with an aliased return type is registered';

is memcmp('abc', 'abc', 3), 0, 'an aliased parameter type on a plain sub marshals';
ok memcmp('abc', 'abd', 3) != 0, 'and it really passes the length through';
