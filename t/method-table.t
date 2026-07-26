use Test;

plan 12;

class Base { method inherited() { } }
role Mixed { method from-role() { } }

class Sample is Base does Mixed {
    has $.public-attr;
    has $!private-attr;
    method connect() { }
    method !secret() { }
    submethod BUILD() { }
    multi method overloaded(Int) { }
    multi method overloaded(Str) { }
}

my %table = Sample.^method_table;

isa-ok %table, Hash, '.^method_table returns a Hash';
is %table.keys.sort.join(','), 'connect,from-role,overloaded,public-attr',
    'own methods, role-composed methods and public accessors are listed';

ok %table<connect>:exists, 'a declared method is present';
nok %table<inherited>:exists, 'an inherited method is not in the own table';
nok %table<BUILD>:exists, 'a submethod is not in the method table';
nok %table<secret>:exists, 'a private method is not in the method table';
nok %table<nonesuch>:exists, 'an undeclared name is absent';

isa-ok %table<connect>, Method, 'the value is a Method object';
is %table<connect>.name, 'connect', 'the Method knows its name';
ok %table<overloaded>.is_dispatcher, 'a multi contributes one dispatcher entry';

# `.^submethod_table` is the sibling table and holds what `method_table` drops.
ok Sample.^submethod_table<BUILD>:exists, '.^submethod_table holds the submethod';

# The metamethod works on an instance too, not just on the type object.
ok Sample.new.^method_table<connect>:exists, '.^method_table works on an instance';
