use Test;

plan 8;

# #8836: `.^private_method_table` was entirely unimplemented
# ("No such method 'private_method_table' for invocant of type
# 'Perl6::Metamodel::ClassHOW'"). AttrX::Lazy uses it to verify a lazy
# attribute's builder method actually exists before installing the
# accessor.

class Base { method !inherited-private() { } }
role Mixed { method from-role() { } }

class Sample is Base does Mixed {
    has $.public-attr;
    method connect() { }
    method !secret() { }
    submethod !secret-submethod() { }
    submethod BUILD() { }
}

my %table = Sample.^private_method_table;

isa-ok %table, Hash, '.^private_method_table returns a Hash';
is %table.keys.sort.join(','), 'secret,secret-submethod',
    'own private methods and private submethods are listed, by their bare name';

ok %table<secret>:exists, 'a private method is present';
ok %table<secret-submethod>:exists, 'a private submethod is present too';
nok %table<connect>:exists, 'a public method is not in the private table';
nok %table<public-attr>:exists, 'a public attribute accessor is not in the private table';
nok %table<inherited-private>:exists,
    'an ancestor\'s private method is not in the own table';
nok %table<from-role>:exists,
    'a public role-composed method is not in the private table';
