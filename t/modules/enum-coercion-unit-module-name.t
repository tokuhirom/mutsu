use Test;
use lib 't/lib';
use EnumUnitNameMismatch;

# An exported enum of a module whose `unit module` name differs from its file
# name is still callable as a coercion (`Errno(2)`) in the importer (#16041,
# found via Universal::Errno).
plan 4;

is Errno16041(2).raku, 'Errno16041::ENOENT16041', 'coercion call by value';
is Errno16041(1).raku, 'Errno16041::EPERM16041', 'coercion call, first value';
is Errno16041::ENOENT16041.raku, 'Errno16041::ENOENT16041', 'qualified variant';
is Errno16041.^enum_values.elems, 2, 'type object introspection';
