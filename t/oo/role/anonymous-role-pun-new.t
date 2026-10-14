use Test;

plan 2;

is (my role { has $.z = 3 }).new.z, 3,
    'an anonymous role declaration value can be punned with .new';
is (my role NamedHere { has $.z = 4 }).new.z, 4,
    'a named role declaration value can be punned with .new';
