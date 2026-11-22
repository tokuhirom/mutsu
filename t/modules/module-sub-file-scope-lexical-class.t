use lib 't/lib';
use Test;
use ModuleSubFileScopeLexClass;

# A module routine's bare name of the module's own file-scope `my class` /
# `my role` is that type, even when the program that loaded the module
# declares a type of the same short name (#18677).

plan 5;

class R { method who { 'program R' } }
role Ro { method role-who { 'program Ro' } }

is make-r().who, 'module R', 'the module routine builds its own class';
is r-name(), 'ModuleSubFileScopeLexClass::R', 'and names it';
is mixed().role-who, 'module Ro', 'its own role too';
is R.new.who, 'program R', 'the program keeps its own class';
is (42 but Ro).role-who, 'program Ro', 'and its own role';
