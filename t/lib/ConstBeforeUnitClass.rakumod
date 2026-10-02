# A `constant` written BEFORE `unit class` sits in the compunit mainline
# (GLOBAL), so rakudo leaves it visible to the importer. Dist::META keeps
# `constant %phases-eq` this way and its t/00-sanity.t reads it.
constant %before-hash = a => 1, b => 2;
constant $before-scalar = 5;
constant @before-list = 1, 2, 3;
unit class ConstBeforeUnitClass;
constant $after-scalar = 9;
method after() { $after-scalar }
method before() { %before-hash<a> + $before-scalar + @before-list.elems }
