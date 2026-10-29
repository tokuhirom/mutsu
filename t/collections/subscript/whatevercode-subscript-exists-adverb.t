use Test;

# `*<a>:exists` / `*.<a><b>:exists` are WhateverCode over the whole subscript
# and its adverb (annotations' `@c.map(*.meta<provides>{$n}:exists)`, #14907).

plan 7;

my %h = a => {b => 1};
is (%h,).map(*.<a><b>:exists).Str, 'True', 'dot-angle chain with :exists';
is (%h,).map(*<a>:exists).Str, 'True', 'angle subscript with :exists';
is (%h,).map(*.<a><c>:exists).Str, 'False', 'a missing key is False';
is (%h,).map(*<a>:!exists).Str, 'False', ':!exists negates';

class C { method m { {x => 1} } }
is (C.new,).map(*.m<x>:exists).Str, 'True', 'method call result subscripted with :exists';

my @a = [1, 2], [3];
is @a.map(*[1]:exists).Str, 'True False', 'positional subscript with :exists';

my $f = *<k>:exists;
is-deeply (($f)({k => 1}), ($f)({})), (True, False), 'the code object can be called directly';
