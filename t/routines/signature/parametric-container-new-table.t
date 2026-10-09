use v6;
use Test;

# Parametric `.new` of Array and Hash is built by the native_ctor table only
# (there is no second parametric branch below it).
plan 8;

is Array[Int].new(1, 2).raku, 'Array[Int].new(1, 2)', 'Array[Int].new';
is Array[Int].new(1, 2).of.raku, 'Int', 'element type is tagged';
is Hash[Int, Str].new("a", 1).raku, '(my Int %{Str} = :a(1))', 'Hash[Int,Str].new';
is Hash[Int, Str].new.keyof.raku, 'Str', 'key type is tagged';
is Array[Int].new(:shape(2, 2)).shape, (2, 2), 'shaped Array[Int].new';
is array[int].new(1, 2).raku, 'array[int].new(1, 2)', 'array[int].new';
is (role R[::T] { has T $.x }).^name, 'R', 'role declared';
is R[Int].new(x => 3).x, 3, 'parametric role pun';
