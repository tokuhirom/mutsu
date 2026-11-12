use Test;

plan 6;

# Assigning through `.AT-KEY` keeps an untyped hash untyped, so it still
# `eqv`s an equal plain Hash; a typed hash keeps its type.

my %x = a => 1;
%x.AT-KEY("b") = 2;
is %x.raku, '{:a(1), :b(2)}', 'untyped hash stays untyped';
ok %x eqv {a => 1, b => 2}, 'and eqv an equal plain hash';
is %x.of.^name, 'Mu', 'element type is unchanged';

my \h = Hash.new;
h.AT-KEY("a") = 1;
is h.raku, '{:a(1)}', 'sigilless hash';

my Int %t;
%t.AT-KEY("k") = 3;
is %t.raku, '(my Int % = :k(3))', 'typed hash keeps its type';
throws-like { %t.AT-KEY("j") = "x" }, X::TypeCheck::Assignment, 'and its element check';
