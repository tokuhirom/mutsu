use lib 't/lib';
use Test;

# #10244: in a *parameterized* role body, a `my class ... is export` and a
# `my constant ... is export` that do not mention the role's type parameters
# are compile-time declarations, importable without any class ever composing
# (or even parameterizing) the role.
use ParamRoleBodyExportedDecls;

plan 7;

is Foo.^name, 'ParamRoleBodyExportedDecls::Foo', 'exported my class from a parameterized role body is imported';
is X, 5, 'exported my constant from a parameterized role body is imported';
ok Foo.new ~~ Foo, 'the imported class is usable';
# #10441: a string literal spelling a parameter's name is data, not a mention
# of the parameter, so the constant is still declared eagerly.
is Y, 'K', 'exported my constant whose value spells the parameter name is imported';

# A parameter-free nested class is one class shared by every parameterization,
# as in Rakudo.
role R[::T] { my class C { method hi { 'hi' } }; method c { C } }
is R[Int].c.^name, 'R::C', 'parameter-free nested class keeps its plain name';
ok R[Int].c === R[Str].c, 'parameter-free nested class is shared across parameterizations';
is R[Int].c.hi, 'hi', 'the shared nested class is usable';

