use Test;

plan 4;

# A package-scoped enum named by its short name as a parameter type.
# Found via Protocol::Postgres (ecosystem).
module Foo {
    enum Format <Text Binary>;
    our sub val(Format $x) { 'val' }
    our sub obj(Format) { 'obj' }
    multi mt(Int:U) { 'int' }
    multi mt(Format) { 'fmt' }
    our sub run-mt() { mt(Format) }
}
is Foo::val(Foo::Text), 'val', 'enum value binds a short-named parameter type';
is Foo::obj(Foo::Format), 'obj', 'enum type object binds a short-named parameter type';
is Foo::run-mt(), 'fmt', 'multi dispatches on the enum type object';
ok Foo::Text ~~ Foo::Format, 'smartmatch still works';
