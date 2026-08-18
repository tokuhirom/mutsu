use Test;

plan 8;

# A `proto method`/`proto submethod` with no candidates yet (`proto method
# bar {*}` alone, before any `multi method bar(...)` is declared) had no
# `MethodEntry.proto`-consulting fallback in `.^lookup`/`.^find_method`/
# `.can` -- only real candidates (`user_method_overloads`) were checked, so
# a bare proto was invisible to introspection even though real Raku reports
# it as a defined method. See
# news/2026-08/proto-method-visible-to-find-method.md.

class Foo {
    proto method bar {*}
}

ok Foo.^find_method('bar').defined, '.^find_method sees a zero-candidate proto';
ok Foo.^lookup('bar').defined, '.^lookup sees a zero-candidate proto';
is-deeply Foo.can('bar').elems, 1, '.can sees a zero-candidate proto';

class Sub1 is Foo { }
ok Sub1.^find_method('bar').defined, 'a subclass still sees the inherited proto';

# A proto WITH real candidates keeps working exactly as before (regression
# guard for the normal path this fix must not touch).
class Bar {
    proto method baz($x) {*}
    multi method baz(Int $x) { "int:$x" }
    multi method baz(Str $x) { "str:$x" }
}
is Bar.new.baz(5), 'int:5', 'dispatch through a proto with real candidates: Int';
is Bar.new.baz('hi'), 'str:hi', 'dispatch through a proto with real candidates: Str';
ok Bar.^find_method('baz').defined, '.^find_method sees a proto with candidates';

# A completely undeclared method name is still Nil (regression guard: the
# fix must not make every lookup succeed).
class Baz { }
nok Baz.^find_method('nope').defined, 'an undeclared method name stays undefined';
