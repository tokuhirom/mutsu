use Test;

# A typed class-body `my` static keeps its own class's type: a later class
# declaring a same-named static of another type does not impose that type on
# the first class's methods, and the type does not leak out of the class body
# onto a same-named lexical of the enclosing scope (#10524).

plan 9;

class First {
    my Int $count;
    method bump() { $count = ($count // 0) + 1; $count }
    method set($v) { $count = $v }
}
class Second {
    my Str $count;
    method get() { $count }
    method set($v) { $count = $v }
}

is First.bump, 1, 'a method stores its own class\'s type into its static';
is First.bump, 2, 'and again';
throws-like { First.set('x') }, X::TypeCheck::Assignment,
    'the static still rejects a value of the wrong type';
lives-ok { Second.set('y') }, 'the other class\'s static takes its own type';
is Second.get, 'y', 'and keeps the value';
throws-like { Second.set(3) }, X::TypeCheck::Assignment,
    'and rejects the first class\'s type';

class Singleton {
    my Singleton $instance;
    method instance() { $instance //= Singleton.new; $instance }
}
class OtherSingleton {
    my OtherSingleton $instance;
    method instance() { $instance //= OtherSingleton.bless; $instance }
}
isa-ok Singleton.instance, Singleton, 'a singleton accessor after a same-named static';

my $count = 'outer';
$count = 42;
is $count, 42, 'an untyped outer lexical is not constrained by a class static';

my Str $typed = 'a';
class Third { my Int $typed; method t() { $typed = 1 } }
Third.t;
throws-like { $typed = 2 }, X::TypeCheck::Assignment,
    'the outer lexical keeps its own type past the class body';

# The prologue registers every class above ahead of the run-time code.
BEGIN { 1 }
