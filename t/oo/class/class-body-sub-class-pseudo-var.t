use Test;

# `$?CLASS` in a `sub` declared in a class body is that class, fixed at compile
# time. It used to read whatever class body had run last, so a later
# `class Other {}` (or a `use`d module's classes) leaked into it (Crypt::RC4's
# `our sub RC4($key, |c) { $?CLASS.new(:$key).RC4(|c) }`).

plan 6;

class Foo {
    our sub made() { $?CLASS }
    sub inner() { $?CLASS }
    method via-sub() { inner() }
    our sub closure() { -> { $?CLASS }() }
}

class Other { }

is Foo::made().^name, 'Foo', 'an our sub sees its own class';
is Foo.via-sub.^name, 'Foo', 'a lexical sub called from a method';
is Foo::closure().^name, 'Foo', 'a closure inside a class-body sub';

class Outer {
    class Inner {
        our sub who() { $?CLASS }
    }
    our sub who() { $?CLASS }
}
is Outer::Inner::who().^name, 'Outer::Inner', 'a nested class has its own';
is Outer::who().^name, 'Outer', 'and the outer class keeps its own';

role R {
    method who() { $?CLASS }
}
class Doer does R { }
is Doer.who.^name, 'Doer', 'a role method still sees the consuming class';
