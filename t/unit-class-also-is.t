use Test;

# `also is Parent` / `also is rw` inside a `unit class Foo;` must be extracted
# into the class's parent list / rw flag, exactly as a `class Foo { ... }` block
# does. Previously the unit-class form left `also is Parent` as a bare
# `is(also, Parent)` infix expression in the body, which executed as a runtime
# "two terms in a row". Regression: roast/integration/diamond.t.

plan 8;

# unit class via EVAL (a fresh compilation unit) exercises the file-scope
# `unit class Foo; also is Parent;` path directly.
class Base { method greet { "hi" } }
my $derived = EVAL q:to/CODE/;
    unit class Derived;
    also is Base;
    method extra { "x" }
    CODE
is $derived.^name, "Derived", "unit class with also-is registers";
is $derived.new.greet, "hi", "unit class also is Parent inherits the parent method";
is $derived.new.extra, "x", "unit class own method works alongside also is";

# A diamond unit class declared at file scope is exercised by the diamond roast
# test; here we also verify the block-equivalent behaviours.
class Animal { method sound { "..." } }
class Dog is Animal { method sound { "woof" } }
is Dog.new.sound, "woof", "ordinary is-inheritance still works";

# also is rw in a class block
class Point { also is rw; has $.x; }
my $p = Point.new(x => 1);
lives-ok { $p.x = 5 }, "also is rw makes accessors writable";
is $p.x, 5, "also is rw accessor assignment took effect";

# also is Parent in a class block resolves the parent into the MRO
class C1 { method m { "c1" } }
class C2 { also is C1; }
is C2.new.m, "c1", "also is Parent (block form) inherits the method";
ok C2.^mro.map(*.^name).grep("C1"), "C1 is in C2's MRO via also is";
