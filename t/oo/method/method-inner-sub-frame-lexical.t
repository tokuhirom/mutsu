use Test;

plan 14;

# A call-only `my sub` in a method body is a frame lexical (ADR-0113): it must
# behave exactly like the registry-installed one it replaces.

my class C {
    has $.base = 10;

    method twice($x) { my sub g($y) { $y * 2 }; g($x) + g(1) }
    method closes($x) { my sub h($y) { $x + $y }; h(5) }
    method uses-self($x) { my sub s($y) { $y + $!base }; s($x) }
    method as-value($x) { my sub k($y) { $y + 1 }; my &c = &k; c($x) }
    method recurse($n) { my sub fact($m) { $m <= 1 ?? 1 !! $m * fact($m - 1) }; fact($n) }
    method named($x) { my sub n(:$a = 1, :$b = 2) { $a * 10 + $b }; n(:a($x)) }
    method counts { my $n = 0; my sub bump { $n++ }; bump() for ^3; $n }
}

my $o = C.new;
is $o.twice(3), 8, 'inner sub called twice from a method';
is $o.twice(10), 22, 'second activation sees its own argument';
is $o.closes(4), 9, 'inner sub reads the method parameter';
is $o.closes(100), 105, 'a later call reads its own parameter';
is $o.uses-self(1), 11, 'inner sub reads an attribute of the invocant';
is $o.as-value(7), 8, 'inner sub used as a value';
is $o.recurse(5), 120, 'inner sub recursion';
is $o.named(3), 32, 'inner sub with named parameters';
is $o.counts, 3, 'inner sub mutating a method lexical';

# Same-named inner subs in two methods do not see one another.
my class D {
    method a { my sub f { 'a' }; f() }
    method b { my sub f { 'b' }; f() }
}
is D.new.a, 'a', 'first method keeps its own inner sub';
is D.new.b, 'b', 'second method keeps its own inner sub';

# An inner sub does not leak into the surrounding program.
my class E { method m { my sub only-here { 1 }; only-here() } }
E.new.m;
dies-ok { EVAL 'only-here()' }, 'inner sub is not visible outside the method';

# A method in a subclass with an inner sub of the same name as its parent's.
my class F is C {
    method twice($x) { my sub g($y) { $y * 3 }; g($x) }
}
is F.new.twice(2), 6, 'subclass method uses its own inner sub';
is C.new.twice(2), 6, 'parent method unaffected by the subclass inner sub';
