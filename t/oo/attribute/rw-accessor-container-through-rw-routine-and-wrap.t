use Test;

# An `is rw` auto-accessor names its attribute's Scalar. When that accessor is
# the result of another `is rw` routine, or the terminal of a `.wrap` chain,
# the Scalar reaches the caller, so an assignment writes the attribute (#9706).
#
#     class A { has $.foo is rw }
#     sub f() is rw { $i.foo }; f() = 4;           # $i.foo is 4
#     A.^method_table<foo>.wrap(method ($s: |c) is rw { callwith($inst, |c) });
#     A.foo = 7;                                   # $inst.foo is 7
#
# Everything here is byte-identical under `raku` and `mutsu`.

plan 21;

# --- an `is rw` routine whose tail is the accessor ---------------------------

{
    class RwSub { has $.foo is rw; has $.ro = 1; has Int $.t is rw = 0 }
    my $i = RwSub.new(foo => 1);
    sub f() is rw { $i.foo }
    f() = 4;
    is $i.foo, 4, 'rw sub tail: assignment writes the attribute';

    my $x = f();
    $x = 10;
    is $i.foo, 4, 'rw sub tail: an ordinary assignment copies the value';
    is f() + 1, 5, 'rw sub tail: the result reads as its value';
    is f().VAR.^name, 'Scalar', 'rw sub tail: the result is a Scalar';

    sub h() is rw { return-rw $i.foo }
    h() = 9;
    is $i.foo, 9, 'return-rw of the accessor writes the attribute';

    sub r() is rw { $i.ro }
    throws-like { r() = 5 }, X::Assignment::RO, 'a read-only accessor stays immutable';
    is $i.ro, 1, 'the read-only attribute is unchanged';

    sub t() is rw { $i.t }
    throws-like { t() = "x" }, X::TypeCheck::Assignment, 'the type constraint travels with the container';
    t() = 3;
    is $i.t, 3, 'a typed attribute is written through the container';

    sub nr() { $i.foo }
    throws-like { nr() = 5 }, X::Assignment::RO, 'a non-rw sub still returns a value';
}

# --- the terminal of a wrapped accessor --------------------------------------

{
    class Wrapped { has $.foo is rw }
    my $inst = Wrapped.new;
    Wrapped.^method_table<foo>.wrap(method ($s: |c) is rw { callwith($inst, |c) });
    Wrapped.foo = 7;
    is $inst.foo, 7, 'type-object assignment through an rw wrapper writes the instance';
    is Wrapped.foo, 7, 'the wrapped read sees the write';
}

{
    class Counted { has $.foo is rw }
    my $n = 0;
    Counted.^method_table<foo>.wrap(method ($s: |c) is rw { $n++; callsame });
    my $c = Counted.new;
    $c.foo = 3;
    is $c.foo, 3, 'instance assignment through a callsame wrapper writes the attribute';
    is $n, 2, 'the assignment runs the wrapper too';

    my $r := $c.foo;
    $r = 5;
    is $c.foo, 5, 'a := bind of a wrapped accessor aliases the attribute';

    my $copy = $c.foo;
    $copy = 50;
    is $c.foo, 5, 'a plain read of a wrapped accessor is a value';

    sub f() is rw { $c.foo }
    f() = 77;
    is $c.foo, 77, 'an rw sub tail reaches a wrapped accessor';
}

{
    class Typed { has Int $.t is rw = 0; has $.ro = 1 }
    Typed.^method_table<t>.wrap(method ($s: |c) is rw { callsame });
    Typed.^method_table<ro>.wrap(method ($s: |c) is rw { callsame });
    my $o = Typed.new;
    throws-like { $o.t = "s" }, X::TypeCheck::Assignment, 'a wrapped typed accessor type-checks';
    $o.t = 4;
    is $o.t, 4, 'a wrapped typed accessor is written';
    throws-like { $o.ro = 5 }, X::Assignment::RO, 'a wrapped read-only accessor stays immutable';
}

{
    class PlainWrapper { has $.v is rw }
    PlainWrapper.^method_table<v>.wrap(method ($s: |c) { callsame });
    my $p = PlainWrapper.new(v => 1);
    throws-like { $p.v = 5 }, X::Assignment::RO, 'a non-rw wrapper decontainerizes the accessor';
}
