use Test;

# A `:=` to an attribute binds the instance's attribute when it runs; there is
# no exit-time copy of the frame back into the object (#18356). Each case is
# checked against rakudo.

plan 14;

# The twigil-less `has $x` alias, bound to a variable.
{
    my $var = 42;
    my class K {
        has $x;
        method bind { $x := $var }
        method get { $x }
        method set($v) { $x = $v }
    }
    my $o = K.new;
    $o.bind;
    is $o.get, 42, 'has $x bound to a variable reads it';
    $var = 23;
    is $o.get, 23, 'and follows the variable';
    $o.set(19);
    is $var, 19, 'and writes through to it';
}

# Bound to a value.
{
    my class V { has $x; method bind { $x := 42 }; method get { $x } }
    my $o = V.new;
    $o.bind;
    is $o.get, 42, 'has $x bound to a value reads the value';
}

# Bound, then read back in the same method after a nested call.
{
    my $var = 'a';
    my class N {
        has $x;
        method inner { $x }
        method bind-and-read { $x := $var; self.inner }
    }
    is N.new.bind-and-read, 'a', 'a nested call sees the binding before the binder returns';
}

# Twigil forms, aggregate and scalar.
{
    my @src = 1, 2;
    my %src = k => 1;
    my $s = 5;
    my class T {
        has @!a; has %!h; has $!s; has @.pa;
        method bind(@a, %h, $s is rw, @pa) { @!a := @a; %!h := %h; $!s := $s; @!pa := @pa }
        method show { "{@!a} {%!h.sort.map(*.kv).join(',')} {$!s} {@!pa}" }
    }
    my @pa = 9;
    my $o = T.new;
    $o.bind(@src, %src, $s, @pa);
    @src.push(3); %src<j> = 2; $s = 6; @pa.push(10);
    is $o.show, '1 2 3 j 2,k 1 6 9 10', 'aggregate and scalar attribute binds follow their sources';
}

# An aggregate bound to an immutable value keeps that value's type.
{
    my class L {
        has @!a; has %!h;
        method bind { @!a := (1, 2); %!h := Map.new((k => 1)) }
        method types { "{@!a.^name} {%!h.^name}" }
        method rebind { @!a := [5]; @!a.push(6); @!a.elems }
    }
    my $o = L.new;
    $o.bind;
    is $o.types, 'List Map', 'a List and a Map bound to attributes stay a List and a Map';
    is $o.rebind, 2, 'a rebind to an Array is mutable in the same method';
    is $o.types, 'Array Map', 'and the instance keeps the rebind';
}

# A method's own `:=` lexical that shares an attribute's name is not the attribute.
{
    my class S {
        has $.pol = 'attr';
        method go { my $pol := 'lexical'; $pol }
    }
    my $o = S.new;
    is $o.go, 'lexical', 'the lexical binds';
    is $o.pol, 'attr', 'and the same-named attribute is untouched';
}

# An `is rw` parameter named like an attribute is not bound into it.
{
    my class P {
        has $.pol = 'attr';
        method !f($pol is rw) { $pol = 'changed' }
        method go { my $x = 'x'; self!f($x); $x }
    }
    my $o = P.new;
    is $o.go, 'changed', 'the rw parameter writes its argument';
    is $o.pol, 'attr', 'and leaves the attribute alone';
}

# Role and inheritance: the bind reaches the composed / inherited attribute.
{
    my $v = 1;
    my role R { has $!r; method bind-r { $!r := $v }; method r { $!r } }
    my class B { has $!b; method bind-b { $!b := $v }; method b { $!b } }
    my class D is B does R { }
    my $o = D.new;
    $o.bind-r; $o.bind-b;
    $v = 7;
    is "{$o.r} {$o.b}", '7 7', 'role and parent attribute binds follow the source';
}
