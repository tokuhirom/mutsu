use MONKEY-TYPING;
use Test;

# ADR-0067 slice 2: a method's rw-capability is the SAME declaration oracle a
# `sub` already uses -- `is rw`, `is raw`, or an explicit `return-rw` in the
# body. The method lvalue path used to test `is rw` alone, so the `is raw` and
# `return-rw` spellings were refused (or, for a type-object invocant, silently
# dropped the write) while the byte-identical `sub` spellings wrote through.
#
# Every expectation below was checked against raku v2026.07.

plan 26;

# --- K1/K2/K4: the three rw-capable spellings, instance invocant -------------

{
    class C1 { method m(\x) is rw { x } }
    my $a = 42;
    C1.new.m($a) = 5;
    is $a, 5, 'method(\x) is rw hands back the caller container';
}

{
    class C2 { method m(\x) is raw { x } }
    my $a = 42;
    C2.new.m($a) = 5;
    is $a, 5, 'method(\x) is raw hands back the caller container (K2)';
}

{
    class C3 { method m(\x) { return-rw x } }
    my $a = 42;
    C3.new.m($a) = 5;
    is $a, 5, 'method(\x) { return-rw x } hands back the caller container (K4)';
}

# --- the same three with a type-object invocant ------------------------------

{
    class D1 { method m(\x) is rw { x } }
    my $a = 42;
    D1.m($a) = 5;
    is $a, 5, 'is rw class-method lvalue';
}

{
    class D2 { method m(\x) is raw { x } }
    my $a = 42;
    D2.m($a) = 5;
    is $a, 5, 'is raw class-method lvalue (was a silent no-op)';
}

{
    class D3 { method m(\x) { return-rw x } }
    my $a = 42;
    D3.m($a) = 5;
    is $a, 5, 'return-rw class-method lvalue (was a silent no-op)';
}

# --- the container is an array/hash element, not just a scalar ---------------

{
    class E1 { method m(\x) is raw { x } }
    my @a = 1, 2;
    E1.new.m(@a[0]) = 9;
    is-deeply @a, [9, 2], 'is raw method over an array element';
}

{
    class E2 { method m(\x) { return-rw x } }
    my @a = 1, 2;
    E2.new.m(@a[0]) = 9;
    is-deeply @a, [9, 2], 'return-rw method over an array element';
}

{
    class E3 { method m(\x) is raw { x } }
    my %h = a => 1;
    E3.new.m(%h<a>) = 9;
    is %h<a>, 9, 'is raw method over a hash element';
}

# --- a multi method carries the trait too ------------------------------------

{
    class F1 {
        multi method m(\x) is raw { x }
    }
    my $a = 42;
    F1.new.m($a) = 5;
    is $a, 5, 'multi method ... is raw is rw-capable';
}

# --- a role-composed method carries the trait --------------------------------

{
    role R1 { method m(\x) is raw { x } }
    class G1 does R1 { }
    my $a = 42;
    G1.new.m($a) = 5;
    is $a, 5, 'a method composed from a role keeps is raw';
}

# --- augment carries the trait ------------------------------------------------

{
    class H1 { }
    augment class H1 { method m(\x) is raw { x } }
    my $a = 42;
    H1.new.m($a) = 5;
    is $a, 5, 'an augmented method keeps is raw';
}

# --- REGRESSION CONTROLS: a method that is NOT rw-capable must still refuse ---
# raku: `Cannot modify an immutable Int (42)`. The wording differs; the refusal
# is the contract.

{
    class N1 { method m(\x) { x } }
    my $a = 42;
    dies-ok { N1.new.m($a) = 5 }, 'a plain method is still not an lvalue';
    is $a, 42, '... and the caller variable is untouched';
}

{
    class N2 { method m($x) { $x } }
    my $a = 42;
    dies-ok { N2.new.m($a) = 5 }, 'a plain readonly-parameter method is still not an lvalue';
    is $a, 42, '... and the caller variable is untouched';
}

{
    # The type-object twin of the row above. It used to reach mutsu's legacy
    # `$obj.name($value)` setter convention and silently report success where
    # raku dies; the oracle now gates that chain, so it is a real refusal.
    class N3 { method m(\x) { x } }
    my $a = 42;
    dies-ok { N3.m($a) = 5 }, 'a plain class method is not an lvalue either';
    is $a, 42, '... and the caller variable is untouched';
}

{
    # The sigil'd-parameter spelling of the same, which additionally used to
    # bind the *assigned value* into the method's first parameter.
    class N4 { method m($x) { $x } }
    my $a = 42;
    dies-ok { N4.m($a) = 5 }, 'a plain class method with a $-parameter likewise';
    is $a, 42, '... and its caller variable is untouched too';
}

{
    # The method is called once, with its REAL argument -- not re-called as a
    # setter with the assigned value (`$`-param) or the invocant (`\`-param).
    my @seen;
    class N5 { method m($x) { @seen.push($x); $x } }
    my $a = 42;
    try { N5.m($a) = 5 };
    is-deeply @seen, [42], 'the method sees its real argument, exactly once';
}

# --- an `is rw` ATTRIBUTE accessor keeps working (the shape the oracle must
# --- not have disturbed: it names a location instead of computing one) -------

{
    class A1 { has $.v is rw }
    my $o = A1.new(v => 1);
    $o.v = 7;
    is $o.v, 7, 'is rw attribute accessor assignment still works';
}

{
    class A2 { has @.items }
    my $o = A2.new(items => [1, 2]);
    $o.items[0] = 9;
    is-deeply $o.items, [9, 2], 'an @-attribute accessor element write still works';
}

# --- the `sub` twin is unchanged (the oracle it already used) ----------------

{
    sub f(\x) is raw { x }
    my $a = 42;
    f($a) = 5;
    is $a, 5, 'the sub spelling is unchanged';
}

{
    sub g(\x) { return-rw x }
    my $a = 42;
    g($a) = 5;
    is $a, 5, 'the sub return-rw spelling is unchanged';
}

{
    sub h(\x) { x }
    my $a = 42;
    dies-ok { h($a) = 5 }, 'a plain sub is still not an lvalue';
}
