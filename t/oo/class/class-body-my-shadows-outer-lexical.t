use Test;

# A `my`/`state` declared at the top level of a class (or role) body shadows a
# same-named lexical of the declaring scope for every method of the type. The
# methods used to capture the declaring frame's variable and read THAT, so the
# outer value won over the type's own static -- and a write through the method
# clobbered the outer variable. Expected values are rakudo's.

plan 20;

# --- the reported shape ------------------------------------------------------
{
    my $a = 1;
    class A1 { my $a = 7; method m { $a } }
    is A1.m, 7, 'a method reads the class-body `my`, not the outer lexical';
    is $a, 1, 'the outer lexical is untouched';
}

# --- writes through a method stay inside the class ---------------------------
{
    my $b = 1;
    class A2 { my $b = 7; method m { $b = $b + 1; $b } }
    A2.m;
    is A2.m, 9, 'the method updates the class static across calls';
    is $b, 1, 'the outer lexical is not written through the method';
}
{
    my $c = 1;
    class A3 { my $c = 7; method set($v) { $c = $v }; method get { $c } }
    A3.set(9);
    is A3.get, 9, 'one method sees another method\'s write to the static';
    is $c, 1, 'the outer lexical stays as it was';
}
{
    my $n = 100;
    class Counter { my $n = 0; method inc { ++$n } }
    Counter.inc for ^2;
    is Counter.inc, 3, 'a per-class counter counts from its own static';
    is $n, 100, 'and leaves the outer counter alone';
}

# --- other declarators and sigils --------------------------------------------
{
    my $s = 1;
    class A4 { state $s = 7; method m { $s } }
    is A4.m, 7, 'a class-body `state` shadows the outer lexical too';
}
{
    my @e = 1;
    my %h = a => 1;
    class A5 { my @e = 7, 8; my %h = b => 2; method m { (@e.elems, %h.keys.join) } }
    is A5.m.join(' '), '2 b', '@ and % class-body lexicals shadow their outer namesakes';
}
{
    my sub f { 1 }
    class A6 { my sub f { 7 }; method m { f() } }
    is A6.m, 7, 'a class-body `my sub` is the one the method calls';
}
{
    my $o = 1;
    class A7 { our $o = 7; method m { $o } }
    is A7.m, 7, 'a class-body `our` is read by the method';
}

# --- what is NOT shadowed ----------------------------------------------------
{
    my $p = 1;
    class A8 { { my $p = 7; }; method m { $p } }
    is A8.m, 1, 'a `my` in a nested block of the body does not shadow';
}
{
    my $q = 5;
    class A9 { my $r = 7; method m { $q + $r } }
    is A9.m, 12, 'an outer lexical with another name is still captured';
}
{
    my $t = 1;
    class A10 { my $t = 7; method m($t) { $t } }
    is A10.m(3), 3, 'a method parameter still shadows the class static';
}

# --- submethods, closures and routine-declared classes -----------------------
{
    my $u = 1;
    class A11 { my $u = 7; submethod s { $u }; method m { my $c = { $u }; $c() } }
    is A11.new.m, 7, 'a closure inside a method reads the class static';
    is A11.new.s, 7, 'a submethod reads the class static';
}
{
    sub make-class-value {
        my $v = 1;
        class A12 { my $v = 7; method m { $v } }
        A12.m
    }
    is make-class-value(), 7, 'a class declared in a routine reads its own static';
}

# --- roles -------------------------------------------------------------------
{
    my $w = 1;
    role R1 { my $w = 7; method m { $w } }
    class C1 does R1 { }
    is C1.m, 7, 'a role-body `my` shadows the outer lexical in the composed method';
    is $w, 1, 'and the outer lexical is untouched';
}
