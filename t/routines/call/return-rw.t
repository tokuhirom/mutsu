use v6;
use Test;

plan 31;

# `return-rw` hands the CALLER the container, not a decontainerized value, so
# the call result is assignable / mutable. The routine does NOT need `is rw`:
# `return-rw` alone is enough.

# --- the ticket's original repro -------------------------------------------
{
    sub s0() { my $a = 41; return-rw $a }
    is ++s0(), 42, 'prefix ++ through a return-rw result';
}

# --- assignment through the call result -------------------------------------
{
    my $v = 1;
    sub f1() is rw { return-rw $v }
    f1() = 5;
    is $v, 5, 'is rw + return-rw: assignment writes the caller lexical';
}

{
    my $v = 1;
    sub f2() { return-rw $v }
    f2() = 5;
    is $v, 5, 'return-rw alone (no `is rw` trait) is still assignable';
}

# --- the plain-`return` contrast: NOT writable ------------------------------
{
    my $v = 1;
    sub g1() is rw { return $v }
    dies-ok { g1() = 5 }, 'plain `return` in an `is rw` sub is not assignable';
    is $v, 1, 'the failed assignment left the lexical untouched';
}

{
    sub g2() { 42 }
    dies-ok { g2() = 5 }, 'a plain non-rw sub is not assignable';
    dies-ok { ++g2() }, 'prefix ++ on a plain non-rw sub dies';
    dies-ok { g2()++ }, 'postfix ++ on a plain non-rw sub dies';
}

# --- ++ / -- in both positions ----------------------------------------------
{
    my $v = 41;
    sub h1() is rw { $v }
    is ++h1(), 42, 'prefix ++ returns the NEW value';
    is $v, 42, 'prefix ++ mutated the underlying container';
    is h1()++, 42, 'postfix ++ returns the OLD value';
    is $v, 43, 'postfix ++ mutated the underlying container';
}

{
    my $v = 41;
    sub h2() { return-rw $v }
    is --h2(), 40, 'prefix -- returns the NEW value';
    is $v, 40, 'prefix -- mutated the underlying container';
    is h2()--, 40, 'postfix -- returns the OLD value';
    is $v, 39, 'postfix -- mutated the underlying container';
}

{
    my $s = 'az';
    sub h3() is rw { $s }
    is ++h3(), 'ba', '++ through an rw sub does Str magic increment';
    is $s, 'ba', 'the string container was mutated';
}

# --- compound assignment through the call result ----------------------------
{
    my $v = 1;
    sub c1() { return-rw $v }
    c1() += 4;
    is $v, 5, '`+=` writes through a return-rw result';

    my $s = 'a';
    sub c2() is rw { $s }
    c2() ~= 'b';
    is $s, 'ab', '`~=` writes through an is-rw result';
}

# A short-circuiting `op=` must not assign at all when it short-circuits, so it
# stays legal on a non-rw routine whose value short-circuits.
{
    my $d = Nil;
    sub c3() is rw { $d }
    c3() //= 5;
    is $d, 5, '`//=` assigns through an rw result when the LHS is undefined';

    my $e = 1;
    sub c4() is rw { $e }
    c4() //= 9;
    is $e, 1, '`//=` leaves a defined rw result alone';

    sub c5() { 1 }
    lives-ok { c5() //= 9 }, '`//=` on a non-rw sub short-circuits before any write';
}

# --- return-rw of an array element ------------------------------------------
{
    my @a = 1, 2, 3;
    sub e1() is rw { return-rw @a[0] }
    e1() = 9;
    is @a, [9, 2, 3], 'return-rw of an array element is assignable';
    e1()++;
    is @a, [10, 2, 3], '++ through a return-rw array element';
    e1() += 5;
    is @a, [15, 2, 3], '+= through a return-rw array element';
}

# --- return-rw of an attribute ----------------------------------------------
{
    class C {
        has $!x = 1;
        method peek() is rw { return-rw $!x }
        method show() { $!x }
    }
    my $c = C.new;
    $c.peek() = 7;
    is $c.show, 7, 'return-rw of a private attribute is assignable';
}

# --- return-rw of a Proxy ---------------------------------------------------
{
    my $under = 0;
    sub p1() {
        return-rw Proxy.new(
            FETCH => -> $        { $under },
            STORE => -> $, $val  { $under = $val * 2 },
        );
    }
    p1() = 4;
    is $under, 8, 'return-rw of a Proxy routes the assignment through STORE';
}

# --- return-rw through the routine's own parameter ---------------------------
{
    my %h = a => 1;
    sub k1(\c) is rw { return-rw c<a> }
    k1(%h) = 9;
    is %h<a>, 9, 'return-rw of an element reached through the routine parameter';
}

# --- arity: no operand, and several ------------------------------------------
{
    sub n1() { return-rw; 'NOT REACHED' }
    is-deeply n1(), Nil, 'a bare `return-rw` returns Nil and exits the routine';
}

{
    my $a = 1;
    my $b = 2;
    sub n2() { return-rw $a, $b }
    is n2().raku, '(1, 2)', 'return-rw of several values returns the whole list';
}

done-testing;
