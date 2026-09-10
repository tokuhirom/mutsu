use v6;
use Test;

# ADR-0059 Slice 2, bare-tail half: an `is rw` / `is raw` routine whose LAST
# statement is a plain expression (no `return-rw`) hands its caller the
# container that expression denotes, and the assignment writes through it.
# Before this, the assignment was re-interpreted from the callee's AST tail in
# the CALLER's frame, which cannot see the callee's parameters -- so
# `sub walk(%h) is rw { %h<k> }; walk(%hash) = "v"` silently did nothing.
# Every expectation below is raku's (v2026.06).

plan 32;

# --- an element reached through the routine's own parameter ----------------
{
    sub walk(%h) is rw { %h<some> }
    my %hash = some => 1;
    walk(%hash) = "val";
    is %hash<some>, "val", 'hash element tail through a %-param writes back';
}
{
    sub walk(@a) is rw { @a[0] }
    my @arr = 1, 2, 3;
    walk(@arr) = 99;
    is-deeply @arr, [99, 2, 3], 'array element tail through an @-param writes back';
}

# --- the doc's recursive walk (Type/Routine.rakudoc) -----------------------
{
    sub walk(\thing, *@keys) is rw {
        my $current := thing;
        for @keys -> $k {
            if $k ~~ Int { $current := $current[$k] } else { $current := $current{$k} }
        }
        $current;
    }
    my %hash;
    walk(%hash, 'some', 'key', 1, 2) = 'autovivified';
    is %hash<some><key>[1][2], 'autovivified', 'a :=-walked path autovivifies through the tail';
}

# --- a bare scalar lexical tail ---------------------------------------------
{
    my $value = 0;
    sub f() is rw { $value }
    f() = 9;    is $value, 9,  'assignment through a bare-variable tail';
    f() += 3;   is $value, 12, 'compound assignment';
    f()++;      is $value, 13, 'postfix increment';
    ++f();      is $value, 14, 'prefix increment';
    my $r := f(); $r = 42;
    is $value, 42, 'a binding to the call result shares the cell';
    $value = 7;
    is $r, 7, '... in both directions';
}

# --- is raw, and an rw parameter relayed as the tail --------------------------
{
    my $z = 3;
    sub n() is raw { $z }
    n() = 4;
    is $z, 4, 'an `is raw` routine hands out its tail container too';
}
{
    sub m($x is rw) is rw { $x }
    my $y = 1;
    m($y) = 9;
    is $y, 9, 'an `is rw` parameter relayed as the tail writes the caller variable';
}

# --- a getter built on the same routine does not vivify --------------------
{
    sub g() is rw { my %h; %h<a> }
    my $q := g();
    nok $q.defined, 'binding a missing-key tail reads undefined';
    my $v = g();
    nok $v.defined, 'assigning a missing-key tail reads undefined';
}

# --- a ternary tail assigns through the taken branch -------------------------
{
    sub c(\c, $flag) is rw { $flag ?? c<x> !! c<y> }
    my %t;
    c(%t, True) = 7;
    c(%t, False) = 8;
    is-deeply %t, {x => 7, y => 8}, 'a ternary tail assigns through the taken branch';
}

# --- methods ------------------------------------------------------------------
{
    class C {
        has %.store;
        has $.n is rw = 1;
        method at(\k) is rw { %!store{k} }
        method slot() is rw { $!n }
    }
    my $o = C.new;
    $o.at('k') = 'v';
    is-deeply $o.store, {k => 'v'}, 'a method element tail assigns';
    $o.slot() = 5;
    is $o.n, 5, 'a method attribute tail still assigns';
}

# --- a body with CATCH, an anonymous sub, return-rw without the trait ----------
{
    my $value = 0;
    sub catchy() is rw { CATCH { default { } }; $value }
    catchy() = 77;
    is $value, 77, 'the tail of a body with CATCH is still the lvalue return';

    my &anon = sub () is rw { $value };
    anon() = 78;
    is $value, 78, 'an anonymous `sub () is rw` assigns through its tail';

    sub rv() { return-rw $value }
    rv() = 79;
    is $value, 79, 'return-rw without the trait is still assignable';
}

# --- what is NOT assignable ----------------------------------------------------
{
    sub k() is rw { 42 }
    throws-like { k() = 1 }, X::Assignment::RO,
        message => /'Cannot modify an immutable Int (42)'/,
        'an `is rw` routine returning a plain value is X::Assignment::RO';
    sub h() { 42 }
    throws-like { h() = 1 }, X::Assignment::RO, 'a routine without the trait is not assignable';
    my $p = 1;
    sub plain() is rw { return $p }
    dies-ok { plain() = 5 }, 'a plain `return` decontainerizes even in an `is rw` routine';
    is $p, 1, '... and leaves the source untouched';
}

# --- the container stays invisible in rvalue context ---------------------------
{
    my $value = 79;
    my @arr = 99, 2, 3;
    sub sayit() is rw { $value }
    sub elem() is rw { @arr[1] }
    is sayit(), 79, 'a bare-variable tail reads as its value';
    is sayit().raku, '79', '.raku of the result is the value';
    is sayit() + 1, 80, 'arithmetic decontainerizes';
    is-deeply [sayit(), 1], [79, 1], 'list context holds the value';
    is elem(), 2, 'an element tail reads as its value';
    is elem().WHAT, Int, '.WHAT of an element tail result is the value type';
    my $copy = elem();
    $copy = 0;
    is-deeply @arr, [99, 2, 3], 'a plain `=` from the call copies, it does not alias';
    is @arr.raku, '[99, 2, 3]', 'the source array still renders without a cell';
    is $value.raku, '79', 'the source scalar still renders as its value';
}
