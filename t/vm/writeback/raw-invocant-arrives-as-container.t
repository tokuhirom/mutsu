use v6.e.PREVIEW;
use MONKEY-TYPING;
use Test;

# ADR-0067 slice 3b: a raw invocant parameter binds the CALLER's container, so
# mutation through it inside the body reaches the caller's variable.
#
# The contract measured against raku v2026.07 is that only the invocant
# PARAMETER's rawness matters here -- `is raw`/`is rw` on the routine governs the
# outbound (lvalue-return) direction, which is slice 3a's question. So all three
# raw spellings mutate whether or not the routine is rw-capable, and a plain
# `$s:` / `C:D $s:` invocant must not.
#
# Both compiled-method binders are exercised on purpose: a sigil-less `\S:`
# invocant keeps the method on the fast binder (`call_compiled_method_fast`),
# while `$s is raw:` / `$s is rw:` set `binds_caller_container` and route to the
# slow binder (`call_compiled_method`).

plan 26;

# --- the three raw-invocant spellings, instance receiver -------------------
{
    class A1 { method m(\S:) { S = 7 } }
    my $c = A1.new;
    $c.m;
    is $c, 7, 'sigil-less raw invocant reaches the caller (fast binder)';
}
{
    class A2 { method m($s is raw:) { $s = 7 } }
    my $c = A2.new;
    $c.m;
    is $c, 7, '`$s is raw:` invocant reaches the caller (slow binder)';
}
{
    class A3 { method m($s is rw:) { $s = 7 } }
    my $c = A3.new;
    $c.m;
    is $c, 7, '`$s is rw:` invocant reaches the caller (slow binder)';
}

# --- rw-capability of the ROUTINE is irrelevant to the arrival direction ----
{
    class B1 { method m(\S:) is raw { S = 8 } }
    my $c = B1.new;
    $c.m;
    is $c, 8, 'an `is raw` routine with a raw invocant still mutates';
}
{
    class B2 { method m(\S:) is rw { S = 9 } }
    my $c = B2.new;
    $c.m;
    is $c, 9, 'an `is rw` routine with a raw invocant still mutates';
}

# --- an augmented native type: the invocant is an ordinary Int -------------
{
    augment class Int { method mutsuArrInc(\S:) { S = S + 1 } }
    my $a = 42;
    $a.mutsuArrInc;
    is $a, 43, 'augment class Int: a raw invocant writes the scalar';
    $a.mutsuArrInc;
    is $a, 44, 'the write is repeatable (the container survives)';
}
{
    augment class Str { method mutsuArrSet(\S:) { S = 'x' } }
    my $s = 'a';
    $s.mutsuArrSet;
    is $s, 'x', 'augment class Str: a raw invocant writes the scalar';
}

# --- the frame shapes a receiver name can have -----------------------------
{
    my @a = 1, 2;
    for @a <-> $e { $e.mutsuArrInc }
    is @a.gist, '[2 3]', 'a `<->` loop parameter aliases the element it names';
}
{
    my @a = 5, 6;
    for @a -> $e is rw { $e.mutsuArrInc }
    is @a.gist, '[6 7]', 'an `is rw` loop parameter aliases the element it names';
}
{
    my $a = 1;
    my $f = { $a.mutsuArrInc };
    $f();
    is $a, 2, 'a captured-outer scalar written from inside a closure';
}
{
    sub g($x is rw) { $x.mutsuArrInc }
    my $a = 10;
    g($a);
    is $a, 11, 'an `is rw` sub parameter passes its container on';
}
{
    my $a = 3;
    my $b := $a;
    $b.mutsuArrInc;
    is $a, 4, 'a `:=`-bound alias shares the container it was bound to';
}

# --- candidate selection: multi, role, dynamic name ------------------------
{
    class C1 { multi method m(\S: Int $n) { S = $n }; multi method m(\S: Str $t) { S = $t } }
    my $c = C1.new;
    $c.m(9);
    is $c, 9, 'a multi candidate selected by a real argument';
    my $d = C1.new;
    $d.m('t');
    is $d, 't', 'the other multi candidate';
}
{
    role R1 { method m(\S:) { S = 'r' } }
    class C2 does R1 {}
    my $c = C2.new;
    $c.m;
    is $c, 'r', 'a raw invocant on a composed role method';
}
{
    class C3 { method m(\S:) { S = 'd' } }
    my $c = C3.new;
    my $name = 'm';
    $c."$name"();
    is $c, 'd', 'the runtime method-name spelling';
}

# --- the body may read the invocant before writing it ----------------------
{
    class D1 { method m(\S:) { my $was = S.^name; S = $was } }
    my $c = D1.new;
    $c.m;
    is $c, 'D1', 'the body observes the invocant before replacing it';
}

# --- regression controls ---------------------------------------------------
{
    # A non-raw invocant must not reach the caller. raku refuses the assignment
    # outright; mutsu does not (yet) -- what both agree on, and what this pins,
    # is that the caller's variable is NOT modified.
    class E1 { method m($s:) { $s = 7 } }
    my $c = E1.new;
    try { $c.m };
    ok $c ~~ E1, 'a plain `$s:` invocant does not reach the caller';
}
{
    class E2 { method m(E2:D $s:) is raw { $s = 7 } }
    my $c = E2.new;
    try { $c.m };
    ok $c ~~ E2, 'a typed non-raw invocant does not reach the caller, even `is raw`';
}
{
    # An ordinary method must keep value semantics for its invocant even though
    # the program above declares raw-invocant methods (the registry pre-filter
    # is program-wide, so this is the row that catches a gate that over-fires).
    class F1 { has $.v is rw; method bump { $!v = $!v + 1; $!v } }
    my $c = F1.new(v => 1);
    is $c.bump, 2, 'an ordinary method is unaffected';
    is $c.v, 2, 'and its attribute write still lands';
}
{
    # A raw-invocant method that only READS is still a plain rvalue call.
    class G1 { method m(\S:) { S } }
    my $c = G1.new;
    my $r = $c.m;
    ok $r === $c, 'a read-only raw-invocant method returns the invocant';
    ok $c ~~ G1, 'and leaves the caller alone';
}
{
    my $a = 100;
    is $a.mutsuArrInc, 101, 'the mutating call returns the assigned value';
    is $a, 101, 'and the caller sees it';
}
