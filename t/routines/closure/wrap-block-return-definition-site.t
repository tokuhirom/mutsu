use Test;

# ADR-0050: a Block's routine-ness is a property of its definition site. A
# block body re-compiled by the carrier (`.wrap`, and the other natives that
# call a code object through `call_sub_value`) used to be classified by
# whatever was on the call stack, so a wrapper block `.wrap`ped onto a method
# compiled as a Routine and its `return` returned from the wrapped method
# (#9892).

plan 9;

# The headline: no Routine encloses the wrapper block, so its `return` dies.
{
    my class C1 { method m() { "orig" } }
    my &w = -> |c { return "R" };
    C1.^lookup('m').wrap(&w);
    throws-like { C1.new.m }, X::ControlFlow::Return,
        'return in a wrapper block with no enclosing Routine throws';
}

# A Routine does enclose it: the `return` leaves that Routine, not the
# wrapped method.
{
    my class C2 { method m() { "orig" } }
    sub run2() {
        C2.^lookup('m').wrap(-> |c { return "R" });
        my $r = C2.new.m;
        "run2 ran on with $r";
    }
    is run2(), "R", 'return in a wrapper block leaves the lexically enclosing sub';
}

# A wrapper that does not return still wraps, and `callsame` still works.
{
    my class C3 { method m() { "orig" } }
    C3.^lookup('m').wrap(-> |c { "w(" ~ callsame() ~ ")" });
    is C3.new.m, "w(orig)", 'a wrapper block without return wraps normally';
}

# A wrapper `sub` is a Routine: its own `return` is its result.
{
    my class C4 { method m() { "orig" } }
    C4.^lookup('m').wrap(sub (|c) { return "S" });
    is C4.new.m, "S", 'return in a wrapper sub returns from that sub';
}

# The shapes that already agreed with raku, kept so the fix cannot move them.
{
    my &blk = -> { return 1 };
    sub call-it(&f) { f(); "not reached" }
    throws-like { call-it(&blk) }, X::ControlFlow::Return,
        'a top-level pointy block called from a sub throws';
}
{
    my class C6 { method go(&f) { f(); "not reached" } }
    my &blk = -> { return 1 };
    throws-like { C6.new.go(&blk) }, X::ControlFlow::Return,
        'a top-level pointy block called from a method throws';
}
{
    sub outer() {
        my &blk = -> { return "from-outer" };
        sub inner(&f) { f(); "inner-end" }
        inner(&blk);
        "outer-end";
    }
    is outer(), "from-outer", 'a pointy block returns from its lexically enclosing sub';
}
{
    sub with-map() { (1, 2, 3).map({ return "early" if $_ == 2; $_ }).eager; "late" }
    is with-map(), "early", 'return in a map block leaves the enclosing sub';
}
{
    my &anon = sub () { return "anon" };
    sub call-anon(&f) { f() ~ "!" }
    is call-anon(&anon), "anon!", 'an anonymous sub returns from itself';
}
