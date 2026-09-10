use Test;

# An lvalue subscript chain rooted at an accessor-style method call
# (`$o.a[0]<x> = 5`) used to lose the write entirely whenever it had to
# autovivify: mutsu rewrote it into a runtime builtin that rebuilt the
# containers copy-on-write and never installed the freshly autovivified level
# back into the attribute. It is now compiled by evaluating the accessor once
# into a compiler temp and running the ordinary variable-rooted chain against
# it -- the accessor already hands back the attribute's *shared* container, so
# the walk autovivifies in place and the write lands.
#
# Every expectation below was measured against Rakudo v2026.06 (2026-09-04)
# first; raku is the oracle and this file passes verbatim under both.

plan 14;

class A { has @.a }
class H { has %.h }

# --- 1. the shapes that lost the write --------------------------------------
{
    my $o = A.new;
    $o.a[0]<x> = 5;
    is $o.a.raku, [{:x(5)},].raku, "array attribute, positional then associative";
}
{
    my $o = A.new;
    $o.a[0][1] = 5;
    is $o.a.raku, [[Any, 5],].raku, "array attribute, two positional levels";
}
{
    my $o = A.new;
    $o.a[0]<x><y> = 5;
    is $o.a.raku, [{:x({:y(5)})},].raku, "array attribute, three levels";
}
{
    my $o = H.new;
    $o.h<a><b> = 5;
    is $o.h.raku, {:a({:b(5)})}.raku, "hash attribute, two associative levels";
}
{
    my $o = H.new;
    $o.h<a>[1] = 5;
    is $o.h<a>[1], 5, "hash attribute, associative then positional";
}
{
    my $o = H.new;
    $o.h<a><b><c> = 5;
    is $o.h<a><b><c>, 5, "hash attribute, three associative levels";
}

# --- 2. it is the attribute that changed, not a copy -------------------------
{
    my $o = A.new;
    $o.a[0]<x> = 5;
    is $o.a.elems, 1, "the attribute itself grew";
    $o.a[0]<y> = 6;
    is $o.a[0].keys.sort.join(","), "x,y", "a second write reaches the same element";
}
{
    sub make() { A.new }
    my $o = make();
    $o.a[0]<x> = 5;
    is $o.a[0]<x>, 5, "the root may be produced by a call";
}

# --- 3. an existing element is written through, not replaced ----------------
{
    my $o = A.new;
    $o.a[0] = {};
    $o.a[0]<x> = 5;
    is $o.a[0]<x>, 5, "an already-present element is descended into";
}

# --- 4. the single-level case is unchanged ----------------------------------
{
    my $o = A.new;
    $o.a[0] = 5;
    is $o.a.raku, [5,].raku, "a single-level accessor subscript still assigns";
}
{
    my $o = H.new;
    $o.h<a> = 5;
    is $o.h<a>, 5, "a single-level accessor key still assigns";
}

# --- 5. a typed attribute still refuses the autovivified intermediate -------
{
    class TA { has Int @.a }
    dies-ok { TA.new.a[0]<x> = 5 },
        "a typed array attribute refuses an autovivified inner Hash";
}
{
    class TH { has Int %.h }
    dies-ok { TH.new.h<a><b> = 5 },
        "a typed hash attribute refuses an autovivified inner Hash";
}
