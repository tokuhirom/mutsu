use Test;
plan 8;

# Method MRO callsame/nextsame chains run the redispatched candidates as compiled
# bytecode (§B). Verify the chain semantics are correct across inheritance.
{
    class A1 { method greet { "A" } }
    class B1 is A1 { method greet { "B" ~ callsame() } }
    class C1 is B1 { method greet { "C" ~ callsame() } }
    is C1.new.greet, "CBA", 'callsame walks the MRO (compiled redispatch)';
}

# nextsame (tail) returns the next candidate's result.
{
    class P2 { method tag { "p" } }
    class Q2 is P2 { method tag { nextsame } }
    is Q2.new.tag, "p", 'nextsame defers to parent';
}

# callsame return value is usable in the caller candidate.
{
    class P3 { method n { 10 } }
    class Q3 is P3 { method n { callsame() + 5 } }
    is Q3.new.n, 15, 'callsame return value usable';
}

# MRO with callsame across 3 levels accumulates.
{
    class L1 { method v { 1 } }
    class L2 is L1 { method v { callsame() + 10 } }
    class L3 is L2 { method v { callsame() + 100 } }
    is L3.new.v, 111, 'three-level callsame chain';
}

# Attribute mutation before callsame is visible to the parent (current self).
{
    class Base4 { has $.log is rw; method run { self.log ~= "base;" } }
    class Sub4 is Base4 { method run { self.log ~= "sub;"; callsame } }
    my $o = Sub4.new(log => "");
    $o.run;
    is $o.log, "sub;base;", 'attribute mutation visible across redispatch';
}

# nextsame with an argument-bearing method.
{
    class A5 { method f($x) { $x * 2 } }
    class B5 is A5 { method f($x) { nextsame } }
    is B5.new.f(7), 14, 'nextsame passes the same args';
}

# callwith re-dispatches with new args.
{
    class A6 { method g($x) { "got:$x" } }
    class B6 is A6 { method g($x) { callwith($x + 1) } }
    is B6.new.g(4), "got:5", 'callwith passes new args';
}

# Plain single-method call (no redispatch) still works.
{
    class S7 { method m { "ok" } }
    is S7.new.m, "ok", 'plain method call unaffected';
}
