use Test;
plan 5;

# §B: private method (`$obj!m`) dispatch now uses the shared compiled-or-treewalk
# helper (was run_instance_method_resolved directly).
{
    class C1 {
        has $.v;
        method !secret { "s:" ~ $!v }
        method reveal { self!secret }
    }
    is C1.new(v => 7).reveal, "s:7", 'private method dispatched (compiled helper)';
}

# Private method that mutates an attribute.
{
    class C2 {
        has $.n is rw = 0;
        method !bump { $!n = $!n + 5 }
        method go { self!bump; self!bump; $!n }
    }
    is C2.new.go, 10, 'private method attribute mutation persists';
}

# Private method with arguments.
{
    class C3 { method !add($a, $b) { $a + $b }; method run { self!add(3, 4) } }
    is C3.new.run, 7, 'private method with args';
}

# `.*` method walk dispatches each MRO level's method.
{
    class A4 { method m { 1 } }
    class B4 is A4 { method m { 2 } }
    is B4.new.*m.sort.join(","), "1,2", '.* walks all MRO methods';
}

# Public method still works alongside privates.
{
    class C5 { has $.x = 9; method !p { $!x }; method pub { self!p + 1 } }
    is C5.new.pub, 10, 'public+private mix';
}
