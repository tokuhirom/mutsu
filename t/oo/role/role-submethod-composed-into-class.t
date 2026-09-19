use Test;

plan 3;

# #8815: a role's `submethod` -- private or public -- must be composed into
# the consuming class's own method table just like any other role method.
# raku flattens role composition textually regardless of the default (6.d+)
# language revision; only class-to-subclass INHERITANCE (`is`) skips
# submethods, not role `does` composition.

{
    role R1 {
        submethod !helper(Int $x) { $x * 2 }
    }
    class C1 does R1 {
        method call-it($x) { self!helper($x) }
    }
    is C1.new.call-it(21), 42,
        'a role-composed private submethod is reachable via self! from the consuming class\'s own method';
}

{
    role R2 {
        submethod pub-helper() { "pub-from-role" }
    }
    class C2 does R2 { }
    is C2.new.pub-helper, "pub-from-role",
        'a role-composed public submethod is callable on the consuming class';
}

{
    # BUILD/TWEAK/DESTROY submethods from roles stay on their own dedicated
    # construction-phase ordering (`ctor_phase_plan.rs` / `class.rs`), which
    # already special-cases those three names -- the fix above only widens
    # composition for every OTHER submethod, so a role-supplied BUILD (no
    # class-own BUILD to interact with) must still run exactly once, not
    # twice via a newly-flattened method-table entry.
    my @order;
    role R4 {
        submethod BUILD(*%a) { @order.push: "R4" }
    }
    class C4 does R4 { }
    C4.new;
    is @order.elems, 1, 'role BUILD submethod runs exactly once, not double-invoked via the flat method table';
}
