use Test;

# Slice F (env<->locals coherence): a method that mutates a captured outer
# lexical (`method bar { $Foo++ }` closing over an outer-block `$Foo`) merges the
# new value into the caller's env, but the caller's local slot was kept coherent
# only by the reverse `sync_locals_from_env` pull. This pins that the method
# call-site drains the captured-outer write through to the caller's local slot,
# so the behavior no longer depends on the reverse pull. Run with
# `MUTSU_NO_REVERSE_SYNC=1` to confirm coherence without the reverse pull.

plan 11;

# Post-increment of a captured outer scalar across two method calls.
{
    my $Foo = 0;
    class C1 { method bar { $Foo++ } }
    C1.new.bar;
    C1.new.bar;
    is $Foo, 2, 'captured-outer post-increment via method visible to caller';
}

# Compound assignment of a captured outer scalar.
{
    my $sum = 0;
    class C2 { method add($n) { $sum += $n } }
    C2.new.add(3);
    C2.new.add(4);
    is $sum, 7, 'captured-outer compound += via method accumulates';
}

# Plain assignment of two captured outer scalars in one method.
{
    my $p = 0;
    my $q = 0;
    class C3 { method both { $p = 5; $q = 9 } }
    C3.new.both;
    is $p, 5, 'first captured-outer scalar assigned via method';
    is $q, 9, 'second captured-outer scalar assigned via method';
}

# Captured outer array, pushed via a method.
{
    my @log;
    class C4 { method rec($x) { @log.push($x) } }
    C4.new.rec("a");
    C4.new.rec("b");
    is @log.join(","), "a,b", 'captured-outer array push via method visible';
    is @log.elems, 2, 'caller array slot is coherent for a later method call';
}

# Captured outer string, mutated via a method.
{
    my $msg = "x";
    class C5 { method app { $msg ~= "!" } }
    C5.new.app;
    C5.new.app;
    is $msg, "x!!", 'captured-outer string mutate via method visible';
}

# Read-modify-write reading back the value a prior method call left.
{
    my $v = 21;
    class C6 { method dbl { $v = $v * 2 } }
    C6.new.dbl;
    is $v, 42, 'captured-outer read-modify-write via method visible';
}

# Captured outer scalar used in a later arithmetic expression in the caller.
{
    my $base = 0;
    class C7 { method set { $base = 40 } }
    C7.new.set;
    is $base + 2, 42, 'caller slot coherent in a later expression';
}

# Captured outer hash, keyed-write via a method.
{
    my %h;
    class C8 { method put($k, $val) { %h{$k} = $val } }
    C8.new.put("a", 1);
    C8.new.put("b", 2);
    is %h<a> + %h<b>, 3, 'captured-outer hash element write via method visible';
}

# Mixed: one method increments, then the caller reads and a second call reads it.
{
    my $count = 0;
    class C9 { method tick { $count++ } }
    C9.new.tick;
    my $mid = $count;
    C9.new.tick;
    is "$mid/$count", "1/2", 'caller observes each captured-outer tick coherently';
}
