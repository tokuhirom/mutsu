use Test;

# Slice F (env<->locals coherence): auto-threading a method call over an
# invocant junction (`$junc.a`) where the threaded user method mutates a
# captured-outer / `our` variable must propagate *every* eigenstate's mutation
# to the caller, not just the last one. Each eigenstate call records its own
# pending writeback; the next call clears it, so an earlier eigenstate writing a
# different variable than the last would be lost. With the reverse env->locals
# pull disabled (single-store default), the junction dispatch path must reconcile
# the caller's local slots from env after threading. Regression:
# roast/S03-junctions/autothreading.t ('basic auto-threading over invocant').

plan 6;

# Two eigenstates writing the SAME var: accumulates.
{
    our $c = 0;
    class JA1 { method a { $c++ } }
    my Mu $x = JA1.new | JA1.new;
    $x.a;
    is $c, 2, 'same-var accumulation across two eigenstates';
}

# Eigenstates writing DIFFERENT vars: both preserved (the core regression).
{
    our $cnt1 = 0;
    our $cnt2 = 0;
    class JB1 { method a { $cnt1++ } }
    class JB2 { method a { $cnt2++ } }
    my Mu $x = JB1.new | JB1.new | JB2.new;
    $x.a;
    is $cnt1, 2, 'two eigenstates writing $cnt1 preserved';
    is $cnt2, 1, 'last eigenstate writing $cnt2 preserved';
}

# Nested junction (junction & junction) over invocant.
{
    our $n1 = 0;
    our $n2 = 0;
    class JC1 { method a { $n1++ } }
    class JC2 { method a { $n2++ } }
    my Mu $x = JC1.new | JC2.new & JC2.new;
    $x.a;
    is $n1, 1, 'nested-junction invocant: $n1';
    is $n2, 2, 'nested-junction invocant: $n2';
}

# Method call on a literal junction (non-mut receiver path).
{
    my $total = 0;
    class JD { method bump { $total++ } }
    (JD.new | JD.new | JD.new).bump;
    is $total, 3, 'literal junction invocant accumulates';
}
