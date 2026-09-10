use Test;

# Lexical shadowing correctness guard (ANALYSIS §1.4). An inner-block `my $x`
# must not corrupt the outer `$x`, and name-based writebacks (undefine, s///, ++)
# must land on the variable that is live at that source point even when the same
# name is shadowed elsewhere in the routine. (Correctness is currently provided by
# the runtime env restore; the compiler-slot rework is deferred — see the note in
# ANALYSIS.md §1.4. These cases guard against regressing the observable behavior.)

plan 12;

# --- shadow read correctness ---
{
    my $x = 1;
    { my $x = 2; is $x, 2, 'inner shadow reads inner slot'; }
    is $x, 1, 'outer slot intact after inner shadow';
}

# --- undefine writeback inside a shadow block hits the INNER slot ---
{
    my $foo = 10;
    {
        my $foo = 20;
        undefine($foo);
        nok $foo.defined, 'undefine targets the inner (live) shadow slot';
    }
    is $foo, 10, 'outer $foo untouched by inner undefine';
}

# --- s/// on the OUTER var, textually BEFORE an inner shadow block, hits OUTER ---
# (regression for the case where a runtime last-occurrence lookup would wrongly
#  pick the later-compiled inner shadow slot)
{
    my $s = 'a1';
    $s ~~ s/(\d+)/<$0>/;
    is $s, 'a<1>', 's/// writes back to the outer slot (shadow declared later)';
    for 1 {
        my $s = 'b2';
        $s ~~ s/(\d+)/[$0]/;
        is $s, 'b[2]', 's/// inside the shadow block hits the inner slot';
    }
}

# --- ++ writeback through a shadow ---
{
    my $n = 5;
    { my $n = 100; $n++; is $n, 101, 'post-increment hits inner shadow'; }
    is $n, 5, 'outer $n untouched by inner ++';
}

# --- temp restore through a shadow hits the INNER slot ---
# (regression: the scope-exit restore resolved the name -> outer slot)
{
    my $t = 1;
    {
        my $t = 10;
        { temp $t = 99; is $t, 99, 'temp assigns the inner shadow slot'; }
        is $t, 10, 'temp restore targets the inner (live) shadow slot';
    }
}

# --- rw-arg writeback through a shadow hits the INNER slot ---
# (regression: the writeback resolved the source name -> outer slot)
{
    sub bump($x is rw) { $x = 99 }
    my $r = 1;
    {
        my $r = 10;
        bump($r);
        is $r, 99, 'rw-arg writeback targets the inner (live) shadow slot';
    }
    is $r, 1, 'outer $r untouched by inner rw-arg writeback';
}
