use Test;

# #11217: dropping a structure nested deeper than the native stack holds,
# built at run time with no parser involved, must not crash the process
# (it used to SIGSEGV in recursive Drop).

plan 3;

# The nightly GC-stress job (`MUTSU_GC_EVERY_CANDIDATE`) fires a full scan
# every N candidate pushes, and each scan walks the whole live chain: building
# a million levels is quadratic there (ADR-0003 §1; 5+ minutes against the
# per-file timeout). That job runs shallower chains; the default job keeps
# the full depths.
my $stress = %*ENV<MUTSU_GC_EVERY_CANDIDATE>.defined;
my $deep = $stress ?? 30_000 !! 1_000_000;
my $mid = $stress ?? 20_000 !! 100_000;

{
    my $a = [];
    for ^$deep { $a = [$a] }
    $a = Nil;
    pass "dropping $deep nested arrays does not overflow";
}

{
    my %h = a => 1;
    for ^$mid { %h = (x => %h.clone) }
    %h = ();
    pass "dropping $mid nested hashes does not overflow";
}

{
    my $p = 1;
    for ^$mid { $p = 1 => $p<> }
    $p = Nil;
    pass "dropping $mid nested pairs does not overflow";
}
