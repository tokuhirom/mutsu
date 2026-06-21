use Test;

# Coherence pin for the env<->locals single-store path
# (MUTSU_NO_BLANKET_RECONCILE): a `LAST` phaser inside a `.map` block that
# writes a captured-outer lexical (`LAST $x = True`) must reach the caller's
# local slot when the map iterator is exhausted, not only the env entry that
# the (removed) blanket reconcile used to pull. The phaser body is compiled
# separately from the map body, so its writeback must be recorded too. Must
# pass identically with and without the blanket reconcile.

plan 4;

{
    my $ranLAST;
    my $iterator := (^20).map({
        LAST $ranLAST = True;
        last if $_ == 10;
        $_
    }).iterator;
    Nil until $iterator.pull-one =:= IterationEnd;
    ok $ranLAST, "LAST phaser write in a .map block reaches the slot (via last)";
}

{
    my $ranLAST = False;
    my @r = (1, 2, 3).map({ LAST $ranLAST = True; $_ * 2 });
    is @r, [2, 4, 6], "map body still produces values";
    ok $ranLAST, "LAST phaser write reaches the slot (natural end)";
}

{
    my $count = 0;
    (^5).map({ LAST $count = 42; $_ }).eager;
    is $count, 42, "LAST phaser captured write reaches the slot after eager force";
}
