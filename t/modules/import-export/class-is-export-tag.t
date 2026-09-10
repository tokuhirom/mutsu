use v6;
use lib 't/lib';
use Test;

# Regression (TODO_dist T-057, Statistics::LinearRegression): a class declared
# `is export` inside a module must be importable by its bare name. A bare
# `is export` == DEFAULT (also under :ALL); `is export(:ALL)` is :ALL-only.
#
# NOTE on ordering: mutsu shares one global namespace across EVALs in a single
# process, so once an :ALL import brings a name in it stays visible. The
# negative check (an :ALL-only name is hidden under a plain `use`) must run
# BEFORE any :ALL import, so it comes first.

plan 4;

{
    # An :ALL-only class must NOT be visible under a plain `use`.
    my $died = False;
    try {
        EVAL q:to/CODE/;
            use ExportedTypeFixture;
            AllOnlyCls.new.who
        CODE
        CATCH { default { $died = True } }
    }
    ok $died, ':ALL-only class is not imported by a plain use';
}

{
    # Plain `use` imports the DEFAULT-tagged class.
    my $out = EVAL q:to/CODE/;
        use ExportedTypeFixture;
        DefaultCls.new.who
    CODE
    is $out, "DefaultCls", 'plain use imports the DEFAULT-tagged class';
}

{
    # Import everything under :ALL — both the DEFAULT and :ALL-only classes.
    my $out = EVAL q:to/CODE/;
        use ExportedTypeFixture :ALL;
        DefaultCls.new.who ~ "," ~ AllOnlyCls.new.who
    CODE
    is $out, "DefaultCls,AllOnlyCls", ':ALL imports both DEFAULT and :ALL-only classes';
}

{
    # multi method new: `(@x, @y)` beats `($a, $b)` for two array args.
    use ExportedTypeFixture :ALL;
    my @x = 1, 2, 3;
    my @y = 4, 5, 6;
    is Model.new(@x, @y).tag, 'arrays',
        'multi method new picks the sigil-narrower (@x, @y) candidate';
}
