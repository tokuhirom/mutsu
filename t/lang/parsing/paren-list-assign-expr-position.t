use Test;

plan 5;

# ecosystem/859ab33e (parse-error-expectation-dump cluster, #7988): a
# parenthesized multi-element list assignment (`($a, $b, ...) = RHS`, the
# "list assignment" operator, distinct from single-scalar "item assignment")
# parsed fine as a whole statement, but not in expression position: as a
# listop's argument, or as a later item of a comma list. The `=` was left
# unconsumed there, producing a bare "Confused. expected statement: expected
# use statement or import statement or no statement or ..." parse error with
# no hint that assignment was the actual problem.
#
# Found via Geo::Coordinates::UTM's t/01_basic_settings.t:
#   ok ($zone,$east,$north)=|latlon-to-utm(...), "latlon-to-utm available";
# which is `ok LIST-ASSIGNMENT, "message"` — the list assignment is the first
# of the two arguments `ok` (an expression listop) takes.

# The listop-argument position (mirrors the Geo::Coordinates::UTM repro).
{
    sub triple() { return 10, 20, 30 }
    my ($a, $b, $c);
    ok ($a, $b, $c) = |triple(), 'list assignment as a listop argument, with a trailing sibling argument';
    is "$a $b $c", "10 20 30", 'the list-assignment targets were actually bound';
}

# `say` (an ordinary, non-Test listop) over the same shape, checking the
# assignment expression's own value (a list assignment evaluates to the
# right-hand list).
{
    my ($a, $b);
    my $out = ($a, $b) = (1, 2);
    is-deeply $out.list, (1, 2), 'list assignment in listop-argument position evaluates to the RHS list';
    is "$a $b", "1 2", 'list assignment in listop-argument position still binds its targets';
}

# A later item of a top-level comma list (not the first term of the
# statement) — the comma before it must not stop the list-assignment
# from parsing, and must not be swallowed by it either.
{
    my ($x, $y);
    my @collected = 1, ($x, $y) = (7, 8);
    is "{@collected.elems} $x $y", "2 7 8",
        'list assignment as a non-first comma-list item still parses and binds';
}
