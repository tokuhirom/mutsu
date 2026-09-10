use Test;

plan 8;

# `:g` / `:ov` / `:ex` make a match return a *List* of Matches, so a failure
# leaves `$/` an empty List — `+@$/` is 0. (A plain match returns a single
# Match, so its failure leaves `Nil`, and `@$/` is then `(Nil,)`, one element.)
# `:ov` and `:ex` used to clear `$/` to `Nil` like a plain match, which made
# `+@$/` report 1.

{
    my $r = ("abcdefgh" ~~ m:exhaustive/ a .+ a /);
    nok $r, ':ex with no match is falsy';
    is +@$/, 0, '...and leaves @$/ empty';
    isa-ok $/, List, '...with $/ an empty List';
}

{
    my $r = ("abc" ~~ m:ov/x/);
    nok $r, ':ov with no match is falsy';
    is +@$/, 0, '...and leaves @$/ empty';
}

{
    my $r = ("abc" ~~ m:g/x/);
    nok $r, ':g with no match is falsy';
    is +@$/, 0, '...and leaves @$/ empty';
}

# A plain failed match still leaves Nil.
{
    my $r = ("abc" ~~ /x/);
    nok $/.defined, 'a plain failed match still leaves $/ undefined';
}
