use v6;
use Test;

plan 8;

# `push(@a, 1, |@rest);` as a *statement* used to fall through the
# mutating-listop rewrite (which only fired for an all-positional argument
# list) to the generic call dispatch, which has no `push` routine to resolve:
# "Unknown call: push". The same call in value position always worked.

{
    my @a;
    push(@a, 1, |(2, 3));
    is-deeply @a.List, (1, 2, 3), 'push with a trailing slip, sink context';
}
{
    my @a;
    push(@a, |(2, 3));
    is-deeply @a.List, (2, 3), 'push whose only argument is a slip';
}
{
    my @a;
    my @rest = 2, 3;
    push @a, 1, |@rest;
    is-deeply @a.List, (1, 2, 3), 'listop spelling (no parens) with a slip';
}
{
    my @a;
    my $r = push(@a, 1, |(2, 3));
    is-deeply @a.List, (1, 2, 3), 'value position still works';
}
{
    my @a = 9;
    unshift(@a, 1, |(2, 3));
    is-deeply @a.List, (1, 2, 3, 9), 'unshift with a slip';
}
{
    my @a;
    append(@a, 1, |(2, 3));
    is-deeply @a.List, (1, 2, 3), 'append with a slip';
}
{
    my @a;
    my @from-call = do { (7, 8) };
    push(@a, |@from-call);
    is-deeply @a.List, (7, 8), 'slip from a variable';
}
{
    # An empty slip contributes nothing.
    my @a;
    push(@a, 1, |());
    is-deeply @a.List, (1,), 'empty slip adds no element';
}

done-testing;
