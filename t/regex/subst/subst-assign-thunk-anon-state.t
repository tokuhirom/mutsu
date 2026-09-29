use Test;

# The RHS of an assignment-form substitution (`s[pat] = EXPR`,
# `S[pat] = EXPR`) is a thunk of the enclosing scope, not a Block: an
# anonymous `state` in it (`$++`, `++$`) belongs to the enclosing scope and
# counts across matches (and across calls of the enclosing routine), while a
# real closure replacement (`s/a/{$++}/`) restarts per match. The thunk sees
# each match's own `$/` and captures, never the match that ran before the
# substitution.

plan 13;

{
    $_ = "aaa";
    s:g{a} = $++;
    is $_, '012', 's:g{} = $++ counts across matches';
}
{
    is (S:g{a} = $++ given "aaa"), '012', 'S:g{} = $++ counts across matches';
}
{
    $_ = "aaa";
    s:g{a} = ++$;
    is $_, '123', 's:g{} = ++$ counts across matches';
}
{
    my $n = 0;
    $_ = "aaa";
    s:g{a} = $n++;
    is $_, '012', 'named counter in the RHS';
    is $n, 3, 'named counter written back to the enclosing scope';
}
{
    $_ = "aaa";
    s:g/a/{$++}/;
    is $_, '000', 'a closure Block replacement restarts its anon state per match';
}
{
    sub count() { my $s = "aa"; $s ~~ s:g{a} = $++; $s }
    is count(), '01', 'first call of the enclosing routine';
    is count(), '23', 'the anon state persists across calls of the enclosing routine';
}
{
    "5" ~~ /(5)/;
    $_ = "abc";
    s{(b)} = "<" ~ $0 ~ ">";
    is $_, 'a<b>c', '$0 in the RHS is this match, not the preceding one';
}
{
    sub prior-match() { "5" ~~ /(5)/; my $x = "abc"; $x ~~ s:g{(\w)} = "<$0|$/>"; $x }
    is prior-match(), '<a|a><b|b><c|c>', 'per-match $/ and $0 after a preceding match in a routine';
}
{
    is-deeply [3, 4].map({ S{5} = $^a given "5" }).List, ("3", "4"),
        'a placeholder in the RHS belongs to the enclosing block';
}
{
    $_ = "a1b2";
    s:g{(\d)} = $0 * 2;
    is $_, 'a2b4', 'capture arithmetic per match';
}

# `$0` is `$/[0]`: a block with its own `$/` parameter reads its captures
# through it, not through the last match run anywhere.
{
    my $f = -> $/ { "[$0|" ~ $0 ~ "|$<x>]" };
    "zz" ~~ /(z)$<x>=z/;
    my $m = $/;
    "qq" ~~ /(q)$<x>=q/;
    is $f($m), '[z|z|z]', '$0 in a -> $/ block reads the parameter';
}
