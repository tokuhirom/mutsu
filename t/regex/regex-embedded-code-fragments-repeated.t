use Test;

# The three regex constructs whose embedded code #10121 moved onto a
# compile-once path -- `<{ … }>` interpolation, a `** { … }` count and a
# leading `:my` declaration -- must still re-evaluate their code on every
# match, against the values live at that match. The compile-count side is
# pinned by tests/regex_embedded_code_compiled_once.rs. Expected values
# were read off rakudo 2026.07.

plan 6;

{
    my $p = 'q';
    my @hits;
    for <q x q> -> $want {
        $p = $want;
        @hits.push: so "pqr" ~~ / p <{ $p }> r /;
    }
    is-deeply @hits, [True, False, True], '<{ }> sees the interpolated value of each iteration';
}

{
    my @hits;
    for ^5 -> $i { @hits.push: so "abbc" ~~ / ^ a b ** {$i % 3 + 1} c $ / }
    is-deeply @hits, [False, True, False, False, True], '** { } re-evaluates its count per match';
}

{
    my $n = 0;
    for ^5 { $n++ if "xyz" ~~ / :my $k = 'y'; x $k z / }
    is $n, 5, 'a leading :my declaration works on every iteration';
}

{
    my @seen;
    for <a b> -> $v {
        "z" ~~ / :my $x = $v; { @seen.push: $x } z /;
    }
    is-deeply @seen, ['a', 'b'], ':my initializer is evaluated afresh on each match';
}

# A plain `:my` no longer snapshots and restores the grammar token table;
# a `:my token` declaration still must work inside the regex that declares it.
{
    ok "a1" ~~ /
        :my token T { \d }
        a <T>
    /, ':my token is visible inside its own regex';
    my $n = 0;
    for ^3 {
        $n++ if "+1.5" ~~ /
            :my token SIGN { <[+-]> }
            <SIGN>? \d+ '.' \d+
        /;
    }
    is $n, 3, ':my token declared repeatedly in a loop keeps matching';
}
