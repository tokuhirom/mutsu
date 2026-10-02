use Test;

# A scalar sigil alias on a call of a Regex-valued variable -- `$<a>=<$re>`,
# and the bare `$<a>=$re` that interpolates a Regex the same way -- names the
# called regex's own Match, nested captures included. Rakudo's `subrule_alias`
# renames the call, so `$<a>=<$re>` is `<a=$re>`. The alias used to wrap a
# capture-isolated copy of the regex, which kept the matched span and dropped
# every capture inside it. Expectations were measured against rakudo 2026.07.

plan 23;

my $digits = /<digit>+/;
my $alphas = /<alpha>+/;

# The ticket's repro.
is ("12" ~~ /$<rx>=<$digits>/)<rx><digit>.elems, 2,
    '$<a>=<$re> keeps the called regex\'s nested captures';
{
    my $m = "12" ~~ /$<rx>=<$digits>/;
    is ~$m<rx>, '12', 'the alias holds the matched span';
    is $m<rx>.from ~ '..' ~ $m<rx>.to, '0..2', 'and its extent';
    ok !$m<digit>.defined, 'the nested capture does not leak into the outer match';
    is $m.keys.sort.join(','), 'rx', 'only the alias is filed at the outer level';
}

# The bare form interpolates the same Regex value.
{
    my $m = "12" ~~ /$<rx>=$digits/;
    is $m<rx><digit>.elems, 2, 'the bare $<a>=$re form nests too';
    is ~$m<rx><digit>[1], '2', 'with the per-iteration Matches in order';
}

# The angle spelling the alias is equivalent to.
is ("12" ~~ /<rx=$digits>/)<rx><digit>.elems, 2, '<a=$re> nests (the equivalent spelling)';

# Positions and the surrounding pattern are unaffected.
{
    my $m = "ab12cd" ~~ /<alpha>+ $<n>=<$digits> <alpha>+/;
    is ~$m<n>, '12', 'an alias inside a longer pattern';
    is $m<n><digit>.elems, 2, 'keeps its nested captures there too';
    is $m<n>.from, 2, 'and its own start offset';
}

# Two different aliases, each with its own nested captures.
{
    my $m = "12ab" ~~ /$<d>=<$digits> $<w>=<$alphas>/;
    is $m<d><digit>.elems, 2, 'the first alias nests its own captures';
    is $m<w><alpha>.elems, 2, 'the second alias nests its own captures';
    ok !$m<d><alpha>.defined, 'and neither sees the other\'s';
}

# One alias used twice is a List of the calls' Matches.
{
    my $m = "1a" ~~ /$<n>=<$digits> $<n>=<$alphas>/;
    is $m<n>.elems, 2, 'a repeated alias lists each call';
    is $m<n>[0]<digit>.elems, 1, 'the first call nests its captures';
    is $m<n>[1]<alpha>.elems, 1, 'the second call nests its captures';
}

# A quantified call is a List with one Match per repetition.
{
    my $m = "12" ~~ /$<rx>=<$digits>+/;
    is $m<rx>.elems, 1, 'a quantified aliased call is a List';
    is $m<rx>[0]<digit>.elems, 2, 'each repetition nests its captures';
}

# Grouping the call first makes the alias a plain span again, as in rakudo.
{
    my $m = "12" ~~ /$<rx>=[<$digits>]/;
    is ~$m<rx>, '12', 'an alias on a non-capturing group holds the span';
    ok !$m<rx><digit>.defined, 'and does not nest the inner call';
    my $c = "12" ~~ /$<rx>=(<$digits>)/;
    ok !$c<rx><digit>.defined, 'nor does one on a capturing group';
}

# An unaliased call is unchanged: still no captures leak out of the regex.
ok !("12" ~~ /<$digits>/)<digit>.defined, 'an unaliased <$re> call leaks no captures';
