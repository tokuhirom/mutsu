use v6;
use lib 't/lib';
use Test;
use DynExportedConst;

plan 38;

# ---------------------------------------------------------------------------
# A hyper operator whose base operator is itself a hyper operator.
#
# `Language/operators.rakudoc` spells the nested form; rakudo builds a
# `MetaInfix::Hyper` wrapping another `MetaInfix::Hyper`. Only the OUTERMOST
# pair's dwim flags govern dimension mismatches, so a nested spelling behaves
# exactly like the plain one carrying the same outer delimiters. (The outer
# pair must be spelled in Unicode: a fully ASCII `<<<<+>>>>` is ambiguous and
# rakudo rejects it too.)
# ---------------------------------------------------------------------------
{
    my $neighbours = ((-1, 0), (0, -1), (0, 1), (1, 0));
    my $p = (2, 3);
    is ($neighbours »>>+<<» ($p, *)).gist, '((1 3) (2 2) (2 4) (3 3))',
        'nested hyper >>+<< inside an outer Unicode pair';
    is ($neighbours »+» ($p, *)).gist, '((1 3) (2 2) (2 4) (3 3))',
        'the plain hyper spelling agrees with the nested one';

    my $a = ((1, 2), (3, 4));
    my $b = ((10, 20), (30, 40));
    is ($a »«+»« $b).gist, '((11 22) (33 44))',
        'a fully Unicode-spelled nested hyper';
    is ($a «<<+>>» $b).gist, '((11 22) (33 44))',
        'a dwim-both outer pair around a dwim-both inner one';

    my $ragged = ((1, 2), (3, 4, 5));
    is ($ragged »>>+<<» $b).gist, '((11 22) (33 44 35))',
        'the OUTER dwim-right governs the mismatch at every depth';
    dies-ok { $ragged »>>+<<« $b },
        'a strict outer pair still rejects a dimension mismatch';

    is ((1, 2, 3) »>>+<<» 1).gist, '(2 3 4)',
        'a nested hyper against a scalar right operand';
}

# ---------------------------------------------------------------------------
# Parentheses are pure grouping: `(EXPR).method` is exactly `EXPR.method`, so a
# parenthesized scalar keeps its container.
# ---------------------------------------------------------------------------
{
    my $j = 1;
    is $j.VAR.^name, 'Scalar', 'a bare scalar reports its container';
    is ($j).VAR.^name, 'Scalar', 'one layer of parens keeps the container';
    is (($j)).VAR.^name, 'Scalar', 'two layers of parens keep the container';
    is ( ($j) ).VAR.^name, 'Scalar', 'whitespace inside the parens changes nothing';

    my @arr = 1, 2;
    is (@arr[0]).VAR.^name, 'Scalar', 'a parenthesized element keeps its container';

    my ($g) = 7, 8, 9;
    is $g, 7, 'my ($g) = LIST assigns the first element';
    is ( ($g) ).VAR.^name, 'Scalar', 'the declared variable is a Scalar container';
}

# ---------------------------------------------------------------------------
# A postcircumfix `{...}` chains onto any parenthesized term, including one
# whose last operation is a compound assignment to a hash subscript — the
# `LogP6` idiom `(%h{$k} //= SetHash.new){$t} = True`.
# ---------------------------------------------------------------------------
{
    my %h;
    (%h{"a"} //= {}){"k"} = 1;
    is %h<a><k>, 1, 'compound-assign in parens, then a chained {} write';

    my %g;
    (%g{"a"} ||= { n => 7 }){"m"} = 8;
    is %g<a><n> ~ '/' ~ %g<a><m>, '7/8', 'the ||= spelling in the same shape';

    my %j;
    %j<a> = { n => 5 };
    is (%j{"a"}){"n"}, 5, 'a parenthesized subscript may be subscripted again';

    my %k;
    is (%k{"a"} //= { n => 6 })<n>, 6, 'the paren result is the assigned value';

    my $x;
    ($x //= {}){"k"} = 3;
    is $x<k>, 3, 'the scalar-lvalue spelling still works';
}

# ---------------------------------------------------------------------------
# `Map.new` / `Hash.new` argument classification (ADR-0021): a bare colonpair
# written in the argument list is a NAMED argument, and named arguments become
# data only when the call carries no positional argument at all.
# ---------------------------------------------------------------------------
{
    is Map.new("a", 1, :b(2)).keys.sort.join(','), 'a',
        'a bare colonpair does not become a positional Pair for Map.new';
    is Hash.new("a", 1, :b(2)).keys.sort.join(','), 'a',
        'the same rule holds for Hash.new';
    is Map.new("a", 1).keys.sort.join(','), 'a',
        'the positional-only call is unchanged';
    is Map.new(:42a, :666b).keys.sort.join(','), 'a,b',
        'an all-named call keeps the named args as data';
    is Map.new((:42a, :666b)).keys.sort.join(','), 'a,b',
        'and agrees with the parenthesized positional spelling';
    is Map.new((:42a), :7c).keys.sort.join(','), 'a',
        'a parenthesized pair is positional, so the named one is dropped';
    is Map.new(a => 1, b => 2).keys.sort.join(','), 'a,b',
        'fat-arrow pairs in a call are positional arguments';

    sub slurpy(*@pos, *%named) { "{@pos.join('|')}/{%named.keys.sort.join('|')}" }
    is slurpy("a", 1, :b(2)), 'a|1/b',
        'the general named-argument rule is unchanged for plain subs';
}

# ---------------------------------------------------------------------------
# The `\q...[...]` embedded-quote escape family works in every quoting
# construct (`Language/quoting.rakudoc`, "Escaping").
# ---------------------------------------------------------------------------
{
    my $v = 7;
    is "a\qq[1+1]b", 'a1+1b', '\\qq[...] inside a double-quoted string';
    is "a\q[1+1]b", 'a1+1b', '\\q[...] inside a double-quoted string';
    is "p\qq[$v]q", 'p7q', '\\qq[...] re-quotes its body under qq rules';
    is qq!a\qq[1+1]b!, 'a1+1b', '\\qq[...] inside qq//';
    is q[r\qq[$v]s], 'r7s', '\\qq[...] inside a non-interpolating q[]';
    is "t\qw[a b]u", 'ta bu', '\\qw[...] still works alongside it';

    my $s = 'ab';
    $s ~~ s/a/x\qq[1+1]y/;
    is $s, 'x1+1yb', '\\qq[...] inside an s/// replacement';

    my $h = qq:to/END/;
    z\qq[1+1]w
    END
    is $h.chomp, 'z1+1w', '\\qq[...] inside a heredoc';
}

# ---------------------------------------------------------------------------
# A `constant` imported from a `use`d module is a complete nullary term, so it
# may stand unparenthesized in a ternary branch. (`Compress::Bzip2` does this
# with constants re-exported through a dynamic `sub EXPORT`.)
# ---------------------------------------------------------------------------
{
    my $c = 1;
    my $t = $c ?? PEG_RUN !! PEG_FLUSH;
    is $t, 11, 'an imported constant in the then-branch of a ternary';
    my $f = 0 ?? PEG_RUN !! PEG_FLUSH;
    is $f, 22, 'and in the else-branch';
    sub second($a, $b) { $b }
    is second($c, ($c) ?? PEG_RUN !! PEG_FLUSH), 11,
        'the same ternary as a call argument';
}

done-testing;
