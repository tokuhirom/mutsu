# A capture node reached through two parents is presented by ONE Match
# (ADR-11404): a wrapped self-recursive token sees each level's Match both as
# its own `callsame` result and as a child of the level above, and those used
# to be separate Matches, each materializing the whole subtree again -- a
# wrapper that renders its result (`.raku`) at every level was quadratic in the
# recursion depth (#11404). Values are rakudo's.
use Test;

plan 7;

{
    grammar R { token TOP { . <TOP> | \d+ } }
    my @seen;
    R.^find_method('TOP').wrap: sub (|c) {
        my Match $result := callsame;
        @seen.push: $result;
        $result
    };
    my $m = R.parse('...42');
    ok $m, 'the wrapped recursion parses';
    is @seen.elems, 5, 'the wrapper ran once per level';
    ok @seen[*-2]<TOP> === $m<TOP><TOP>,
        'a node reached through two parents is one Match';
    is $m<TOP><TOP><TOP>.Str, '42', 'the innermost level';
}

{
    grammar H { token TOP { <val=word> }; token word { \w+ } }
    my $h = H.parse('abc');
    ok $h<val> === $h<word>, 'an alias and its rule name share one Match';
}

# The work stays linear in the depth: with every level's result kept (as
# Grammar::Extractor keeps them), every node is materialized once, and every
# level matches against the one subject instead of copying it.
{
    my $script = q:to/END/;
        grammar G { token TOP { . <TOP> | \d+ } }
        my @keep;
        G.^find_method('TOP').wrap: sub (|c) {
            my Match $result := callsame;
            @keep.push: $result;
            my $shown = $result.raku if $result;
            $result
        };
        say so G.parse('.' x 100 ~ '42');
        END
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', $script, :out, :err, :%env);
    $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('match_materializations=')) // '';
    my $mat = +($line ~~ / 'match_materializations=' (\d+) /)[0] // -1;
    my $targets = +($line ~~ / 'match_targets=' (\d+) /)[0] // -1;
    ok 0 < $mat < 1000, "materializations are linear in the depth ($mat)";
    ok 0 < $targets < 10, "the subject is not copied per level ($targets)";
}
