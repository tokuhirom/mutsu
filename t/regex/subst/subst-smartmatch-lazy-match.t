# `$s ~~ s:g/../../` returns the list of every occurrence's Match. The `~~`
# op then looks through its right-hand value for regexes whose interpolated
# variables it must publish first, and that walk used to `view()` each lazy
# Match, forcing every one of them to materialize. A Match never holds a regex
# the smartmatch runs, so the walk now leaves them lazy. Values are rakudo's.
use Test;

plan 8;

{
    my $s = 'a1b22c333';
    my $r = ($s ~~ s:g/ \d+ /#/);
    is $s, 'a#b#c#', 's:g/// through ~~ still substitutes every occurrence';
    is $r.elems, 3, 'the smartmatch result lists one Match per occurrence';
    is $r.map(*.Str).join(','), '1,22,333', 'each Match reads its span';
    is $/.elems, 3, '$/ holds the same list';
    is $/[2].from, 6, 'positions are kept';
}

{
    my $pat = 'b';
    my $s = 'abcabc';
    $s ~~ s:g/ $pat /X/;
    is $s, 'aXcaXc', 'an interpolated variable in the pattern is still published';
}

# The work: a smartmatch-driven s:g/// over 200 occurrences materializes none
# of the Matches it returns (it used to materialize every one).
{
    my $script = q:to/END/;
        my $text = (^200).map({ "w$_" }).join(' ');
        for ^3 { my $copy = $text; $copy ~~ s:g/ \d+ /#/; }
        say 'done';
        END
    my %env = %*ENV;
    %env<MUTSU_VM_STATS> = '1';
    my $proc = run($*EXECUTABLE, '-e', $script, :out, :err, :%env);
    my $out = $proc.out.slurp(:close);
    my $line = $proc.err.slurp(:close).lines.first(*.contains('match_materializations=')) // '';
    is $out.trim, 'done', 'the script ran';
    my $mat = +(($line ~~ / 'match_materializations=' (\d+) /)[0] // -1);
    ok 0 <= $mat < 50, "the returned Matches stay lazy ($mat materializations for 600 occurrences)";
}
