use Test;

# #9585: a bounded pull of a lazy `gather` suspends the body at a `take` with
# an internal signal. A `CATCH` (even `default`) inside the gather body used to
# see that signal as an exception: it ran its handler and ended the region, so
# the rest of the body was lost. The suspension must be invisible to CATCH and
# resume the protected body right where it stopped.

plan 11;

{
    my @caught;
    my $head = (gather { CATCH { default { @caught.push: .message } }; take 1; take 2 }).head;
    is $head, 1, '.head of a gather whose body has CATCH default';
    is-deeply @caught, [], 'CATCH default does not see the suspension';
}

{
    my @log;
    my \g = gather {
        CATCH { default { @log.push: "caught" } };
        @log.push: 'a'; take 1;
        @log.push: 'b'; take 2;
        @log.push: 'c'; take 3;
        @log.push: 'd';
    };
    is g[0], 1, 'first element';
    is-deeply @log, ['a'], 'body suspended after the first take';
    is g[1], 2, 'second element resumes after the take';
    is g.elems, 3, 'every take is kept';
    is-deeply @log, [<a b c d>], 'each statement ran exactly once';
}

{
    my @seen;
    my @r = (gather { CATCH { when X::AdHoc { @seen.push: 'adhoc' } }; take 1; take 2; take 3 }).head(2);
    is-deeply [@r, @seen], [[1, 2], []], 'CATCH when X::AdHoc does not see the suspension';
}

is-deeply (gather { CATCH { default { } }; for ^2 -> $i { for ^2 -> $j { take "$i$j" } } })[^4],
    ('00', '01', '10', '11'), 'nested for loops inside a CATCH region resume in place';

is-deeply (gather { CATCH { default { } }; my $i = 0; while $i < 10 { take $i; $i++ } })[^4],
    (0, 1, 2, 3), 'while loop inside a CATCH region resumes in place';

{
    my @caught;
    my \d = gather { CATCH { default { @caught.push: .message } }; take 1; die 'late' };
    d[1];
    is-deeply @caught, ['late'], 'a real exception after a suspension still reaches CATCH';
}
