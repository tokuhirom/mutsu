use Test;

# A source file containing Git/VCS conflict markers must be a compile-time
# error: rakudo reports "Found a version control conflict marker". A single
# conflict block throws a bare X::Comp::AdHoc; multiple blocks throw an
# X::Comp::Group whose sorrows hold all but the last marker and whose panic is
# the last one. Markers inside quotes/heredocs are NOT detected.

use MONKEY-SEE-NO-EVAL;

plan 11;

# Single conflict block -> X::Comp::AdHoc at the opening marker line.
{
    my $code = q:to/END/;
        say 'a';
        <<<<<<< HEAD
        say 'b';
        =======
        say 'c';
        >>>>>>> branch
        say 'd';
        END
    my $ex;
    { $code.EVAL; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Comp::AdHoc, 'single block throws X::Comp::AdHoc';
    is $ex.line, 2, 'opening marker reported at line 2';
    is $ex.payload, 'Found a version control conflict marker', 'payload set';
}

# Two conflict blocks -> X::Comp::Group; the marker inside the inner heredoc is
# NOT counted as a conflict.
{
    my $code = q:to/END/;
        say 'first both';
        <<<<<<< HEAD
        say 'your';
        =======
        say 'their';
        >>>>>>> branch

        say 'middle both';

        <<<<<<< HEAD
        say 'your';
        =======
        say 'their';
        >>>>>>> branch

        q:to/QUOTED/;
        <<<<<<< HEAD
        this is not
        =======
        a vcs conflict
        >>>>>>> branch
        QUOTED
        END
    my $ex;
    { $code.EVAL; CATCH { default { $ex = $_ } } }
    isa-ok $ex, X::Comp::Group, 'two blocks throw X::Comp::Group';
    is $ex.sorrows.elems, 1, 'one sorrow collected';
    isa-ok $ex.sorrows[0], X::Comp::AdHoc, 'sorrow is X::Comp::AdHoc';
    is $ex.sorrows[0].line, 2, 'first marker (sorrow) at line 2';
    is $ex.sorrows[0].payload, 'Found a version control conflict marker', 'sorrow payload';
    isa-ok $ex.panic, X::Comp::AdHoc, 'panic is X::Comp::AdHoc';
    is $ex.panic.line, 10, 'second marker (panic) at line 10';
    is $ex.panic.payload, 'Found a version control conflict marker', 'panic payload';
}
