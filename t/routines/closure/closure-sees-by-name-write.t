use Test;

# A write to a lexical by a runtime name (`EVAL`, a symbolic store, `MY::`)
# is seen by a closure that captured the lexical: the frame's lexicals are
# shared cells, not snapshots (#12753).

plan 10;

{
    my $x = 5;
    my &g = { $x };
    EVAL q[$x = 9];
    is $x, 9, 'EVAL write: the declaring frame sees it';
    is g(), 9, 'EVAL write: the capturing closure sees it';
}

{
    my $x = 5;
    my &g = { $x };
    ::(q[$x]) = 9;
    is g(), 9, '::("$x") store';
}

{
    my $x = 5;
    my &g = { $x };
    my $n = 'x';
    $::($n) = 9;
    is g(), 9, '$::($n) store';
}

{
    my $x = 5;
    my &g = { $x };
    MY::<$x> = 9;
    is g(), 9, 'MY::<$x> store';
}

sub in-routine-eval { my $x = 5; my &g = { $x }; EVAL q[$x = 9]; g() }
is in-routine-eval(), 9, 'EVAL write inside a routine';

sub in-routine-symbolic { my $x = 5; my &g = { $x }; ::(q[$x]) = 9; g() }
is in-routine-symbolic(), 9, 'symbolic store inside a routine';

sub in-routine-my { my $x = 5; my &g = { $x }; MY::<$x> = 9; (g(), $x) }
is-deeply in-routine-my(), (9, 9), 'MY:: store inside a routine';

sub nested-writer { my $x = 5; my &g = { $x }; { EVAL q[$x = 9] }(); (g(), $x) }
is-deeply nested-writer(), (9, 9), 'the writer is a closure nested in the frame';

{
    my $x = 5;
    my &g = { $x };
    EVAL q[$x = 9];
    $x = 11;
    is g(), 11, 'a later plain write still reaches the closure';
}
