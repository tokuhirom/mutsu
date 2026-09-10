use Test;

plan 4;

# A gather body pulls lazily after the declaring frame moves on, so a
# captured lexical read inside the body must be promoted to a shared cell:
# the pull must see writes made after the gather was created, not a stale
# by-value env snapshot. (MakeGather now carries an analysis-only closure of
# its body and boxes the captured-and-mutated lexicals it names, exactly like
# closure creation does.)
{
    my $x = 1;
    my $s = gather { take $x; take $x; };
    $x = 2;
    is $s.list.join(' '), '2 2', 'lazy gather body sees post-creation write';
}

# Same, inside a sub frame (a different boxing site than top-level code).
{
    sub f() {
        my $x = 1;
        my $s = gather { take $x; take $x; };
        $x = 2;
        $s.list.join(' ');
    }
    is f(), '2 2', 'lazy gather in a sub frame sees post-creation write';
}

# A cell-promoted captured counter must be incremented once per produced
# element across take batches: the second slice must RESUME the suspended
# coroutine, not re-run the body from scratch (a re-run would double-apply
# the increments through the shared cell). The body here is a single
# compound loop op, which suspends with ip == 0 — the resume check must use
# the coroutine's started flag, not `ip > 0`.
{
    my $c = 0;
    my @seq = lazy gather { loop { $c++; take $c } };
    is @seq[^4].join(' '), '1 2 3 4', 'coroutine batch 1 through a shared cell';
    is @seq[^5].join(' '), '1 2 3 4 5', 'coroutine batch 2 resumes instead of re-running';
}
