use Test;

# A hyper assignment into a literal list of lvalues distributes its RHS across
# the target's shape with the ordinary hyper dwim rules, exactly like every
# other hyper target. It used to index the RHS positionally instead, which made
# a scalar RHS an out-of-range index and a short list pad with Any.

plan 12;

{
    my ($x, $y);
    ($x, $y) »=» 5;
    is "$x,$y", "5,5", 'a scalar RHS broadcasts across a list of lvalues';
}

{
    my ($x, $y);
    ($x, $y) «=» 5;
    is "$x,$y", "5,5", 'a scalar RHS broadcasts when both sides dwim';
}

{
    my ($a, $b, $c);
    (($a, $b), $c) »=» 9;
    is "$a,$b,$c", "9,9,9", 'a scalar RHS broadcasts into nested sublists';
}

{
    my ($x, $y, $z);
    ($x, $y, $z) »=» (5, 6);
    is "$x,$y,$z", "5,6,5", 'a short dwimmy RHS cycles instead of padding with Any';
}

{
    my ($x, $y);
    ($x, $y) »=» (5, 6, 7);
    is "$x,$y", "5,6", 'a long dwimmy RHS is truncated to the target shape';
}

{
    my ($x, $y);
    ($x, $y) »=» (5, 6);
    is "$x,$y", "5,6", 'an equal-length RHS still assigns element-wise';
}

# The arrows decide which side may adapt: only `»` on the right lets the RHS
# grow or shrink to the target's length.
throws-like { EVAL 'my ($x, $y); ($x, $y) «=« 5' }, X::HyperOp::NonDWIM,
    'a scalar RHS under a non-dwimmy right arrow is X::HyperOp::NonDWIM';

throws-like { EVAL 'my ($x, $y); ($x, $y) »=« (5, 6, 7)' }, X::HyperOp::NonDWIM,
    'a length mismatch under a non-dwimmy right arrow is X::HyperOp::NonDWIM';

{
    my @a = 1, 2, 3;
    my $x;
    (@a, $x) »=» 5;
    is "@a[]/$x", "5 5 5/5", 'a listy target element takes one broadcast value per slot';
}

{
    my ($x, $y);
    ($x, $y) «=» ();
    ok !$x.defined && !$y.defined, 'an empty RHS leaves every target undefined';
}

# The assignment's own value is the distributed list, not the last element
# stored.
{
    my ($x, $y);
    my $r = (($x, $y) »=» (5, 6));
    is $r.raku, '$(5, 6)', 'a list-target hyper assignment evaluates to the distributed list';
}

{
    my ($a, $b);
    my $evaluations = 0;
    ($a, $b) »=» do { $evaluations++; 7 };
    is "$a,$b/$evaluations", "7,7/1", 'a broadcast RHS is still evaluated exactly once';
}
