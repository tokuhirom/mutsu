use Test;

# A closure literal written as an operand of a user-defined infix operator is
# handed to the operator's routine like a call argument, so it may outlive the
# statement: the routine can keep it and run it later. A lexical it writes is
# then the creating frame's own, shared through a cell, whether it runs
# during the call or afterwards (ADR-12529 §3, slice 26). Checked against
# rakudo.

plan 5;

my @later;
sub infix:<later>(&c, $x) { @later.push(&c); $x }
sub infix:<keep>($x, &c) { @later = (&c,); $x }
sub infix:<now>(&c, Int $times) { c() for ^$times; $times }

{
    my $n = 0;
    { $n++ } later 1;
    { $n += 10 } later 2;
    is $n, 0, 'the operator kept the closures without running them';
    .() for @later;
    is $n, 11, 'run after the statement, they write the block\'s lexical';
}

{
    my $m = 0;
    5 keep { $m = 42 };
    @later[0]();
    is $m, 42, 'a closure on the right of the operator too';
}

{
    my $k = 0;
    { $k++ } now 3;
    is $k, 3, 'a closure the operator runs during the call';
    my &again = -> { $k * 2 };
    { $k++ } now 1;
    is again(), 8, 'a sibling closure sees the operand closure\'s writes';
}
