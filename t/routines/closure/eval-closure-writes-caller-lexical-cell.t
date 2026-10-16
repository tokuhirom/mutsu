use Test;

# A closure created by an `EVAL` over a lexical of the calling code shares
# that variable with it: a write lands in one container, whichever routine
# runs the closure and however late, and the caller, the closure and its
# siblings all see it (ADR-12529 §3, slice 28). Checked against rakudo.

plan 7;

sub run(&c) { c() }

{
    my $h;
    my $handle;
    EVAL '$handle = { $h ~= "1"; $h ~= "2" }';
    run { $handle() };
    is $h, '12', 'a write from a closure another routine runs reaches the caller';
    $handle();
    is $h, '1212', 'and a direct call keeps adding to the same variable';
    $h = 'reset';
    $handle();
    is $h, 'reset12', 'the caller\'s own assignment is what the closure sees next';
}

{
    my $w = 0;
    my $inc = EVAL '-> { $w++ }';
    run($inc) for ^3;
    is $w, 3, 'a pointy block from EVAL accumulates in the caller\'s variable';
}

{
    my $get;
    my $set;
    {
        my $v = '';
        $get = { $v };
        EVAL '$set = { $v ~= "x" }';
    }
    run($set);
    $set();
    is $get(), 'xx', 'a sibling closure made before the EVAL shares the variable';
}

{
    my $h;
    my $handle;
    EVAL '$handle = { $h ~= "1"; once { $h ~= "F" }; $h ~= "2" }';
    lives-ok { $handle() }, 'a closure with a once block runs';
    is $h, '1F2', 'its writes reach the caller';
}
