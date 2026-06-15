use Test;

# A `.subst` / `s///` replacement block may mutate variables it closes over.
# Those writes must propagate back to the caller (the substitution machinery
# previously cloned and restored the whole env around the block, discarding
# them), and across matches under `:g` the counter must keep advancing.

plan 6;

# single mutation visible outside
{
    my $n = 0;
    "abc".subst(/(\w)/, { $n++; "x" });
    is $n, 1, 'closure mutation in .subst writes back (single match)';
}

# :g runs the block once per match and carries the mutation forward
{
    my $n = 0;
    "abc".subst(/\w/, { $n++; "x" }, :g);
    is $n, 3, 'closure counter advances across :g matches';
}

# assigning an outer variable from the match inside the block
{
    my $m;
    "abc".subst(/(\w)/, { $m = ~$/[0]; "x" });
    is $m, 'a', 'block can assign an outer var from $/';
}

# post-increment string replacement + :samecase across :g (roast subst.t 152)
{
    my $str = "that";
    is 'The foo and the bar'.subst(/:i the/, { ++$str }, :samecase),
        'Thau foo and the bar', 'pre-incr block replacement with :samecase';
    is 'The foo and the bar'.subst(/:i the/, { $str++ }, :g, :samecase),
        'Thau foo and thav bar', ':g block replacement advances shared $str';
}

# the outer topic is untouched by the block's injected match topic
{
    $_ = "OUTER";
    "abc".subst(/\w/, { "x" }, :g);
    is $_, "OUTER", 'outer $_ restored after subst block';
}
