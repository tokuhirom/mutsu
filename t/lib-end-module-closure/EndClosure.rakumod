my @q;
my sub bag-it() { @q.Bag }
sub add() is export { @q.push('x') }
END {
    if @q.elems -> uint $total {
        my $b := bag-it;
        note "typed-if ", $b.elems;
    }
    my $f = -> $t { bag-it };
    note "closure ", $f(1).elems;
    note "anon-sub ", (sub (Int $t) { bag-it })(1).elems;
}
