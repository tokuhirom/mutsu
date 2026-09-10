use v6;
use Test;

# A `.hyper(:batch(1)).map` block that reads an outer lexical must observe that
# lexical's CURRENT value on every invocation. Previously the batch-thread env
# migration cached the first invocation's value in `shared_vars` and thread-clone
# `@`/`%` reads preferred that stale copy, freezing the captured lexical at its
# first-ever value across independent hyper ops. (zef
# distribution-depends-parsing test 21: Repository.candidates' per-call search
# args were frozen.)

plan 8;

sub via-hyper(@items, @filter) {
    my $results := @items.hyper(:batch(1)).map: -> $x {
        $x ~~ any(@filter) ?? $x !! Empty;
    }
    my @out;
    for $results.flat -> $v { @out.push($v) }
    @out;
}

my @items = <a b c>;

is via-hyper(@items, ['a']).sort,      <a>,   'first call: filter [a]';
is via-hyper(@items, ['b']).sort,      <b>,   'second call: filter [b] (was frozen at [a])';
is via-hyper(@items, ['c']).sort,      <c>,   'third call: filter [c]';
is via-hyper(@items, []).elems,        0,     'fourth call: empty filter';
is via-hyper(@items, <a b>).sort,      <a b>, 'fifth call: filter [a b]';

# Same shape, but the first call is the empty-result one.
is via-hyper(@items, ['z']).elems,     0,     'no-match first call';
is via-hyper(@items, ['a']).sort,      <a>,   'match after a no-match first call';

# A scalar captured lexical must also refresh per call.
sub scalar-hyper(@items, $needle) {
    my $r := @items.hyper(:batch(1)).map(-> $x { $x eq $needle ?? $x !! Empty });
    my @o; for $r.flat -> $v { @o.push($v) }; @o;
}
scalar-hyper(@items, 'a');
is scalar-hyper(@items, 'b'), <b>, 'scalar captured lexical refreshes per hyper call';
