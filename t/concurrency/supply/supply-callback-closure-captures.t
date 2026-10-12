use Test;

# A closure made in one invocation of a Supply callback reads that
# invocation's lexicals, even when a later invocation of the same callback
# calls it (#12872, ADR-12529). The callback runs as a compiled block, as a
# `.map` callback does, so its closures' free variables are their captures,
# never a same-named lexical of the frame that runs them. Every expectation
# was checked against rakudo.

plan 5;

{
    my @cl;
    my @seen;
    my $s = Supplier.new;
    $s.Supply.tap(-> $x {
        if $x eq 'stop' { .() for @cl }
        else { @cl.push: { @seen.push: "tap sees $x" } }
    });
    $s.emit('a');
    $s.emit('b');
    $s.emit('stop');
    is @seen.join(','), 'tap sees a,tap sees b',
        'a closure made in a tap callback keeps its invocation\'s parameter';
}

{
    my @cl;
    my @seen;
    my $s = Supplier.new;
    $s.Supply.map(-> $x { @cl.push: { @seen.push: $x }; $x }).tap(-> $ { .() for @cl });
    $s.emit(1);
    $s.emit(2);
    is @seen.join(','), '1,1,2', 'a closure made in a Supply.map callback keeps its value';
}

{
    my $count = 0;
    my $s = Supplier.new;
    $s.Supply.tap(-> $x { $count += $x });
    $s.emit(2);
    $s.emit(3);
    is $count, 5, 'a tap callback writes an outer lexical';
}

{
    my @got;
    my $s = Supplier.new;
    $s.Supply.tap({ @got.push: $_ * 10 });
    $s.emit(1);
    $s.emit(2);
    is @got.join(','), '10,20', 'a bare-block tap callback binds its topic';
}

{
    my $s = Supplier.new;
    my $p = $s.Supply.grep(* > 1).map(* * 2).Promise;
    $s.emit(1);
    $s.emit(3);
    $s.done;
    is await($p), 6, 'WhateverCode callbacks still run';
}
