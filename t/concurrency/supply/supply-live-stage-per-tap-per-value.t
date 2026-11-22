use Test;

# skip / min / max / minmax / rotor / repeated / encode / decode over a live
# source are per-tap recipes with their own state, as in rakudo (ADR-18443,
# #18579). They used to snapshot the source's (empty) values and emit
# nothing.

plan 21;

{
    my $s = Supplier.new;
    my (@a, @b, @c, @d);
    $s.Supply.skip(1).tap({ @a.push: $_ });
    $s.Supply.map(* + 0).skip(1).tap({ @b.push: $_ });
    $s.Supply.map(* + 0).min.tap({ @c.push: $_ });
    $s.Supply.map(* + 0).rotor(2).tap({ @d.push: $_ });
    $s.emit($_) for 3, 1, 2;
    is @a.join(' '), '1 2', 'skip over a live source';
    is @b.join(' '), '1 2', 'skip over a recipe';
    is @c.join(' '), '3 1', 'min over a recipe';
    is @d.raku, '[[3, 1],]', 'rotor over a recipe';
}

{
    my $s = Supplier.new;
    my %r;
    sub t($name, $sup) {
        %r{$name} = [];
        $sup.tap({ %r{$name}.push: $_.raku }, done => { %r{$name}.push: 'done' });
    }
    t 'max', $s.Supply.max;
    t 'minby', $s.Supply.min(-*);
    t 'max2', $s.Supply.max({ $^a <=> $^b });
    t 'minmax', $s.Supply.minmax;
    t 'rotorp', $s.Supply.rotor(2, :partial);
    t 'rotorgap', $s.Supply.rotor(2 => 1);
    t 'rotorov', $s.Supply.rotor(3 => -2);
    t 'rotorcyc', $s.Supply.rotor(1, 2);
    t 'repeated', $s.Supply.repeated;
    t 'encode', $s.Supply.map(~*).encode;
    $s.emit($_) for 3, 1, 2, 3, 5, 0, 4;
    $s.done;
    is %r<max>.join(' '), '3 5 done', 'max emits each new maximum';
    is %r<minby>.join(' '), '3 5 done', 'min with a mapper';
    is %r<max2>.join(' '), '3 5 done', 'max with a comparator';
    is %r<minmax>.join(' '), '3..3 1..3 1..5 0..5 done', 'minmax emits each new range';
    is %r<rotorp>.join(' '), '[3, 1] [2, 3] [5, 0] [4] done', 'rotor(:partial) flushes on done';
    is %r<rotorgap>.join(' '), '[3, 1] [3, 5] done', 'rotor with a gap';
    is %r<rotorov>.join(' '), '[3, 1, 2] [1, 2, 3] [2, 3, 5] [3, 5, 0] [5, 0, 4] done',
        'rotor with an overlap';
    is %r<rotorcyc>.join(' '), '[3] [1, 2] [3] [5, 0] [4] done', 'rotor cycles its counts';
    is %r<repeated>.join(' '), '3 done', 'repeated emits the values seen before';
    is %r<encode>.elems, 8, 'encode emits a Blob per value';
}

{
    my $s = Supplier.new;
    my @r;
    $s.Supply.repeated(:as(* % 2)).tap({ @r.push: $_ });
    $s.emit($_) for 3, 1, 2, 4, 5;
    is @r.join(' '), '1 4 5', 'repeated(:as) compares the keys';
}

{
    my $s = Supplier.new;
    my $skip = $s.Supply.skip(1);
    my (@x, @y);
    $skip.tap({ @x.push: $_ });
    $s.emit(1);
    $skip.tap({ @y.push: $_ });
    $s.emit(2);
    $s.emit(3);
    is "@x[] | @y[]", '2 3 | 3', 'each tap skips its own first value';
}

{
    my $n = 0;
    my $s = Supplier.new;
    my $max = $s.Supply.map({ $n++; $_ }).max;
    $s.emit(1);
    is $n, 0, 'an untapped stage runs nothing';
    my $tap = $max.tap({;});
    $s.emit(2);
    $tap.close;
    $s.emit(3);
    is $n, 1, 'a closed tap releases the chain';
}

{
    my $s = Supplier.new;
    my @r;
    $s.Supply.decode.tap({ @r.push: $_ });
    $s.emit("ab".encode);
    $s.emit("c".encode);
    $s.done;
    is @r.join, 'abc', 'decode over a live source';
}

is Supply.from-list(1..7).rotor(2 => 1, :partial).list.raku, '([1, 2], [4, 5], [7])',
    'a finite rotor folds through the same state';
is Supply.from-list(3, 1, 2, 5).minmax.list.raku, '(3..3, 1..3, 1..5)',
    'a finite minmax folds through the same state';
