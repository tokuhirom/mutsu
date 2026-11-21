use Test;

# ADR-18443 slice 3: `merge`, `zip`, `zip-latest`, `flat` and `start` over a
# live source (or over a per-tap recipe) are per-tap recipes too (#18443):
# nothing runs until a tap, each tap has its own buffers, and completion
# follows rakudo. The class-method forms take a recipe argument as live.

plan 11;

{
    my ($a, $b) = Supplier.new xx 2;
    my $n = 0;
    my $z = $a.Supply.map({ $n++; $_ }).zip($b.Supply);
    $a.emit(1);
    is $n, 0, 'an untapped zip runs nothing';
    my (@x, @y);
    $z.tap({ @x.push: $_ }, done => { @x.push: 'done' });
    $a.emit(2);
    $a.emit(3);
    $z.tap({ @y.push: $_ });
    $b.emit('x');
    $b.emit('y');
    $a.done;
    $b.emit('z');
    is-deeply ($n, @x, @y), (2, [(2, 'x'), (3, 'y'), 'done'], []),
        'each zip tap buffers for itself and is done once a done source runs dry';
}

{
    my $a = Supplier.new;
    my @z;
    $a.Supply.zip(Supply.from-list(1, 2)).tap({ @z.push: $_ }, done => { @z.push: 'done' });
    $a.emit('x');
    $a.emit('y');
    is-deeply @z, [('x', 1), ('y', 2), 'done'], 'zip with a finite source';
}

{
    my $s = Supplier.new;
    my @z;
    Supply.zip($s.Supply, $s.Supply).tap({ @z.push: $_ });
    $s.emit(3);
    is-deeply @z, [(3, 3),], 'Supply.zip over two live sources';
}

{
    my $s = Supplier.new;
    my @z;
    Supply.zip($s.Supply.map(* + 1), $s.Supply.grep(*.so), :with(&infix:<+>)).tap({ @z.push: $_ });
    $s.emit(3);
    is-deeply @z, [7], 'Supply.zip over recipes, with :with';
}

{
    my ($c, $d) = Supplier.new xx 2;
    my @l;
    Supply.zip-latest($c.Supply, $d.Supply, :initial(0, 0))
        .tap({ @l.push: $_ }, done => { @l.push: 'done' });
    $c.emit(1);
    $d.emit(2);
    $c.emit(3);
    $c.done;
    $d.emit(4);
    $d.done;
    is-deeply @l, [(1, 0), (1, 2), (3, 2), (3, 4), 'done'], 'zip-latest with :initial';
}

{
    my ($e, $f) = Supplier.new xx 2;
    my @m;
    Supply.merge($e.Supply.map(* * 2), $f.Supply).tap({ @m.push: $_ }, done => { @m.push: 'done' });
    $e.emit(1);
    $f.emit(5);
    $e.done;
    $f.emit(6);
    $f.done;
    is-deeply @m, [2, 5, 6, 'done'], 'Supply.merge over a recipe is done when every source is';
}

{
    my $s = Supplier.new;
    my @m;
    my $t = $s.Supply.map(* * 2).merge($s.Supply).tap({ @m.push: $_ });
    $s.emit(4);
    $t.close;
    $s.emit(5);
    is-deeply @m, [8, 4], 'merge as a method, released by close';
}

{
    my $n = 0;
    my $g = Supplier.new;
    my $src = $g.Supply.map({ $n++; $_ });
    my @stages = $src.flat, $src.start({ $_ });
    $g.emit(1);
    is $n, 0, 'untapped flat and start run nothing';
    my (@a, @b);
    my $f = $g.Supply.map({ $n++; ($_, $_ * 10) }).flat;
    my $t1 = $f.tap({ @a.push: $_ });
    $g.emit(1);
    my $t2 = $f.tap({ @b.push: $_ });
    $g.emit(2);
    $t1.close;
    $t2.close;
    $g.emit(3);
    is-deeply ($n, @a, @b), (3, [1, 10, 2, 20], [2, 20]), 'each flat tap runs its own chain';
}

{
    my $g = Supplier.new;
    my $results = Channel.new;
    $g.Supply.start({ $_ + 100 }).tap(-> $s { $s.tap({ $results.send: $_ }) });
    $g.emit(1);
    $g.emit(2);
    is-deeply (^2).map({ $results.receive }).sort.List, (101, 102), 'start over a live source';
}
