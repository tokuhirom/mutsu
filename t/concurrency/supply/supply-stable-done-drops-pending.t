use Test;

# #18615: `Supply.stable` passes `done` straight through, as rakudo does; it
# does not flush the value it holds first. A finite source is done before any
# window ends, so it emits nothing.

plan 4;

{
    my $s = Supplier.new;
    my @c;
    $s.Supply.stable(10).tap({ @c.push: $_ }, done => { @c.push: 'done' });
    $s.emit(1);
    $s.done;
    sleep 0.3;
    is-deeply @c, ['done'], 'done is passed on at once, without the held value';
}

is-deeply Supply.from-list(1, 2, 3).stable(0.05).list, (),
    'a finite source is done before any value is stable';

{
    my @e = supply { emit 1; sleep 0.3; emit 2; emit 3; sleep 0.3 }.stable(0.1).list;
    is-deeply @e, [1, 3], 'a finite source that pauses emits the value before each pause';
}

{
    my $t = Supplier.new;
    my @d;
    $t.Supply.stable(0.05).tap({ @d.push: $_ });
    $t.emit(1);
    $t.emit(2);
    sleep 0.3;
    $t.emit(3);
    sleep 0.3;
    is-deeply @d, [2, 3], 'a live source still debounces';
}
