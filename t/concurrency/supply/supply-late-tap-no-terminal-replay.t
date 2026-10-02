use Test;

plan 6;

# Issue #10866: a plain Supplier keeps no terminal state, so a tap made after
# it finished sees neither `done` nor `quit` (a Supplier::Preserving does
# replay). A `.share`d supply block feeds a plain Supplier, so it behaves the
# same once it completed.

{
    my $q = Supplier.new;
    $q.quit('x');
    my $quit = False;
    $q.Supply.tap({;}, quit => { $quit = True });
    nok $quit, 'a tap after a plain Supplier quit sees no quit';
}

{
    my $q = Supplier.new;
    my $sup = $q.Supply;
    $q.quit('y');
    my $quit = False;
    $sup.tap({;}, quit => { $quit = True });
    nok $quit, '... also through a Supply obtained before the quit';
}

{
    my $p = Supplier::Preserving.new;
    $p.emit(1);
    $p.quit('z');
    my @got;
    my $quit;
    $p.Supply.tap({ @got.push($_) }, quit => { $quit = .message });
    is-deeply @got, [1], 'a Supplier::Preserving still replays its backlog';
    is $quit, 'z', '... and its quit';
}

{
    my $s = Supplier.new;
    my $sh = supply { whenever $s.Supply { emit $_ } }.share;
    my $early = False;
    $sh.tap({;}, done => { $early = True });
    $s.done;
    ok $early, 'a shared supply tap sees the block complete';
    my $late = False;
    $sh.tap({;}, done => { $late = True });
    nok $late, 'a tap made after the shared block completed sees no done';
}
