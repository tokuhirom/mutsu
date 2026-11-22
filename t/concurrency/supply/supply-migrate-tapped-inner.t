use Test;

# `migrate` over inner supplies that deliver only once tapped (#18601):
# `Supply.interval`, a `supply { }` block. The migration taps such an inner
# supply itself, forwards what it emits, and closes that tap when it switches
# away. They used to be read as already-finished snapshots, so a migrate
# over `Supply.interval` emitted nothing -- App::ByWord zips one with its
# words, and that zip never got a tick.

plan 3;

{
    my $intervaller = Supplier.new;
    my $migrated = $intervaller.Supply.migrate;
    my $ticks = Channel.new;
    $migrated.tap({ $ticks.send: $_ });
    $intervaller.emit: Supply.interval(0.02);
    is-deeply (^3).map({ $ticks.receive }).List, (0, 1, 2), 'migrate forwards a Supply.interval';
}

{
    my $intervaller = Supplier.new;
    my $words = Supply.zip($intervaller.Supply.migrate, Supply.from-list(<a b c>), :with{ @_.tail });
    my @got;
    my $done = Promise.new;
    $words.tap({ @got.push: $_ }, done => { $done.keep });
    $intervaller.emit: Supply.interval(0.02);
    await Promise.anyof($done, Promise.in(10));
    is-deeply @got, [<a b c>], 'zip of a migrated interval with a finite supply';
}

{
    my $intervaller = Supplier.new;
    my $closed = 0;
    my $migrated = $intervaller.Supply.migrate;
    my @got;
    $migrated.tap({ @got.push: $_ });
    $intervaller.emit: supply { emit 1; CLOSE { $closed++ } };
    $intervaller.emit: supply { emit 2 };
    is-deeply (@got, $closed), ([1, 2], 1), 'switching away closes a supply block the migration tapped';
}
