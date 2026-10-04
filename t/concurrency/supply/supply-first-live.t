use Test;

# `.first` on a live (Supplier-backed) supply must register on the supplier
# and emit the first match as it arrives, not snapshot the (empty) values at
# call time (#11646).

plan 6;

{
    my $s = Supplier.new;
    my @seen;
    my $done = False;
    $s.Supply.first(* == 2).tap({ @seen.push($_) }, done => { $done = True });
    $s.emit($_) for 1..4;
    is-deeply @seen, [2], 'WhateverCode matcher emits only the first match';
    ok $done, 'the first supply is done once it matched';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.first({ .value == 2 }).tap({ @seen.push($_) });
    $s.emit((a => 1));
    $s.emit((b => 2));
    $s.emit((c => 2));
    is-deeply @seen, [b => 2], 'block matcher over Pairs';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.first.tap({ @seen.push($_) });
    $s.emit($_) for 5..7;
    is-deeply @seen, [5], 'no matcher: the first emitted value';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.first(Str).tap({ @seen.push($_) });
    $s.emit(1);
    $s.emit('x');
    $s.emit('y');
    is-deeply @seen, ['x'], 'a type object matcher smartmatches';
}

{
    my $s = Supplier.new;
    my @seen;
    $s.Supply.map(* * 10).first(* > 15).tap({ @seen.push($_) });
    $s.emit($_) for 1..3;
    is-deeply @seen, [20], 'first after a live map';
}
