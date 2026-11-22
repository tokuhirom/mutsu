use Test;

# ADR-18443 slice 3: `classify` and `categorize` over a live source (or over a
# per-tap recipe) are per-tap recipes too (#18443): nothing runs until a tap,
# each tap has its own groups, closing it releases the source, and the groups
# finish with the source.

plan 5;

{
    my $n = 0;
    my $s = Supplier.new;
    my $c = $s.Supply.map({ $n++; $_ }).classify(* %% 2);
    $s.emit(1);
    is $n, 0, 'an untapped classify runs nothing';
    my (%a, %b);
    my $t1 = $c.tap(-> $p { my $k = $p.key; $p.value.tap({ %a{$k}.push: $_ }) });
    $s.emit(1);
    my $t2 = $c.tap(-> $p { my $k = $p.key; $p.value.tap({ %b{$k}.push: $_ }) });
    $s.emit(2);
    $s.emit(3);
    is $n, 5, 'each classify tap runs its own chain';
    is-deeply (%a.sort.List, %b.sort.List),
        ((False => [1, 3], True => [2]), (False => [3], True => [2])),
        'each classify tap has its own groups';
    $t1.close;
    $t2.close;
    $n = 0;
    $s.emit(4);
    is $n, 0, 'closed classify taps release the source';
}

{
    my @g;
    my $s = Supplier.new;
    $s.Supply.categorize({ $_ > 1 ?? <big odd> !! () }).tap(
        -> $p {
            @g.push: $p.key;
            $p.value.tap({ @g.push: "{$p.key}:$_" }, done => { @g.push: "{$p.key} done" });
        },
        done => { @g.push: 'done' });
    $s.emit(1);
    $s.emit(3);
    $s.done;
    is-deeply @g, ['big', 'big:3', 'odd', 'odd:3', 'big done', 'odd done', 'done'],
        'categorize groups finish before the categorized supply';
}
