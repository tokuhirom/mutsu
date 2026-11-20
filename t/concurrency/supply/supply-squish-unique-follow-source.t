use Test;

# `Supply.squish` / `Supply.unique` follow their source per tap, like
# rakudo's `supply { whenever self { ... } }`, instead of reading its
# materialized values (#18400): an on-demand source with a `whenever` over a
# live supply never finishes, and a live source has no values yet.

plan 13;

{
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup.Supply -> $value { emit $value }
        .emit for 1, 1, 2;
    }.squish;
    my @v;
    my $tap = $s.tap: { @v.push: $_ };
    $sup.emit($_) for 2, 3, 3, 1;
    is-deeply @v, [1, 2, 3, 1], 'squish over an on-demand source with a live whenever';
    $tap.close;
}

{
    my $sup = Supplier.new;
    my @v;
    $sup.Supply.squish.tap: { @v.push: $_ };
    $sup.emit($_) for 1, 1, 2, 2, 1;
    is-deeply @v, [1, 2, 1], 'squish over a live supply';
}

{
    my $sup = Supplier.new;
    my @v;
    $sup.Supply.squish(:as(*.lc)).tap: { @v.push: $_ };
    $sup.emit($_) for <a A b B a>;
    is-deeply @v, [<a b a>], 'squish :as over a live supply';
}

{
    my $sup = Supplier.new;
    my @v;
    $sup.Supply.squish(:with({ $^b - $^a == 1 })).tap: { @v.push: $_ };
    $sup.emit($_) for 1, 2, 4, 5, 9;
    is-deeply @v, [1, 4, 9], 'squish :with compares with the previous value';
}

{
    my $sup = Supplier.new;
    my @v;
    $sup.Supply.unique.tap: { @v.push: $_ };
    $sup.emit($_) for 1, 1, 2, 2, 1;
    is-deeply @v, [1, 2], 'unique over a live supply';
}

is-deeply supply { emit 1; emit 1; emit 2 }.unique.list, (1, 2),
    'unique over an on-demand supply';
is-deeply supply { emit $_ for <a A b B a> }.squish(:as(*.lc)).list, <a b a>,
    'squish :as over an on-demand supply';
is-deeply supply { emit $_ for <a A b B a> }.unique(:as(*.lc)).list, <a b>,
    'unique :as over an on-demand supply';
is-deeply supply { emit $_ for 1, 2, 4, 5, 9 }.unique(:with({ abs($^b - $^a) == 1 })).list,
    (1, 4, 9), 'unique :with over an on-demand supply';
is-deeply Supply.from-list(1, 1, 2, 2, 3, 1).squish.list, (1, 2, 3, 1),
    'squish over a finite supply';

{
    my @v;
    my $sq = supply { emit 1; emit 1; emit 2 }.squish;
    $sq.tap: { @v.push: $_ };
    $sq.tap: { @v.push: $_ };
    is-deeply @v, [1, 2, 1, 2], 'each tap keeps its own previous value';
}

{
    my @v;
    react {
        whenever supply { emit $_ for 3, 3, 4 }.squish { @v.push: $_ }
    }
    is-deeply @v, [3, 4], 'a react whenever over a squished on-demand supply';
}

{
    my $sup = Supplier.new;
    my $s = supply {
        whenever $sup.Supply -> $value { emit $value }
    }.unique;
    my @v;
    $s.tap: { @v.push: $_ };
    $sup.emit($_) for 5, 6, 5;
    is-deeply @v, [5, 6], 'unique over an on-demand source with a live whenever';
}
