use Test;

plan 5;

my %values =
    first => { left => 1, right => 2 },
    second => { only => 3 };

%values{*;*}.map({ $_ = $_ + 10 }).eager;

is-deeply %values,
    { first => { left => 11, right => 12 }, second => { only => 13 } },
    'map on an associative multidimensional slice writes through to every leaf';

my %nested =
    first => { left => { value => 1 }, right => { value => 2 } },
    second => { only => { value => 3 } };

%nested{*;*;*}.map({ $_ = $_ * 10 }).eager;

is-deeply %nested,
    {
        first  => { left => { value => 10 }, right => { value => 20 } },
        second => { only => { value => 30 } },
    },
    'nested associative multidimensional slices retain writable leaf cells';

my %to-delete = first => 1, second => 2, keep => 3;
%to-delete{%to-delete.keys.grep(* ne 'keep')}:delete;

is-deeply %to-delete, { keep => 3 },
    'a lazy key sequence remains a hash slice for :delete';

my %stores = first => { path => { remove => 1, keep => 2 } };
for %stores{*;*} -> $store {
    $store{ $store.pairs.grep(*.value == 1).map: *.key }:delete;
}

is-deeply %stores, { first => { path => { keep => 2 } } },
    'deletion through a for-bound nested hash preserves the selected slice';

my $topic_root = { first => { path => { remove => 1, keep => 2 } } };
for $topic_root {
    .{ * ; * }.map: { .<remove>:delete }
}

is-deeply $topic_root, { first => { path => { keep => 2 } } },
    'a topic-root associative slice preserves writeback through map';
