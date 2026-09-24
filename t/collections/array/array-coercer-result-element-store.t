use Test;

# #9208: `X.method[i] = v` on a plain (non-instance) receiver is raku's
# `(X.method)[i] = v`. The store lands in whatever container the method
# returned: the receiver's own (`.self`, `.list`, `%h.Hash`) is updated, a
# fresh copy (`.Array`, `.clone`) takes the store and is discarded, and an
# immutable return (`.List`) is refused. mutsu used to die on every method but
# `.self`, and `.Array` handed back the receiver itself instead of a copy.

plan 14;

{
    my $p = [1, 2];
    lives-ok { $p.Array[1] = 8 }, '$p.Array[1] = 8 lives';
    is-deeply $p, [1, 2], 'and the store lands in the discarded copy';
}

{
    my @a = 1, 2;
    @a.Array[0] = 5;
    is-deeply @a, [1, 2], '@a.Array[0] = 5 leaves @a alone';
    @a.Array[5] = 8;
    is-deeply @a, [1, 2], 'an out-of-range store into the copy does not grow @a';
    my $b = @a.Array;
    $b.push(3);
    is-deeply @a, [1, 2], '.Array is a fresh Array: pushing to it does not reach @a';
    nok @a.Array =:= @a, '@a.Array is not the same container as @a';
}

{
    my $p = [1, 2];
    $p.clone[1] = 8;
    is-deeply $p, [1, 2], '.clone[i] = v stores into the clone';
    my %h = a => 1;
    %h.clone<k> = 1;
    is-deeply %h, {a => 1}, '%h.clone<k> = v stores into the clone';
}

{
    my %h = a => 1;
    %h.Hash<k> = 1;
    is-deeply %h, {a => 1, k => 1}, '%h.Hash is %h itself, so the store reaches it';
    my $p = [1, 2];
    $p.list[0] = 9;
    is-deeply $p, [9, 2], '.list on an Array is the Array itself';
    $p.self[1] = 7;
    is-deeply $p, [9, 7], '.self still stores in place (#9197)';
}

throws-like { (1, 2).List[0] = 5 }, X::Assignment::RO,
    'a store into an immutable List still dies';
throws-like { my $p = [1, 2]; $p.List[0] = 9 }, X::Assignment::RO,
    '.List on an Array returns an immutable List, which refuses the store';

{
    my Int @t = 1, 2;
    is-deeply @t.Array, [1, 2], 'a typed array coerces to a plain Array';
}
