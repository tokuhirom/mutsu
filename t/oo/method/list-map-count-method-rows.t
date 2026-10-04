use Test;

# ADR-11276 slice 3: `keys`, `Numeric` and `Int` on `List` and `Map`, and
# `chars` on `Cool`, are rows in the built-in method table. Calls on a
# variable are answered by the call-site lane; the loops run each site
# repeatedly.

plan 8;

subtest 'List.keys', {
    plan 4;
    my @a = 3, 4, 5;
    is-deeply @a.keys, (0, 1, 2).Seq, 'an Array';
    is-deeply (1, 2).keys, (0, 1).Seq, 'a List';
    is-deeply [].keys, ().Seq, 'empty';
    is-deeply (^3).map({ @a.keys.elems }).List, (3, 3, 3), 'one site, repeated';
}

subtest 'Map.keys', {
    plan 3;
    my %h = a => 1, b => 2;
    is-deeply %h.keys.sort.List, <a b>, 'a Hash';
    is-deeply Map.new((x => 1)).keys, ("x",).Seq, 'a Map';
    my %o{Any} = 1 => "a";
    is-deeply %o.keys.map({ .^name }).List, ("Int",), 'an object hash keeps its key objects';
}

subtest 'Numeric and Int count the elements', {
    plan 6;
    my @a = 3, 4, 5;
    my %h = a => 1, b => 2;
    is-deeply @a.Numeric, 3, 'Array.Numeric';
    is-deeply @a.Int, 3, 'Array.Int';
    is-deeply (1, 2).Numeric, 2, 'List.Numeric';
    is-deeply %h.Numeric, 2, 'Hash.Numeric';
    is-deeply %h.Int, 2, 'Hash.Int';
    is-deeply (^3).map({ %h.Int }).List, (2, 2, 2), 'one site, repeated';
}

subtest 'Cool.chars stringifies', {
    plan 6;
    my $i = 12345;
    is-deeply $i.chars, 5, 'Int';
    is-deeply 1.5.chars, 3, 'Rat';
    is-deeply (1/7).chars, 8, 'a Rat rendered to six places';
    is-deeply [1, 2, 3].chars, 5, 'an Array as "1 2 3"';
    is-deeply %(a => 1).chars, 3, 'a Hash as "a\t1"';
    is-deeply (^3).map({ $i.chars }).List, (5, 5, 5), 'one site, repeated';
}

is-deeply "aé\x[1F600]".chars, 3, 'Str.chars counts graphemes';

{
    my @z = 1, 2, 3;
    my $k = @z.keys;
    is-deeply $k.List, (0, 1, 2), '.keys of an Array';
}

is-deeply (1..4).keys.List, (0, 1, 2, 3), 'a Range keeps its own keys';

ok List.^can('keys') && Map.^can('Int') && Cool.^can('chars'), 'introspection sees the methods';
