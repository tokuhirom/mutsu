use Test;

# ADR-11276 slice 3: Map's value, key/value and pair views use handler rows.
# Hash inherits these rows through Map, and object hashes retain their key
# objects in each key-bearing view.

plan 5;

subtest 'Map views', {
    plan 4;
    my $map = Map.new(a => 1, b => 2);
    is-deeply $map.values.sort.List, (1, 2).List, 'values';
    is-deeply $map.kv.map(*.Str).sort.List, ('1', '2', 'a', 'b').List,
        'key/value sequence';
    is-deeply $map.pairs.map({ .key.Str ~ '=' ~ .value.Str }).sort.List,
        <a=1 b=2>, 'pairs';
    is-deeply $map.antipairs.map({ .key.Str ~ '=' ~ .value.Str }).sort.List,
        <1=a 2=b>, 'antipairs';
}

subtest 'Hash inherits Map views', {
    plan 4;
    my %hash = a => 1, b => 2;
    is-deeply %hash.values.sort.List, (1, 2).List, 'values';
    is-deeply %hash.kv.map(*.Str).sort.List, ('1', '2', 'a', 'b').List,
        'key/value sequence';
    is-deeply %hash.pairs.map({ .key.Str ~ '=' ~ .value.Str }).sort.List,
        <a=1 b=2>, 'pairs';
    is-deeply %hash.antipairs.map({ .key.Str ~ '=' ~ .value.Str }).sort.List,
        <1=a 2=b>, 'antipairs';
}

subtest 'object hashes retain original keys', {
    plan 3;
    my %object{Any} = 1 => 'one';
    is %object.kv[0].^name, 'Int', '.kv keeps the typed key';
    is %object.pairs.head.key.^name, 'Int', '.pairs keeps the typed key';
    is %object.antipairs.head.value.^name, 'Int', '.antipairs keeps the typed key';
}

my $map = Map.new(a => 1, b => 2);
is-deeply (^3).map({ $map.values.elems }).List, (2, 2, 2),
    'a repeated call site returns the Map values';
ok Map.^can('values') && Map.^can('kv') && Map.^can('pairs') && Map.^can('antipairs'),
    'the Map method table exposes each view';
