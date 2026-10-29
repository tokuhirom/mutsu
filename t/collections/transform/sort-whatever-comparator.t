use Test;

plan 9;

# A bare `*` is not a comparator: no `sort` candidate takes a Whatever, so the
# method throws X::Multi::NoMatch (the routine form sorts the list `*, ...`).
my @a = 3, 1, 2;
my @cases = ('List', (3, 1, 2)), ('Array', [3, 1, 2]), ('array variable', @a),
    ('Seq', (3, 1, 2).Seq), ('lazy map', (3, 1, 2).map(* + 0)), ('Range', 1..3);
for @cases -> ($name, $value) {
    my $threw = '';
    try { $value.sort(*); CATCH { default { $threw = .^name } } }
    is $threw, 'X::Multi::NoMatch', "{$name}.sort(*)";
}

throws-like { (3, 1, 2).sort(*) }, X::Multi::NoMatch, message => /'sort(List:D: Whatever:D)'/,
    'the message names the receiver type and the Whatever argument';
is-deeply (3, 1, 2).sort(-*).list, (3, 2, 1), 'a WhateverCode comparator still sorts';
is-deeply (3, 1, 2).sort.list, (1, 2, 3), 'sort without arguments';
