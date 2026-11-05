use Test;

plan 7;

sub lit(Str $src) {
    my $n = $src.AST.statements[0].expression;
    (try $n.literalize.raku) // "ERR: " ~ $!.^name;
}

# An array inside a hash value literalizes by the same rule as a top-level
# one: `[ EXPR ]` is the literal of EXPR, a comma list is a List.
is lit('{a => [1]}'),           'Map.new((:a(1)))', 'single-element array unwraps';
is lit('{a => [1, 2]}'),        'Map.new((:a((1, 2))))', 'two elements stay a List';
is lit('{a => [1, [2, 3]]}'),   'Map.new((:a((1, (2, 3)))))', 'nested array inside a list';
is lit('{a => (1, 2)}'),        'Map.new((:a((1, 2))))', 'parenthesised list';
is lit('{a => {b => [1]}}'),    'Map.new((:a(Map.new((:b(1))))))', 'nested hash';
is lit('[{a => [1]}]'),         'Map.new((:a(1)))', 'array of one hash';
is lit('{a => [1, {b => 2}]}'), 'Map.new((:a((1, Map.new((:b(2)))))))', 'hash inside a list';
