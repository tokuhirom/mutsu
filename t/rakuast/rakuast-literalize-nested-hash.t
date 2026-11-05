use Test;

# #14916: a hash nested anywhere in a literalized constant is a `Map` too
# (not only the outermost one), and a nested array is a `List`. Measured on
# rakudo 2026.09.

plan 6;

sub lit(Str $src) { $src.AST.statements[0].expression.literalize }

is lit('{a => {b => 1}}').raku, 'Map.new((:a(Map.new((:b(1))))))',
    'a hash in a hash value is a Map';
is lit('{a => {b => 1}}')<a>.^name, 'Map', 'the inner value is a Map';
is lit('(1, {a => 2})').raku, '(1, Map.new((:a(2))))', 'a hash in a list';
is lit('{a => [1, {b => 2}]}').raku, 'Map.new((:a((1, Map.new((:b(2)))))))',
    'a hash in an array in a hash value';
is lit('{a => [1, 2]}')<a>.^name, 'List', 'a nested array is a List';
is lit('[{a => 1}, 2]').raku, '(Map.new((:a(1))), 2)', 'a hash in an array composer';
