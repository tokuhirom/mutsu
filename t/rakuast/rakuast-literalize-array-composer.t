use Test;

# #16529: rakudo literalizes `[ EXPR ]` as the literal of EXPR (so a single
# element is the element itself, `[1]` is `1`) and a comma list as the List of
# its literalized operands; a nested one-element array is its element too.
# Every shape below was measured on rakudo 2026.09.

plan 20;

sub lit(Str $src) { $src.AST.statements[0].expression.literalize }

is lit('[]').elems, 0, 'an empty array literalizes to an empty List';
is lit('[]').^name, 'List', 'which is a List';
is lit('[1]'), 1, 'a one-element array is its element';
is lit('[ "a" ]'), 'a', 'a string element';
is lit('[1, 2]').raku, '(1, 2)', 'two elements are a List';
is lit('[[1]]'), 1, 'nesting a one-element array unwraps both';
is lit('[[1], 2]').raku, '(1, 2)', 'a nested one-element array is its element';
is lit('[1, [2, 3]]').raku, '(1, (2, 3))', 'a nested two-element array stays a List';
is lit('[1, [2, [3]]]').raku, '(1, (2, 3))', 'unwrapping works at any depth';
is lit('[(1, 2)]').raku, '(1, 2)', 'a parenthesized list inside is kept';
is lit('[(1,)]').raku, '(1,)', 'including a one-element one';
is lit('[(1, 2), 3]').raku, '((1, 2), 3)', 'as an element of a longer list';
is lit('[[1, 2], [3, 4]]').raku, '((1, 2), (3, 4))', 'two nested lists';
is lit('[[1], [2]]').raku, '(1, 2)', 'two nested one-element arrays';
is lit('[[]]').elems, 0, 'an array of an empty array is the empty List';
is lit('[{a => 1}]').^name, 'Map', 'a one-element array of a hash is the Map';
is lit('[{a => 1}, 2]').raku, '(Map.new((:a(1))), 2)', 'a hash among elements';
is lit('(1, [2])').raku, '(1, 2)', 'a paren list unwraps a one-element array';
is lit('[1, 2, 3]').raku, '(1, 2, 3)', 'three elements';
is lit('[[[1]]]'), 1, 'three levels';
