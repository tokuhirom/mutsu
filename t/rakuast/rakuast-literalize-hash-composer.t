use Test;

# `{a => 1}` literalizes to a `Map` of its constant pairs, as on rakudo
# 2026.09 (#14916); a `%(...)` contextualizer and a hash with a non-constant
# value do not literalize.

plan 6;

sub lit(Str $src) { $src.AST.statements[0].expression.literalize }

is lit('{a => 1}').raku, 'Map.new((:a(1)))', 'a one-pair hash composer';
is lit('{a => 1}').^name, 'Map', 'is a Map, not a Hash';
is lit('{a => 1, b => 2}').raku, 'Map.new((:a(1),:b(2)))', 'two pairs';
is lit('{a => 1 + 1}').raku, 'Map.new((:a(2)))', 'a constant expression value';
is lit('{a => "x"}').raku, 'Map.new((:a("x")))', 'a string value';
is lit('%(a => 1)').^name, 'RakuAST::Node::CannotLiteralize', 'a contextualizer does not';
