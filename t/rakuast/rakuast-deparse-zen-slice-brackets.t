use Test;
use experimental :rakuast;

plan 12;

# A zen slice keeps the brackets it was written with (#14845).
sub dep($src) { $src.AST.DEPARSE.lines.tail.trim }

is dep('my @a; my $y = @a[];'), 'my $y = @a[]', '[] is an array index';
is dep('my @a; my $y = @a{};'), 'my $y = @a{}', '{} on an array';
is dep('my @a; my $y = @a<>;'), 'my $y = @a<>', '<> on an array';
is dep('my %h; my $y = %h<>;'), 'my $y = %h<>', '<> on a hash';
is dep('my %h; my $y = %h{};'), 'my $y = %h{}', '{} on a hash';
is dep('my $y = *{};'), 'my $y = *{}', '{} on a whatever';

my $ast = 'my @a; @a<>'.AST;
is $ast.find-nodes(RakuAST::Postcircumfix::LiteralHashIndex).elems, 1, '<> is a LiteralHashIndex';
is $ast.find-nodes(RakuAST::Postcircumfix::HashIndex).elems, 0, 'and no HashIndex';

is EVAL('my @a = 1,2; @a<>.elems'.AST), 2, 'EVAL of <> is still the zen slice, not a lookup of the key ""';
is EVAL('my @a = 3,4; @a{}.raku'.AST), '[3, 4]', 'EVAL of {} is the zen slice';

# The brackets do not change what a zen slice does: `*{}` still curries.
is (*{}).^name, 'WhateverCode', '*{} is a WhateverCode';
is EVAL('(*<>)(<a b>)'.AST).raku, '("a", "b")', 'EVAL of a curried zen slice';
