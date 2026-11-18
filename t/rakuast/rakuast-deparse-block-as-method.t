use Test;
use experimental :rakuast;

plan 9;

# `.&{ ... }` calls a block as a method; rakudo itemizes the block, so the
# `Call::TermAsMethod` callee is `Contextualizer::Item(Block)` (#14845).
sub dep($src) { $src.AST.DEPARSE.lines.tail.trim }

is dep('my @a; my $x = @a.&{ .elems };'), 'my $x = @a.&{ .elems }', 'a block as a method';
is dep('my @a; my $x = @a>>.&{ .elems };'), 'my $x = @a>>.&{ .elems }', 'hyper';
is dep('my @a; my $y = @a.&{ $_ }.elems;'), 'my $y = @a.&{ $_ }.elems', 'chained';

my $callee = 'my @a; @a.&{ .elems }'.AST.find-nodes(RakuAST::Call::TermAsMethod)[0].callee;
isa-ok $callee, RakuAST::Contextualizer::Item, 'the callee is itemized';
isa-ok $callee.target, RakuAST::Block, 'around the block';

is EVAL('my @a = <a bb ccc>; @a.&{ .elems }'.AST), 3, 'EVAL of the AST calls the block on the invocant';
is EVAL('my @a = <a bb ccc>; (@a>>.&{ .chars }).raku'.AST), '[1, 2, 3]', 'hyper EVAL';
is EVAL('my $x = 5; $x.&{ $_ * 2 }'.AST), 10, 'the invocant is the topic';
is dep('my $f = sub { 1 }; my $x = 3.$f;'), 'my $x = 3.$f', 'a variable callee is not itemized';
