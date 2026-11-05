use Test;

# `EVAL` of a hand-built tree holding `RakuAST::Doc::Block` statements
# establishes `$=pod` (#14880), as in rakudo 2026.09: the blocks become the
# `Pod::Block`s the same text would have read from a source string.

plan 9;

use experimental :rakuast;

# The Pod the tree's `$=pod` holds: one line per block, its class and the
# class of each content item.
sub pod-of($block) {
    my $ast = RakuAST::StatementList.new($block,
        RakuAST::Statement::Expression.new(expression => RakuAST::Var::Doc.new("pod")));
    EVAL $ast
}
sub shape($pod) {
    $pod.map(-> $b { $b.^name ~ "(" ~ $b.contents.map(*.^name).join(",") ~ ")" }).join(" ")
}
sub texts($pod) { $pod.map(-> $b { $b.contents.map(*.contents.join).join("|") }).join(" ; ") }

my $plain = pod-of(RakuAST::Doc::Block.new(type => "pod", paragraphs => ("hello\n",)));
is $plain.elems, 1, 'one block';
is shape($plain), 'Pod::Block::Named(Pod::Block::Para)', 'a delimited pod block holds a paragraph';
is texts($plain), "hello", 'its text';

my $head = pod-of(RakuAST::Doc::Block.new(type => "head", level => 1, abbreviated => True, paragraphs => ("Title\n",)));
is shape($head), 'Pod::Heading(Pod::Block::Para)', 'an abbreviated head is a Pod::Heading';
is $head[0].level, 1, 'its level';

my $two = pod-of(RakuAST::Doc::Block.new(type => "foo", paragraphs => ("a\n", "b\n")));
is shape($two), 'Pod::Block::Named(Pod::Block::Para,Pod::Block::Para)', 'two paragraphs';
is texts($two), "a|b", 'their texts';

my $nested = pod-of(RakuAST::Doc::Block.new(type => "pod", paragraphs => (
    RakuAST::Doc::Block.new(type => "head", level => 2, abbreviated => True, paragraphs => ("T\n",)), "text\n")));
is shape($nested), 'Pod::Block::Named(Pod::Heading,Pod::Block::Para)', 'a nested block';

my $none = RakuAST::StatementList.new(RakuAST::Statement::Expression.new(expression => RakuAST::Var::Doc.new("pod")));
is (EVAL $none).elems, 0, 'a tree without a block has an empty $=pod';
