use Test;

# Pod blocks between the statements of a unit are `RakuAST::Doc::Block`
# statements of its `.AST` (#14880), shaped as rakudo 2026.09 shapes them: a
# delimited block runs to its `=end`, an extended or abbreviated one to the
# next blank line or directive, a paragraph keeps the blank lines after it, and
# `head1` is a `head` of level 1. Blocks with configuration, markup or indented
# lines are not read yet and stay out of the tree.

plan 13;

use experimental :rakuast;

sub flat-statements(Str $src) {
    $src.AST.statements.map(*.raku.subst(/\s+/, ' ', :g)).join("\n")
}

is flat-statements("=begin pod\nfirst line\nsecond line\n\nanother para\n=end pod\nsay 1;\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( \"first line\\nsecond line\\n\\n\", \"another para\\n\", ) )\nRakuAST::Statement::Expression.new( expression => RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier(\"say\"), args => RakuAST::ArgList.new( RakuAST::IntLiteral.new(1) ) ) )",
    "plain delimited block, two paragraphs";
is flat-statements("=begin pod\n=head1 Title\n\ntext\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( type => \"head\", level => 1, abbreviated => True, paragraphs => ( \"Title\\n\\n\", ) ), \"text\\n\", ) )",
    "an abbreviated block inside a delimited one";
is flat-statements("=head2 Sub title\ncontinued\n\n=item1 one\n=item2 two\n"),
    "RakuAST::Doc::Block.new( type => \"head\", level => 2, abbreviated => True, paragraphs => ( \"Sub title\\ncontinued\\n\\n\", ) )\nRakuAST::Doc::Block.new( type => \"item\", level => 1, abbreviated => True, paragraphs => ( \"one\\n\", ) )\nRakuAST::Doc::Block.new( type => \"item\", level => 2, abbreviated => True, paragraphs => ( \"two\\n\", ) )",
    "abbreviated blocks: a level, the next directive ends one";
is flat-statements("=head1 A\nsay 1;\n"),
    "RakuAST::Doc::Block.new( type => \"head\", level => 1, abbreviated => True, paragraphs => ( \"A\\nsay 1;\\n\", ) )",
    "an abbreviated block takes the lines after it";
is flat-statements("=for foo\na\nb\n\n=head1 c\n"),
    "RakuAST::Doc::Block.new( type => \"foo\", for => True, paragraphs => ( \"a\\nb\\n\\n\", ) )\nRakuAST::Doc::Block.new( type => \"head\", level => 1, abbreviated => True, paragraphs => ( \"c\\n\", ) )",
    "an extended block";
is flat-statements("=begin foo\n=end foo\n=foo\n=item\nx\n"),
    "RakuAST::Doc::Block.new( type => \"foo\" )\nRakuAST::Doc::Block.new( type => \"foo\", abbreviated => True )\nRakuAST::Doc::Block.new( type => \"item\", abbreviated => True, paragraphs => ( \"x\\n\", ) )",
    "an empty delimited block and a bare directive";
is flat-statements("=begin pod\n\n\na\n\n\n\nb  \n   \nc\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( \"a\\n\\n\\n\\n\", \"b \\n\\n\", \"c\\n\", ) )",
    "blank lines after a paragraph are part of it";
is flat-statements("=begin pod\n=begin item\ninner\n=end item\ntail\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( type => \"item\", paragraphs => ( \"inner\\n\", ) ), \"tail\\n\", ) )",
    "nested delimited block";
is flat-statements("say 1;\n=head1 X\n\nsay 2;\n=begin pod\nz\n=end pod\nsay 3;\n"),
    "RakuAST::Statement::Expression.new( expression => RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier(\"say\"), args => RakuAST::ArgList.new( RakuAST::IntLiteral.new(1) ) ) )\nRakuAST::Doc::Block.new( type => \"head\", level => 1, abbreviated => True, paragraphs => ( \"X\\n\\n\", ) )\nRakuAST::Statement::Expression.new( expression => RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier(\"say\"), args => RakuAST::ArgList.new( RakuAST::IntLiteral.new(2) ) ) )\nRakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( \"z\\n\", ) )\nRakuAST::Statement::Expression.new( expression => RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier(\"say\"), args => RakuAST::ArgList.new( RakuAST::IntLiteral.new(3) ) ) )",
    "blocks between statements";
is flat-statements("=NAME x\n=for pod\nfoo\n"),
    "RakuAST::Doc::Block.new( type => \"NAME\", abbreviated => True, paragraphs => ( \"x\\n\", ) )\nRakuAST::Doc::Block.new( type => \"pod\", for => True, paragraphs => ( \"foo\\n\", ) )",
    "uppercase names";

# The ranges of the blocks and of the unit's list.
sub ranges(Str $src) {
    my $ast = $src.AST;
    ($ast.origin, |$ast.statements.map(*.origin)).map({ .defined ?? "{.from}..{.to}" !! '-' }).join(' ')
}
is ranges("say 1;\n=begin pod\nz\n=end pod\nsay 3;\n=head1 A\n\nsay 4;\n"),
    '0..52 0..5 7..28 29..34 36..44 46..51', 'the origins of the blocks and of the list';
is ranges("=begin pod\nx\n=end pod\n"), '0..21 0..21', 'a unit of just a block';

# A hand-built or round-tripped tree still runs: a Pod block runs nothing.
ok EVAL(Q[sub f { 1 }
=begin pod
x
=end pod
f()
].AST) == 1, 'EVAL of a tree with a block';
