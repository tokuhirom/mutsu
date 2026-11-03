use Test;

# Pod blocks between the statements of a unit are `RakuAST::Doc::Block`
# statements of its `.AST` (#14880), shaped as rakudo 2026.09 shapes them: a
# delimited block runs to its `=end`, an extended or abbreviated one to the
# next blank line or directive, a paragraph keeps the blank lines after it, and
# `head1` is a `head` of level 1, and `:name(...)` configuration is kept as the
# value nodes. Indented paragraphs are implicit code blocks (`margin`). Blocks with markup
# are not read yet and stay out of the tree.

plan 29;

use experimental :rakuast;

# The margins of the implicit code blocks directly inside the first block.
sub margins(Str $src) {
    $src.AST.statements[0].paragraphs.grep(RakuAST::Doc::Block).map(*.margin.raku).join(' ')
}

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

is flat-statements("=begin pod :flag :!off :n(3) :s('str') :w<a b> :d(\"x\")\nx\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", config => ( :flag(RakuAST::Term::Enum.from-identifier('True')), :off(RakuAST::Term::Enum.from-identifier('False')), :n(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::IntLiteral.new(3) ) ) )), :s(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new(\"str\"), ) ) ) ) )), :w(RakuAST::QuotedString.new( processors => <words val>, segments => ( RakuAST::StrLiteral.new(\"a b\"), ) )), :d(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new(\"x\"), ) ) ) ) )), ), paragraphs => ( \"x\\n\", ) )",
    "configuration: flags, numbers, strings and word quotes";
is flat-statements("=begin foo :kind(\"Type\") :category<basic>\nx\n=end foo\nsay 1;\n=for foo :a(1+2)\nx\n"),
    "RakuAST::Doc::Block.new( type => \"foo\", config => ( :kind(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new(\"Type\"), ) ) ) ) )), :category(RakuAST::QuotedString.new( processors => <words val>, segments => ( RakuAST::StrLiteral.new(\"basic\"), ) )), ), paragraphs => ( \"x\\n\", ) )\nRakuAST::Statement::Expression.new( expression => RakuAST::Call::Name::WithoutParentheses.new( name => RakuAST::Name.from-identifier(\"say\"), args => RakuAST::ArgList.new( RakuAST::IntLiteral.new(1) ) ) )\nRakuAST::Doc::Block.new( type => \"foo\", for => True, config => ( :a(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::ApplyInfix.new( left => RakuAST::IntLiteral.new(1), infix => RakuAST::Infix.new(\"+\"), right => RakuAST::IntLiteral.new(2) ) ) ) )), ), paragraphs => ( \"x\\n\", ) )",
    "configuration of a delimited and an extended block";
is flat-statements("=begin pod :a(1,2) :b('a)b')\nx\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", config => ( :a(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::ApplyListInfix.new( infix => RakuAST::Infix.new(\",\"), operands => ( RakuAST::IntLiteral.new(1), RakuAST::IntLiteral.new(2), ) ) ) ) )), :b(RakuAST::Circumfix::Parentheses.new( RakuAST::SemiList.new( RakuAST::Statement::Expression.new( expression => RakuAST::QuotedString.new( segments => ( RakuAST::StrLiteral.new(\"a)b\"), ) ) ) ) )), ), paragraphs => ( \"x\\n\", ) )",
    "configuration with an argument list and a bracket in a string";
is flat-statements("=begin pod :a<x>\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", config => ( :a(RakuAST::QuotedString.new( processors => <words val>, segments => ( RakuAST::StrLiteral.new(\"x\"), ) )), ) )",
    "an empty block with configuration";

is flat-statements("=begin pod\n  indented line\n  more\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( margin => \" \", type => \"implicit-code\", paragraphs => ( \"indented line\\nmore\\n\", ) ), ) )",
    "an indented paragraph is implicit code; the margin is its first line's indentation";
is margins("=begin pod\n  indented line\n  more\n=end pod\n"), "\"  \"", 'the margins';
is flat-statements("=begin pod\ntext\n\n  code here\n  and here\n\nafter\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( \"text\\n\\n\", RakuAST::Doc::Block.new( margin => \" \", type => \"implicit-code\", paragraphs => ( \"code here\\nand here\\n\\n\", ) ), \"after\\n\", ) )",
    "code after text, a deeper line keeps what is beyond the margin";
is margins("=begin pod\ntext\n\n  code here\n  and here\n\nafter\n=end pod\n"), "\"  \"", 'the margins';
is flat-statements("=begin pod\n  a\n\n    b deeper\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( margin => \" \", type => \"implicit-code\", paragraphs => ( \"a\\n\\n b deeper\\n\", ) ), ) )",
    "a blank line inside code, then a deeper line";
is margins("=begin pod\n  a\n\n    b deeper\n=end pod\n"), "\"  \"", 'the margins';
is flat-statements("=begin pod\n    four\n  two\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( margin => \" \", type => \"implicit-code\", paragraphs => ( \"four\\n\", ) ), RakuAST::Doc::Block.new( margin => \" \", type => \"implicit-code\", paragraphs => ( \"two\\n\", ) ), ) )",
    "a less indented line starts another block";
is margins("=begin pod\n    four\n  two\n=end pod\n"), "\"    \" \"  \"", 'the margins';
is flat-statements("=begin pod\ntext\n  indented continuation\nmore\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( \"text\\nindented continuation\\nmore\\n\", ) )",
    "an indented line inside a text paragraph is joined to it";
is margins("=begin pod\ntext\n  indented continuation\nmore\n=end pod\n"), "", 'the margins';
is flat-statements("=begin pod\n\ttab\n\tmore\n=end pod\n"),
    "RakuAST::Doc::Block.new( type => \"pod\", paragraphs => ( RakuAST::Doc::Block.new( margin => \"\\t\", type => \"implicit-code\", paragraphs => ( \"tab\\nmore\\n\", ) ), ) )",
    "tab indentation";
is margins("=begin pod\n\ttab\n\tmore\n=end pod\n"), "\"\\t\"", 'the margins';

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
