use Test;
use experimental :rakuast;

# A Pod block written inside a parenthesised or bracketed list is read by rakudo and
# comes out as a `Doc::Block` statement after the statement that holds the list; the
# same lines inside a string are text. Measured on Rakudo 2026.11 (#14880).

plan 12;

sub names(Str $code) { $code.AST.statements.map(*.^name.split('::').tail(2).join('::')).join(' ') }

is names("my \@a = (1,\n=begin pod\nhi\n=end pod\n2);\nsay 1;\n"),
    'Statement::Expression Doc::Block Statement::Expression', 'a delimited block in a paren list';
is names("my \@a = [1,\n=begin pod\nhi\n=end pod\n2];\n"),
    'Statement::Expression Doc::Block', 'in a bracketed list';
is names("my \%h = (a => 1,\n=begin pod\nhi\n=end pod\nb => 2);\n"),
    'Statement::Expression Doc::Block', 'in a hash list';
is names("my \@a = (1,\n=for pod\nhi\n\n2);\n"),
    'Statement::Expression Doc::Block', 'a paragraph block';
is names("my \@a = (1,\n=head1 Foo\n\n2);\n"),
    'Statement::Expression Doc::Block', 'an abbreviated block';
is names("my \$s = \"a\n=begin pod\nb\n=end pod\n\";\n"),
    'Statement::Expression', 'the same lines in a string are text';

my $block = "my \@a = (1,\n=begin pod\nhi\n=end pod\n2);\n".AST.statements[1];
is $block.type, 'pod', 'the block keeps its type';
is $block.paragraphs.join, "hi\n", 'and its text';

my $in-sub = "sub f \{\n  my \@a = (1,\n=begin pod\nhi\n=end pod\n2);\n  1\n\}\n".AST;
my @inner = $in-sub.statements[0].expression.body.statement-list.statements.map(*.^name.split('::').tail(2).join('::'));
is @inner.join(' '), 'Statement::Expression Doc::Block Statement::Expression',
    'the block is placed in the sub body\'s statement list';

my $code = "my \@a = (1,\n=begin pod\nhi\n=end pod\n2);\n";
my $ast = $code.AST;
is $ast.statements.elems, 2, 'two statements';
is $ast.statements[0].^name, 'RakuAST::Statement::Expression', 'the declaration first';
is $ast.statements[1].^name, 'RakuAST::Doc::Block', 'then the block';
