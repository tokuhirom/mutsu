use experimental :rakuast;
use Test;

# A Pod block written anywhere inside a statement but in quoted text (after an
# operator, after `=`, between call arguments) is whitespace to rakudo's
# reader: the block becomes a `Doc::Block` after the statement. A heredoc's
# body is its text, even though it follows the statement (#14880, measured on
# rakudo 2026.11).

plan 7;

sub shape(Str $src) {
    $src.AST.statements.map({
        .^name.subst('RakuAST::', '') ~ '@' ~ (.origin ?? .origin.from ~ '-' ~ .origin.to !! '-')
    }).join(' ')
}

is shape("my \$x = 1 +\n=begin pod\nhi\n=end pod\n2;\nsay \$x;\n"),
    "Statement::Expression\@0-36 Doc::Block\@12-34 Statement::Expression\@38-44",
    'after a binary operator';
is shape("my \$x =\n=begin pod\nhi\n=end pod\n2;\nsay 1;\n"),
    "Statement::Expression\@0-32 Doc::Block\@8-30 Statement::Expression\@34-39",
    'after `=`';
is shape("say 1,\n=for comment\nhi\n\n2;\nsay 3;\n"),
    "Statement::Expression\@0-25 Doc::Block\@7-22 Statement::Expression\@27-32",
    'between call arguments, `=for`';
is shape("my \$y = 1 +\n=begin pod\nhi\n=end pod\n2 + 3;\n"),
    "Statement::Expression\@0-40 Doc::Block\@12-34",
    'the statement ends after the block';
is shape("say (1 +\n=head1 T\n\n2);\n"),
    "Statement::Expression\@0-21 Doc::Block\@9-17",
    'inside parentheses';
is shape("say \"a\n=begin pod\nb\n=end pod\n\";\n"),
    "Statement::Expression\@0-30",
    'in a string the lines are text';
is shape("my \$s = q:to/END/;\na\n=begin pod\nhi\n=end pod\nb\nEND\nprint \$s;\n"),
    "Statement::Expression\@0-17 Statement::Expression\@50-58",
    'in a heredoc body the lines are text';
