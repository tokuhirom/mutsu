use v6.e.PREVIEW;
use experimental :rakuast;
use Test;

# The text from a `=finish` line on is `$=finish`, not a Pod block of the
# statement list (#14880): `.AST` leaves it out, even on the first line.

plan 7;

sub kinds(Str $src) { $src.AST.statements.map(*.^name).List }

is-deeply kinds("say 1;\n=finish\nthe rest\n"), ('RakuAST::Statement::Expression',),
    'text after =finish is not a statement';
is-deeply kinds("say 1;\n=finish\n"), ('RakuAST::Statement::Expression',),
    'an empty =finish section adds nothing';
is-deeply kinds("=finish\nonly this\n"), (),
    '=finish on the first line leaves no statements';
is-deeply kinds("say 1;\n=begin pod\nTitle\n=end pod\n=finish\nrest\n"),
    ('RakuAST::Statement::Expression', 'RakuAST::Doc::Block'),
    'a Pod block before =finish is still a Doc::Block';
is kinds("say 1;\n=finish\nsay 2;\n").elems, 1, 'code after =finish is not parsed';
is-deeply "say 1;\n=finish\nx\n".AST.statements.map(*.raku).List.elems, 1,
    'one statement';
is-deeply kinds("=begin pod\nhi\n=end pod\nsay 1;\n=finish\nz\n"),
    ('RakuAST::Doc::Block', 'RakuAST::Statement::Expression'),
    'Pod before the code and =finish after';
