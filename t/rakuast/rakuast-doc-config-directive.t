use Test;

# The `=config` directive (#14880): a `Doc::Block` of type `config` with
# `directive => True`, the pairs as its config and the configured block type
# as its only paragraph. Shapes and ranges measured on rakudo 2026.09.

plan 6;

use experimental :rakuast;

sub block(Str $src) { $src.AST.statements[0].raku.subst(/\s+/, ' ', :g).subst('RakuAST::', '', :g) }

is block("=config head1 :numbered\n=head1 T\n"),
    q[Doc::Block.new( type => "config", directive => True, config => ( :numbered(Term::Enum.from-identifier('True')), ), paragraphs => ( "head1", ) )],
    'a flag';
is block("=config head1 :!numbered :level(2)\n"),
    q[Doc::Block.new( type => "config", directive => True, config => ( :numbered(Term::Enum.from-identifier('False')), :level(Circumfix::Parentheses.new( SemiList.new( Statement::Expression.new( expression => IntLiteral.new(2) ) ) )), ), paragraphs => ( "head1", ) )],
    'a negated flag and a parenthesised value';

my $ast = "=config head1 :numbered\n=head1 Title\nsay 1;\n=config item :x<y z>\n".AST;
is $ast.statements.elems, 3, 'a directive is a statement of its own';
is $ast.statements[0].type, 'config', 'its type';
is $ast.statements[0].directive, True, 'its directive flag';
is $ast.statements.map({ .origin ?? "{.origin.from}-{.origin.to}" !! '-' }).join(' '),
    '0-23 24-43 44-64',
    'the ranges of the statements';
