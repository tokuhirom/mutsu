use Test;

# `RakuAST::Node.rakudoc` (#14916): the documentation nodes of a tree as a Seq,
# walked pre-order over the descendants (the node itself is not visited): a
# `Doc::Block` as it is, a declaration with `#|` / `#=` docs as a
# `Doc::Declarator`. Shapes measured on rakudo 2026.09.

plan 12;

use experimental :rakuast;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }

my $ast = Q"#| doc foo
sub foo { 1 }
=begin pod
x
=end pod
class C { #| m
  method m { } #= tm
}
".AST;

my @docs = $ast.rakudoc;
is @docs.elems, 3, 'the unit holds three documentation nodes';
is @docs.map(*.^name).join(' '),
    'RakuAST::Doc::Declarator RakuAST::Doc::Block RakuAST::Doc::Declarator', 'in source order';
isa-ok $ast.rakudoc, Seq, 'a Seq';
is @docs[0].leading.raku, '("doc foo",)', 'leading';
is @docs[0].trailing.elems, 0, 'no trailing';
is @docs[2].leading.raku ~ @docs[2].trailing.raku, '("m",)("tm",)', 'leading and trailing';
is @docs[0].WHEREFORE.^name, 'RakuAST::Sub', 'WHEREFORE is the declaration';
is $ast.statements[0].rakudoc.elems, 1, 'a statement holds its declaration docs';
is $ast.statements[0].expression.rakudoc.elems, 0, 'the node itself is not visited';
is $ast.statements[1].rakudoc.elems, 0, 'a Doc::Block holds none';
is Q"1".AST.rakudoc.elems, 0, 'an undocumented unit';

my $d = RakuAST::Doc::Declarator.new(
    WHEREFORE => RakuAST::Sub.new(name => RakuAST::Name.from-identifier("f")),
    leading => ("a", "b"), trailing => ("c",));
is flat($d.raku).substr(0, 60 ), 'RakuAST::Doc::Declarator.new( WHEREFORE => RakuAST::Sub.new(', 'constructor';
