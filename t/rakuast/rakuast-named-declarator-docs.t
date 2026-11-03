use Test;

# `#|` / `#=` comments documenting NAMED declarations are printed by rakudo as
# a trailing `.declarator-docs(...)` call on the declaration node (#14880),
# the same shape an anonymous sub carries.

plan 6;

use experimental :rakuast;

sub flat(Str $s) { $s.subst(/\s+/, ' ', :g) }
sub stmt-raku(Str $src, Int $n = 0) { flat($src.AST.statements[$n].raku) }

is stmt-raku(Q"#| doc for foo
sub foo { 1 }
"),
    'RakuAST::Statement::Expression.new( expression => RakuAST::Sub.new( name => RakuAST::Name.from-identifier("foo"), body => RakuAST::Blockoid.new( RakuAST::StatementList.new( RakuAST::Statement::Expression.new( expression => RakuAST::IntLiteral.new(1) ) ) ) ).declarator-docs( leading => ( "doc for foo", ) ) )',
    'a leading doc on a sub';
ok stmt-raku(Q"sub baz { 3 } #= after
").ends-with('.declarator-docs( trailing => ( "after", ) ) )'), 'a trailing doc on a sub';
unlike stmt-raku(Q"sub plain { 1 }
"), /'declarator-docs'/, 'an undocumented sub has none';

my $c = flat(Q"#| class doc
class C { #| attr doc
  has $.x; #= trailing x
  #| meth
  method m { } }
".AST.statements[0].raku);
ok $c.contains('.declarator-docs( leading => ( "class doc", ) )'), 'a class';
ok $c.contains('.declarator-docs( leading => ( "attr doc", ), trailing => ( "trailing x", ) )'), 'an attribute with both';
ok $c.contains('.declarator-docs( leading => ( "meth", ) )'), 'a method';
