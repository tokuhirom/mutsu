use Test;
use experimental :rakuast;

# #16783: `WHY` of a parsed declaration answers the Doc::Declarator built from
# its `#|` / `#=` comments, one list element per comment, with the declaration
# as `WHEREFORE`. mutsu answered the type object, and joined several comments
# into one text.

plan 18;

my $ast = Q[#| hello
#| more
sub f {} #= trail
#| cls
class C {
  #| meth
  method m {} #= mt
  #| attr
  has $.a;
}
#| role
role R {}
#| mod
module M {}
#| gram
grammar G {}
sub g {}
].AST;

my @top = $ast.statements.map(*.expression);
my $sub = @top[0];
ok $sub.WHY.defined, 'a documented sub has a WHY';
isa-ok $sub.WHY, RakuAST::Doc::Declarator, 'it is a Doc::Declarator';
is $sub.WHY.leading.raku, '("hello", "more")', 'each #| comment is one element';
is $sub.WHY.trailing.raku, '("trail",)', 'the #= comment is the trailing list';
is $sub.WHY.WHEREFORE.^name, 'RakuAST::Sub', 'WHEREFORE is the declaration';

is @top[1].WHY.leading.raku, '("cls",)', 'class';
is @top[1].WHY.trailing.raku, '()', 'a class without #= has an empty trailing list';
is @top[2].WHY.leading.raku, '("role",)', 'role';
is @top[3].WHY.leading.raku, '("mod",)', 'module';
is @top[4].WHY.leading.raku, '("gram",)', 'grammar';
nok @top[5].WHY.defined, 'an undocumented sub answers the type object';
is @top[5].WHY.^name, 'RakuAST::Doc::Declarator', 'which is the Doc::Declarator type object';

my ($method) = $ast.find-nodes(RakuAST::Method);
is $method.WHY.leading.raku ~ $method.WHY.trailing.raku, '("meth",)("mt",)', 'method';
my ($attr) = $ast.find-nodes(RakuAST::VarDeclaration::Simple);
is $attr.WHY.leading.raku, '("attr",)', 'attribute';

is $sub.raku.lines.skip(5).join('|'),
    ').declarator-docs(|  leading  => (|    "hello",|    "more",|  ),|  trailing => (|    "trail",|  )|)',
    'the declaration prints each comment as its own line';
is $sub.WHY.raku.lines.head(2).join('|'), 'RakuAST::Doc::Declarator.new(|  WHEREFORE => RakuAST::Sub.new(',
    'the doc prints its declaration';

# a hand-built WHY still wins over the comments
my $own = RakuAST::Doc::Declarator.new(WHEREFORE => RakuAST::Var::Lexical.new('$x'), leading => ('own',));
my $s2 = RakuAST::Sub.new(name => RakuAST::Name.from-identifier('f'), WHY => $own);
is $s2.WHY.leading.raku, '("own",)', 'a stored WHY is returned as given';
# a single comment line is still one element
my $one = Q[#| just one
sub h {}].AST.statements[0].expression;
is $one.WHY.leading.raku, '("just one",)', 'one comment is a one-element list';
