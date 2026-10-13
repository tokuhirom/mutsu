use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

plan 15;

my $ast = Q[class RakuAstGroupedAttributes { has Int ($.x, $.y) is rw }].AST;
my $declaration = $ast.statements[0].expression.body.body.statement-list.statements[0].expression;
isa-ok $declaration, RakuAST::VarDeclaration::Signature,
    'a parenthesized has declaration is one signature declaration';
is $declaration.scope, 'has', 'the grouped declaration keeps attribute scope';
is $declaration.signature.parameters.elems, 2,
    'each listed attribute becomes a signature parameter';
is $declaration.type.name.canonicalize, 'Int', 'the shared type remains on the declaration';

my $type = EVAL($ast);
my $object = $type.new(x => 1, y => 2);
is $object.x, 1, 'the first grouped attribute is installed';
is $object.y, 2, 'the second grouped attribute is installed';
$object.x = 8;
is $object.x, 8, 'the group trait remains applied after the RakuAST round trip';

my $defaulted = EVAL(Q[class RakuAstGroupedDefaults { has Numeric ($.x = 9, $.y = 10) }].AST).new;
is $defaulted.x, Numeric, 'a per-member default stays ignored as in Rakudo';
is $defaulted.y, Numeric, 'each typed member retains its type-object default';

my $colon-ast = Q[class RakuAstAttributeColon { has Int $.gid: has Str @.m }].AST;
my $colon-declaration = $colon-ast.statements[0].expression.body.body.statement-list.statements[0].expression;
isa-ok $colon-declaration, RakuAST::VarDeclaration::Simple,
    'a colon arglist keeps the initiating attribute as its visible declaration';
is $colon-declaration.desigilname.canonicalize, 'gid',
    'the visible declaration retains its source name';
my $colon-type = EVAL($colon-ast);
is $colon-type.^attributes.elems, 2,
    'a declaration nested in the colon arglist is still installed';
is $colon-type.new(gid => 3, m => <x y>).m, Array[Str].new(<x y>),
    'the hidden grouped declaration retains its typed attribute behavior';

my $chain-type = EVAL(Q[class RakuAstAttributeColonChain { has $.x: has $.y: has $.z }].AST);
is $chain-type.^attributes.elems, 3, 'chained colon arglists retain every nested declaration';
is $chain-type.new(z => 7).z, 7, 'the last chained declaration remains constructible';
