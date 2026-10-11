use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

sub expression($source) { $source.AST.statements.head.expression }
sub attribute($source) { expression($source).body.body.statement-list.statements.head.expression }
sub same($source, $expected, $description) { is EVAL($source.AST), $expected, $description }

my @attrs;
for Q[class CShape1 { has %.h{Str} }], Q[class CShape2 { has Any %.h{Str} }],
    Q[class CShape3 { has Int %.h{Str} }], Q[class CShape4 { has Int %!h{Str} }] -> $source {
    my $node = attribute($source);
    @attrs.push($node);
    isa-ok $node, RakuAST::VarDeclaration::Simple, 'keyed attribute uses the shared declaration node';
    isa-ok $node.shape, RakuAST::SemiList, 'key type is the attribute shape';
    isa-ok $node.shape.statements.head.expression, RakuAST::Type::Simple, 'shape holds a type';
    like $node.shape.raku, /'"Str"'/, 'key type name is retained';
}
nok @attrs[0].type.defined, 'implicit Any is absent from source';
ok @attrs[1].type.defined, 'explicit Any is retained';
same Q[class CExec1 { has Int %.h{Str} }; my $c = CExec1.new; $c.h<a> = 4; $c.h<a>], 4,
    'keyed attribute executes through the shared compiler';
same Q[class CExec2 { has %.h{Str} }; my $c = CExec2.new; $c.h<a> = 'word'; $c.h<a>], 'word',
    'omitted value type remains unconstrained';
dies-ok { EVAL(Q[class CExec3 { has Int %.h{Str} }; my $c = CExec3.new; $c.h<a> = 'word'].AST) },
    'keyed attribute preserves value checks';

my $coercion = expression(Q[my Int:D(Str) $n = '2']).type;
isa-ok $coercion, RakuAST::Type::Coercion, 'outer coercion node';
isa-ok $coercion.base-type, RakuAST::Type::Definedness, 'coercion target carries definedness';
ok $coercion.base-type.definite, 'coercion target is definite';
same Q[my Int:D(Str) $n = '2'; $n + 1], 3, 'coercion of a definite target survives';
my $returns = expression(Q[sub f(--> Hash:D[Int:D]) { }]).signature.returns;
isa-ok $returns, RakuAST::Type::Definedness, 'parameterized return carries outer definedness';
isa-ok $returns.base-type, RakuAST::Type::Parameterized, 'definedness wraps the type application';
isa-ok $returns.base-type.args.args.head, RakuAST::Type::Definedness,
    'type application retains definedness on its argument';
same Q[sub f(--> Hash:D[Int:D]) { my Int:D %h = a => 2; %h }; f()<a>], 2,
    'parameterized definite return executes';

same Q[class Type-Name {}; my Type-Name $x = Type-Name.new; $x.^name], 'Type-Name',
    'hyphenated type works in a variable constraint';
same Q[module Types { class Version-info {} }; sub f(--> Types::Version-info) { Types::Version-info.new }; f().^name],
    'Types::Version-info', 'qualified hyphenated return type survives';
same Q[class Doesn't {}; sub f(Doesn't $x) { $x.^name }; f(Doesn't.new)], "Doesn't",
    'apostrophe inside a type name survives';

my $loop = Q[loop (my Int $i = 0; $i < 2; $i++) {}].AST.statements.head;
isa-ok $loop.setup, RakuAST::VarDeclaration::Simple, 'typed loop setup is a declaration';
isa-ok $loop.setup.type, RakuAST::Type::Simple, 'loop setup keeps its type';
same Q[my $sum = 0; loop (my Int $i = 0; $i < 4; $i++) { $sum += $i }; $sum], 6,
    'typed loop setup survives execution';
same Q[class Élément {}; my Élément $x = Élément.new; $x.^name], 'Élément',
    'Unicode type names use the parser identifier grammar';
my $nested = expression(Q[my Array[Int](Cool) $items]).type;
isa-ok $nested, RakuAST::Type::Coercion, 'coercion can wrap a parameterized target';
isa-ok $nested.base-type, RakuAST::Type::Parameterized, 'parameterized coercion base survives';
my $constructed-type = RakuAST::Type::Coercion.new(
    base-type => RakuAST::Type::Definedness.new(
        base-type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
        definite => True),
    constraint => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str')));
my $built-value = EVAL(RakuAST::VarDeclaration::Simple.new(
    type => $constructed-type, sigil => '$',
    desigilname => RakuAST::Name.from-identifier('constructed'),
    initializer => RakuAST::Initializer::Assign.new(RakuAST::StrLiteral.new('7'))));
is $built-value, 7, 'constructed composed type lowers through the existing declaration compiler';
isa-ok $built-value, Int, 'constructed declaration applies its supplied coercion';
my Int:D(Str) $direct = '8';
is $direct, 8, 'ordinary typed declarations resolve a definite coercion target';
my $apostrophe-args = expression(Q[sub f(--> Hash[Doesn't, Int]) {}]).signature.returns.args.args;
is $apostrophe-args.elems, 2, 'an apostrophe in a type argument does not consume the next argument';
my $built-attribute = RakuAST::VarDeclaration::Simple.new(
    scope => 'has', type => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Int')),
    shape => RakuAST::SemiList.new(RakuAST::Statement::Expression.new(
        expression => RakuAST::Type::Simple.new(RakuAST::Name.from-identifier('Str')))),
    sigil => '%', twigil => '.', desigilname => RakuAST::Name.from-identifier('entries'));
my $built-class = EVAL(RakuAST::Class.new(
    name => RakuAST::Name.from-identifier('BuiltKeyShape'),
    body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $built-attribute))))));
my $built-object = $built-class.new;
$built-object.entries<a> = 11;
is $built-object.entries<a>, 11, 'constructed attribute shape compiles and executes';
dies-ok { $built-object.entries<a> = 'wrong' }, 'constructed attribute retains its value constraint';
dies-ok { $built-object.entries{42} = 12 }, 'constructed attribute retains its key constraint';
my $optional-fields = RakuAST::VarDeclaration::Simple.new(
    type => Nil, shape => Nil, scope => 'my', sigil => '$',
    desigilname => RakuAST::Name.from-identifier('optional'));
nok $optional-fields.type.defined, 'Nil omits an optional constructed type';
nok $optional-fields.shape.defined, 'Nil omits an optional constructed shape';
is RakuAST::Class.new.body.body.statement-list.statements.elems, 0,
    'a constructed class defaults to an empty block';
my $multi-shape = attribute(Q[class MultiShape { has Int %.h{Str;Str} }]).shape;
is $multi-shape.statements.elems, 2, 'each hash key dimension retains its own type node';
isa-ok $multi-shape.statements[1].expression, RakuAST::Type::Simple,
    'later key dimensions are source type expressions';
same Q[my %h{Str:D;Str:D}; %h{'a';'b'} = 6; %h{'a';'b'}], 6,
    'multidimensional variable key shapes survive execution';
same Q[class MultiExec { has %.h{Str;Str} }; my $c = MultiExec.new;
    $c.h{'a';'b'} = 9; $c.h{'a';'b'}], 9,
    'multidimensional attribute key shapes survive execution';
done-testing;
