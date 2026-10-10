use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# Normalized execution flags cannot recover the written order. These pins
# exercise the source record and the existing compiled trait application path.
sub routine(Str $source) { $source.AST.statements[0].expression }
sub names($node) { $node.traits.map({ .isa(RakuAST::Trait::Is) ?? .name.canonicalize !! .^name }).join(',') }

is names(routine(Q[sub f() is rw is export { 1 }])), 'rw,export', 'rw before export';
is names(routine(Q[sub f() is export is rw { 1 }])), 'export,rw', 'export before rw';
is names(routine(Q[sub f() is raw is export { 1 }])), 'raw,export', 'raw before export';
is names(routine(Q[sub f() is export is raw { 1 }])), 'export,raw', 'export before raw';
is names(routine(Q[sub f() returns Int is rw { 1 }])), 'RakuAST::Trait::Returns,rw', 'return trait before flag';
is names(routine(Q[sub f() is rw returns Int { 1 }])), 'rw,RakuAST::Trait::Returns', 'flag before return trait';
ok routine(Q[sub f() is export(:DEFAULT) { 1 }]).traits[0].argument.defined, 'explicit DEFAULT retains its argument';
is routine(Q[sub f() returns Positional of Int { (1,) }]).traits.elems, 1, 'of parameterizes the return trait';
like routine(Q[sub f() returns Positional of Int { (1,) }]).traits[0].gist,
    /'Type::Parameterized'/, 'parameterized return type is represented structurally';

is EVAL(Q[sub f() is export is rw { state $x = 1; $x }; f() = 7; f()].AST), 7, 'rw and export survive lowering';
is EVAL(Q[sub f() returns Int is export { 4 }; f()].AST), 4, 'return and export survive lowering';
is EVAL(Q[sub f() returns Positional of Int { my Int @x = 1, 2; @x }; f().sum].AST), 3, 'parameterized returns survives lowering';

my $method = Q[class C { method m() is rw is DEPRECATED("old") { state $x = 2; $x } }].AST.statements[0].expression.body.body.statement-list.statements[0].expression;
is names($method), 'rw,DEPRECATED', 'method flag and deprecation stay ordered';
is EVAL(Q[class D { method m() returns Int is rw { state $x = 2; $x } }; my $c = D.new; $c.m = 9; $c.m].AST), 9, 'method flags and return trait survive lowering';

my $defs = Q[multi trait_mod:<is>(Routine $r, :$tagged!) { $r.wrap(-> |c { $tagged.join('+') ~ callsame() }) }; ];
my $tree = ($defs ~ Q[sub f() is tagged("x") is export returns Str { "v" }; f()]).AST;
is names($tree.statements[1].expression), 'tagged,export,RakuAST::Trait::Returns', 'custom trait mixed with builtins';
is EVAL($tree), 'xv', 'custom trait argument survives lowering';
is EVAL(($defs ~ Q[my $f = sub () returns Str is tagged("a") { "b" }; $f()]).AST), 'ab', 'anonymous routine retains custom and return traits';
is EVAL(Q[my $f = sub (Int $x) returns Int is rw { $x }; $f(8)].AST), 8, 'anonymous rw routine retains return trait';

my $proto = ($defs ~ Q[proto sub f(|) is tagged("p") returns Str {*}; multi sub f(Int) { "i" }; f(1)]).AST;
is names($proto.statements[1].expression), 'tagged,RakuAST::Trait::Returns', 'proto retains written traits';
is EVAL($proto), 'pi', 'proto custom trait applies to the dispatcher';
is EVAL(($defs ~ Q[proto sub g(|) is tagged("q") returns Str {*}; multi sub g(Int) { "j" }; &g.returns.^name]).AST),
    'Str', 'proto return trait reaches the execution signature';
is EVAL(($defs ~ Q[sub words() is tagged<x y> is export { "v" }; words()]).AST),
    'x+yv', 'angle-word custom argument survives alongside a builtin trait';
is EVAL(($defs ~ Q[my $f = sub () is tagged<z> returns Str { "v" }; $f()]).AST),
    'zv', 'anonymous angle-word trait argument survives lowering';
is EVAL(Q[class Holder { }; my $m = method (Int $x) returns Int is rw { $x }; Holder.new.$m(6)].AST),
    6, 'anonymous method retains return and flag traits';

for Q[sub infix:<+++>($a, $b) is assoc<list> is equiv(&infix:<+>) { $a + $b }],
    Q[sub infix:<+++>($a, $b) is equiv(&infix:<+>) is assoc<list> { $a + $b }] -> $source {
    my @expected = $source.index('is assoc') < $source.index('is equiv') ?? <assoc equiv> !! <equiv assoc>;
    is names(routine($source)), @expected.join(','), 'associativity and precedence retain written order';
}

my $parens = routine(Q[sub infix:<++++>($a, $b) is assoc("left") is equiv(&infix:<+>) { $a + $b }]);
isa-ok $parens.traits[0].argument, RakuAST::Circumfix::Parentheses, 'parenthesized associativity argument retains its form';
is EVAL(Q[sub infix:<++++>($a, $b) is assoc("left") is equiv(&infix:<+>) { $a + $b }; 2 ++++ 3].AST),
    5, 'parenthesized associativity and precedence survive lowering';
like routine(Q[sub infix:<plus>($a,$b) is equiv(&[+]) { $a + $b }]).traits[0].gist,
    /'&infix:<+>'/, 'bracket operator reference uses its canonical lexical name';
is EVAL(Q[sub infix:<plus>($a,$b) is equiv(&[+]) is assoc<left> { $a + $b }; 2 plus 3].AST),
    5, 'bracket operator precedence reference survives lowering';
is EVAL(Q[sub infix:<plus>($a,$b) is tighter<+> { $a + $b }; 2 plus 3].AST),
    5, 'angle-word precedence reference survives lowering';

is names(routine(Q[sub annotated() is nodal { 7 }])), 'nodal',
    'recognized annotation retains its source trait';
is EVAL(Q[sub annotated() is nodal { 7 }; annotated()].AST), 7,
    'recognized annotation lowers without becoming a custom trait call';
is EVAL(Q[proto sub raw-dispatch(|) is raw {*}; multi sub raw-dispatch(Int $x) { $x + 1 }; raw-dispatch(4)].AST), 5,
    'raw proto annotation does not prevent candidate dispatch';

done-testing;
