use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# The written control signature must survive independently of its executable
# binding scaffolding. Measure shape as well as EVAL, including false branches.
my $unless = Q[unless 0 -> \index { index + 7 }].AST.statements[0];
isa-ok $unless, RakuAST::Statement::Unless, 'unless retains its written keyword';
isa-ok $unless.body, RakuAST::PointyBlock, 'unless retains its pointy signature';
isa-ok $unless.body.signature.parameters[0].target,
    RakuAST::ParameterTarget::Term, 'unless sigilless parameter is a term';
is EVAL(Q[do unless 0 -> \index { index + 7 }].AST), 7,
    'unless binds the original false value';

my $orwith = Q[with Any { 0 } orwith 5 -> Int $value { $value + 1 }].AST.statements[0];
isa-ok $orwith.elsifs[0], RakuAST::Statement::Orwith, 'orwith keeps its own clause';
isa-ok $orwith.elsifs[0].then, RakuAST::PointyBlock, 'orwith keeps its signature';
isa-ok $orwith.elsifs[0].then.signature.parameters[0].type,
    RakuAST::Type::Simple, 'orwith retains its parameter type';
is EVAL(Q[do with Any { 0 } orwith 5 -> Int $value { $value + 1 }].AST), 6,
    'typed orwith executes after lowering';
is EVAL(Q[do with Any { 0 } orwith 5 -> \index { index + 2 }].AST), 7,
    'orwith sigilless term is resolved in the body';
is EVAL(Q[do with Any { 0 } orwith (3, 4) -> ($a, $b) { $a + $b }].AST), 7,
    'orwith destructures one condition value';

my $loop = Q[while (1, 2) -> ($a, $b) { last }].AST.statements[0];
isa-ok $loop, RakuAST::Statement::Loop::While, 'while expansion remains one loop';
isa-ok $loop.body, RakuAST::PointyBlock, 'while has the written pointy block';
is $loop.body.signature.parameters[0].sub-signature.parameters.elems, 2,
    'while retains both destructuring leaves';
is EVAL(Q[
    my $i = 0;
    my @out;
    while (++$i < 4 ?? ($i, $i * 10) !! Empty) -> ($a, $b) {
        @out.push($a + $b);
    }
    @out.join(',')
].AST), '11,22,33', 'while destructures each iteration value';
is EVAL(Q[
    my $i = 0;
    my @out;
    while ++$i < 4 -> \index { @out.push(index) }
    @out.join(',')
].AST), 'True,True,True', 'while sigilless parameters bind on each iteration';
is EVAL(Q[
    my $i = 0;
    my @out;
    until ++$i > 3 -> \index { @out.push(index) }
    @out.join(',')
].AST), 'False,False,False', 'until binds the unnegated condition';

is EVAL(Q[do if 0 -> \index { 9 } else -> \join { join + 8 }].AST), 8,
    'else reads through a sigilless then binding';
is EVAL(Q[do if 0 { 9 } else -> Int $value { $value + 8 }].AST), 8,
    'else can create the condition binding itself';
is EVAL(Q[do if 0 { 9 } elsif 0 { 10 } else -> Int $value { $value + 8 }].AST), 8,
    'else binds the last elsif condition';
is EVAL(Q[do with Any { 9 } else -> \value { value.^name }].AST), 'Any',
    'with else retains its sigilless signature';
is EVAL(Q[do with Any { 9 } orwith Int { 10 } else -> \value { value.^name }].AST), 'Int',
    'orwith else binds the last tested value';

is EVAL(Q[do with (1, 2, 3) -> @items { @items.elems }].AST), 3,
    'with aggregate parameter sees all elements';
is EVAL(Q[do with Any { 0 } orwith (1, 2, 3) -> @items { @items.elems }].AST), 3,
    'orwith aggregate parameter sees all elements';
is EVAL(Q[
    my $calls = 0;
    sub candidate { ++$calls; 4 }
    my $value = do with Any { 0 } orwith candidate() -> $x { $x + 1 };
    "$value/$calls"
].AST), '5/1', 'orwith evaluates an effectful condition once';

my $constructed = RakuAST::Statement::Unless.new(
    condition => RakuAST::IntLiteral.new(0),
    body => RakuAST::PointyBlock.new(
        signature => RakuAST::Signature.new(parameters => (
            RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(name => '$value')),
        )),
        body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
            RakuAST::Statement::Expression.new(expression => RakuAST::Var::Lexical.new('$value'))
        ))
    )
);
is EVAL($constructed), 0, 'constructed unless uses the ordinary clause binder';

my $body = Q[-> $value { $value }].AST.statements[0].expression;
my $false = RakuAST::IntLiteral.new(0);
my $true = RakuAST::IntLiteral.new(1);
is EVAL(RakuAST::Statement::If.new(condition => $true, then => $body)), 1,
    'constructed if uses a pointy signature';
is EVAL(RakuAST::Statement::With.new(condition => $true, then => $body)), 1,
    'constructed with uses a pointy signature';
my $undefined = Q[Any].AST.statements[0].expression;
is EVAL(RakuAST::Statement::Without.new(condition => $undefined, body => $body)).^name,
    'Any', 'constructed without binds the undefined condition';
is EVAL(RakuAST::Statement::If.new(
    condition => $false,
    then => $body,
    elsifs => (RakuAST::Statement::Elsif.new(condition => $true, then => $body),)
)), 1, 'constructed elsif retains its pointy signature';
is EVAL(RakuAST::Statement::With.new(
    condition => $undefined,
    then => $body,
    elsifs => (RakuAST::Statement::Orwith.new(condition => $true, then => $body),)
)), 1, 'constructed orwith retains its pointy signature';
lives-ok { EVAL(RakuAST::Statement::Loop::While.new(condition => $false, body => $body)) },
    'constructed while accepts a pointy body';
lives-ok { EVAL(RakuAST::Statement::Loop::Until.new(condition => $true, body => $body)) },
    'constructed until accepts a pointy body';

is EVAL(Q[my $matches = ('a b' ~~ m:g/(\w+)/); $matches.elems].AST), 2,
    'initializer parentheses retain multi-match result semantics';
is EVAL(Q[
    my @values = 1, 2;
    my $calls = 0;
    my $answer = do with Any { 0 } orwith (++$calls == 1 ?? @values !! Any) -> @v {
        @v.elems
    };
    "$answer/$calls"
].AST), '2/1', 'an orwith aggregate argument uses the once-evaluated value';
is EVAL(Q[
    my $calls = 0;
    sub candidate { ++$calls; Int }
    my $answer = do with Any { 0 } orwith candidate() { 1 } else -> \value { value.^name };
    "$answer/$calls"
].AST), 'Int/1', 'orwith else does not evaluate the failed condition again';

is EVAL(Q[
    my $i = 0;
    my @out;
    while (++$i < 5 ?? ($i, $i * 10) !! Empty) -> ($a, $b) {
        next if $a == 2;
        last if $a == 4;
        @out.push($b);
    }
    @out.join(',')
].AST), '10,30', 'destructuring retains loop next and last control';

done-testing;
