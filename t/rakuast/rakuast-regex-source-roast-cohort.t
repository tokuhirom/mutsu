use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 20;

my token wc { \w }
my $body = Q[/<wc=&wc>/].AST.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::Assertion::Alias,
    'a callable subrule alias has an Alias node';
is $body.name, 'wc', 'the alias keeps its capture name';
isa-ok $body.assertion, RakuAST::Regex::Assertion::Callable,
    'the aliased assertion remains callable';
isa-ok $body.assertion.callee, RakuAST::Var::Lexical,
    'a named callable retains its lexical variable';
is $body.assertion.callee.name, '&wc', 'the callee keeps its sigil';

is Q[/<wc=&wc()>/].AST.statements[0].expression.body.gist, $body.gist,
    'an empty argument list has the same tree';
my $round = 'a' ~~ EVAL(Q[/<wc=&wc>/].AST);
ok $round.defined, 'the round-tripped alias matches';
is ~$round<wc>, 'a', 'it captures under the alias name';

my $tok = token { \w };
my $scalar = Q[/<alias=&$tok>/].AST.statements[0].expression.body;
isa-ok $scalar.assertion.callee, RakuAST::Contextualizer::Item,
    'a scalar holding a callable is item-contextualized';
is ~('b' ~~ EVAL(Q[/<alias=&$tok>/].AST))<alias>, 'b',
    'the scalar callable alias lowers and matches';

my $constructed = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Assertion::Alias.new(
        name => 'wc',
        assertion => RakuAST::Regex::Assertion::Callable.new(
            callee => RakuAST::Var::Lexical.new('&wc'),
        ),
    ),
);
is ~('c' ~~ EVAL($constructed))<wc>, 'c',
    'a hand-built callable alias lowers through EVAL';
is ~('d' ~~ /<wc=&wc>/)<wc>, 'd',
    'ordinary regex execution keeps callable alias capture semantics';

my $anonymous = Q!/$'x'/!.AST.statements[0].expression.body;
isa-ok $anonymous, RakuAST::Regex::Sequence,
    'a bare scalar before a regex quote forms a sequence';
isa-ok $anonymous.terms[0], RakuAST::Regex::Interpolation,
    'the first term is an interpolation';
isa-ok $anonymous.terms[0].var, RakuAST::VarDeclaration::Anonymous,
    'it refers to an anonymous scalar';
is $anonymous.terms[0].var.scope, 'state', 'the anonymous scalar has state scope';
is $anonymous.terms[0].var.sigil, '$', 'the anonymous scalar keeps its sigil';
isa-ok EVAL(Q!/$'x'/!.AST), Regex, 'a regex with that interpolation lowers through EVAL';
is Q!/$"x"/!.AST.statements[0].expression.body.gist, $anonymous.gist,
    'a double-quoted atom has the same anonymous interpolation';
isa-ok EVAL(Q!/$"x"/!.AST), Regex, 'the double-quoted spelling also lowers';
