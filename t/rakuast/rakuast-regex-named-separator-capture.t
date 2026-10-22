use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 9;

my $body = Q|/("XX")+ %% $<delim>=<[a..z]>*/|.AST.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::WithWhitespace,
    'the source retains whitespace before the separator';
isa-ok $body.regex, RakuAST::Regex::QuantifiedAtom,
    'the separator belongs to the quantified atom';
isa-ok $body.regex.separator, RakuAST::Regex::NamedCapture,
    'the separator retains its named capture';
is $body.regex.separator.name, 'delim', 'the capture name is retained';
isa-ok $body.regex.separator.regex, RakuAST::Regex::QuantifiedAtom,
    'the zero-width-capable separator body remains quantified';
is-deeply $body.regex.trailing-separator, True, 'the trailing separator is allowed';

is EVAL(Q[grammar G { regex TOP { ("XX")+ %% $<delim>=<[a..z]>* } }; G.parse("XXXXXX").caps.map(*.key).raku].AST),
    '(0, "delim", 0, "delim", 0, "delim").Seq',
    'round-tripped grammar executes each zero-width named separator';
is EVAL(Q[grammar H { regex TOP { ("XX")+ % $<sep>="," } }; H.parse("XX,XX").caps.map(*.key).raku].AST),
    '(0, "sep", 0).Seq',
    'a required named separator captures between repetitions';
is EVAL(Q[grammar J { regex TOP { ("XX")+ %% $<sep>="," } }; J.parse("XX,XX,").caps.map(*.key).raku].AST),
    '(0, "sep", 0, "sep").Seq',
    'a trailing named separator keeps its capture';
