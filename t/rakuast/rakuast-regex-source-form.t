use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 14;

sub expr($source) { $source.AST.statements.head.expression }

my $rx = expr(Q[rx/a/]);
isa-ok $rx, RakuAST::QuotedRegex, '`rx//` remains a quoted regex node';
isa-ok $rx.body, RakuAST::Regex::Literal, '`rx//` keeps its literal body';
is $rx.body.text, 'a', '`rx//` body text is retained';
is $rx.gist,
    "RakuAST::QuotedRegex.new(\n  body => RakuAST::Regex::Literal.new(\"a\")\n)",
    '`rx//` has Rakudo’s measured AST shape';
is EVAL(Q[rx/a/].AST).gist, 'rx/a/', 'round-trip evaluation retains the `rx//` value form';

my $slash = expr(Q[/a/]);
isa-ok $slash, RakuAST::QuotedRegex, 'slash regex remains a quoted regex node';
isa-ok $slash.body, RakuAST::Regex::Literal, 'slash regex keeps its literal body';
is $slash.body.text, 'a', 'slash regex body text is retained';
is $slash.gist,
    "RakuAST::QuotedRegex.new(\n  body => RakuAST::Regex::Literal.new(\"a\")\n)",
    'slash regex has the same measured RakuAST shape';
is EVAL(Q[/a/].AST).gist, '/a/', 'round-trip evaluation retains the slash regex value form';

is EVAL(Q[rx:m/a/].AST).gist, 'rx:m/a/', 'round-trip retains source adverbs on an `rx//` value';
ok 'ä' ~~ EVAL(Q[rx:m/a/].AST), 'round-trip retains `:m` matching semantics';

is EVAL(Q[rx/a/].AST).WHAT, Regex, '`rx//` still evaluates to a Regex';
is EVAL(Q[/a/].AST).WHAT, Regex, 'slash regex still evaluates to a Regex';
