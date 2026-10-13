use v6;
use Test;

# Regex constructs whose tree was missing, so a regex holding one refused to
# round-trip. Measured on rakudo 2026.09:
#   <|b> <|c> <|g>       Assertion::Pass (the boundaries hold at every position)
#   <!|w> / <?|w>        Assertion::Lookahead(Assertion::Named(wb))
#   <!|b>                Assertion::Lookahead(negated, Assertion::Pass)
#   :i<[ab]>             InternalModifier::IgnoreCase, then the char class
#   a*:?                 Quantifier::ZeroOrMore(backtrack => Backtrack::Frugal)
#   @$ref                Interpolation(var => Contextualizer::List(Var::Lexical))
#   <rx={$r}>            Assertion::Alias(assertion => InterpolatedBlock)
#   <v(/\d+/)>           Assertion::Named::Args(args => ArgList(QuotedRegex))

plan 26;

sub body($src) {
    my $body = $src.AST.statements[0].expression.body;
    $body
}

# Read direction.
is body(Q[/<|b>/]).^name, 'RakuAST::Regex::Assertion::Pass', '<|b> is Assertion::Pass';
is body(Q[/<|c>/]).^name, 'RakuAST::Regex::Assertion::Pass', '<|c> is Assertion::Pass';
my $not-wb = body(Q[/<!|w>/]);
is $not-wb.^name, 'RakuAST::Regex::Assertion::Lookahead', '<!|w> is a lookahead';
ok $not-wb.negated, '... negated';
is $not-wb.assertion.name.canonicalize, 'wb', '... over wb';
my $not-b = body(Q[/<!|b>/]);
is $not-b.assertion.^name, 'RakuAST::Regex::Assertion::Pass', '<!|b> is a lookahead over Pass';
my $mod = body(Q[/:i<[ab]>/]);
is $mod.^name, 'RakuAST::Regex::Sequence', ':i<[ab]> is a sequence';
is $mod.gist.lines.grep(/'InternalModifier::IgnoreCase' | 'Assertion::CharClass'/).elems, 2,
    '... of the modifier and the char class';
like body(Q[/a*:?/]).gist, /'backtrack => RakuAST::Regex::Backtrack::Frugal'/,
    '*:? is a frugal quantifier';
my $ref = body(Q[/@$ref/]);
is $ref.^name, 'RakuAST::Regex::Interpolation', '@$ref is an interpolation';
is $ref.var.^name, 'RakuAST::Contextualizer::List', '... in list context';
like $ref.var.gist, /'RakuAST::Var::Lexical.new("\$ref")'/, '... of the scalar';
my $alias = body(Q[/<rx={$r}>/]);
is $alias.^name, 'RakuAST::Regex::Assertion::Alias', '<rx={$r}> is an alias';
is $alias.assertion.^name, 'RakuAST::Regex::Assertion::InterpolatedBlock', '... over a block';

# EVAL direction and semantics: the round-tripped regex matches as the parsed one.
ok 'ab' ~~ EVAL(Q[/a<|b>b/].AST), 'EVAL of <|b> always passes';
nok 'ab' ~~ EVAL(Q[/a<!|b>b/].AST), 'EVAL of <!|b> always fails';
is ~('xaab' ~~ EVAL Q[/a<!|w>/].AST), 'a', 'EVAL of <!|w> is no word boundary';
nok 'a b' ~~ EVAL(Q[/a<?|w>b/].AST), 'EVAL of <?|w> needs a boundary';
is ~('xA' ~~ EVAL Q[/x:i<[ab]>/].AST), 'xA', 'EVAL of :i<[ab]> ignores case';
is ~('aaa' ~~ EVAL Q[/a*:? a/].AST), 'a', 'EVAL of *:? is frugal';
my $arr = [<St Strict>];
is ~('StrictX' ~~ EVAL Q[/@$arr/].AST), 'Strict', 'EVAL of @$ref takes the longest';
is ~('ab' ~~ EVAL Q[/ $( my $x = 'a'; $x ~ 'b' ) /].AST), 'ab',
    'EVAL of a multi-statement $( ) interpolates its last value';
my $r = /\d+/;
is ~('a12' ~~ EVAL Q[/<rx={$r}>/].AST)<rx>, '12', 'EVAL of <rx={$r}> files the alias';
is ~('a5' ~~ EVAL Q[/ a <?before <[0..9] + [x]> > /].AST), 'a',
    'a compound class in a lookahead';
is ~('a5' ~~ / a <?before <-[\x[22]]>> /), 'a', 'a bracketed escape in a lookahead class';

grammar G {
    token TOP { <v(/\d+/)> }
    token v($x) { <$x> }
}
is ~G.parse('12')<v>, '12', 'a regex literal as a subrule argument';
