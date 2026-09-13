use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: ordinary scalar interpolation is retained in the
# shared RegexTree, exposed through RakuAST, and lowered to the existing
# match-time VarInterp atom. Array/hash interpolation, code interpolation,
# named aliases, and subrules remain separate boundaries.

plan 12;

my $x = 'X';
my $rx = /a $x b/;
ok 'aXb' ~~ $rx, 'a parser-created regex reads its scalar interpolation';
$x = 'Y';
ok 'aYb' ~~ $rx, 'the stored regex reads the current lexical value';
nok 'aXb' ~~ $rx, 'the stored regex no longer matches the old value';

my $interpolated = EVAL(RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::Sequence.new(
        RakuAST::Regex::Literal.new('a'),
        RakuAST::Regex::Interpolation.new(
            sequential => False,
            var => RakuAST::Var::Lexical.new(q[$x]),
        ),
        RakuAST::Regex::Literal.new('b'),
    ),
));
ok 'aYb' ~~ $interpolated, 'a constructed interpolation lowers through EVAL';
$x = 'Z';
ok 'aZb' ~~ $interpolated, 'a constructed interpolation stays match-time dynamic';

my $case = 'rex';
ok 'Rex' ~~ m:i/$case/, 'scalar interpolation honors regex ignorecase';

my $hash = { a => 1 };
throws-like { 'a' ~~ /$hash/ }, X::Syntax::Reserved,
    'hash interpolation keeps its reserved syntax error';

grammar GRegexInterpolated {
    token x { a $x b }
}
ok 'aZb' ~~ &GRegexInterpolated::x,
    'a token declaration retains scalar interpolation provenance';
$x = 'Q';
ok 'aQb' ~~ &GRegexInterpolated::x,
    'a token declaration reads the current lexical value';

my $ast = Q[/a $x b/].AST;
ok $ast.gist.contains('RakuAST::Regex::Interpolation'),
    'the parser AST names the interpolation node';
is RakuAST::Regex::Interpolation.new(:var(RakuAST::Var::Lexical.new(q[$x]))).var.name,
    '$x', 'the constructor accepts a lexical interpolation variable';
nok 'aZb' ~~ /a @($x) b/,
    'the bounded slice does not conflate array interpolation with scalar interpolation';
