use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-0088 issue #8033: angle scalar interpolation retains its distinct
# RakuAST::Regex::Assertion::InterpolatedVar shape. The value is still read by
# the established runtime regex parser, which reparses it at match time.

plan 17;

my $inner = /a/;
my $stored = /<$inner>/;
ok 'a' ~~ $stored, 'angle interpolation matches the current Regex value';
$inner = /b/;
ok 'b' ~~ $stored, 'angle interpolation observes a later Regex reassignment';
nok 'a' ~~ $stored, 'angle interpolation no longer uses the old Regex value';

my $text = 'x';
my $stored-text = /<$text>/;
ok 'x' ~~ $stored-text, 'angle interpolation also matches a scalar string';
$text = 'y';
ok 'y' ~~ $stored-text, 'angle interpolation observes a later string reassignment';
nok 'x' ~~ $stored-text, 'angle interpolation does not cache the old string value';

my $node = Q[/<$inner>/].AST.statements[0].expression.body;
ok $node ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'angle interpolation has its dedicated assertion node';
is $node.var.name, '$inner', 'the assertion retains the scalar lexical';
is $node.sequential, False, 'ordinary angle interpolation is not sequential';

my $lookaround = /<?before <$inner>>/;
ok 'b' ~~ $lookaround, 'angle interpolation works inside a named lookaround';
$inner = /c/;
ok 'c' ~~ $lookaround, 'lookaround angle interpolation observes reassignment';
nok 'b' ~~ $lookaround, 'lookaround angle interpolation is not cached';

my $sequential = Q[/foo || <$inner>/].AST.statements[0].expression.body;
ok $sequential.gist.contains('RakuAST::Regex::SequentialAlternation'),
    'angle interpolation composes with sequential alternation';
ok $sequential.gist.contains('sequential => True'),
    'angle interpolation records sequential-branch context';

my $constructed = RakuAST::Regex::Assertion::InterpolatedVar.new(
    sequential => False,
    var => RakuAST::Var::Lexical.new(q[$inner]),
);
ok $constructed ~~ RakuAST::Regex::Assertion::InterpolatedVar,
    'the constructed assertion uses the measured node type';
is $constructed.var.name, '$inner', 'the constructed assertion exposes its lexical';
my $constructed-regex = EVAL(RakuAST::QuotedRegex.new(body => $constructed));
ok 'c' ~~ $constructed-regex,
    'a constructed angle interpolation lowers through the existing regex path';
