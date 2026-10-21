use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 13;

my $source = Q[rx/%<chars>=( \s+ \S+ )+/].AST;
my $body = $source.statements[0].expression.body;
isa-ok $body, RakuAST::Regex::NamedCapture,
    'a hash alias has a structural named-capture node';
is $body.name, 'chars', 'the source alias name is retained';
ok $body.hash, 'the hash sigil is represented';
nok $body.array, 'the hash alias is distinct from an array alias';
isa-ok $body.regex, RakuAST::Regex::QuantifiedAtom,
    'the aliased group retains its quantifier';
ok $source.raku.contains('hash  => True'),
    'the extension field survives constructor rendering';

is EVAL(Q[my $m = "  a b\tc" ~~ /%<chars>=( \s+ \S+ )+/; $m<chars>.keys.elems].AST),
    3, 'the source AST files one hash key per iteration';
is EVAL(Q[my $m = "  a b\tc" ~~ /%chars=( \s+ \S+ )+/; $m<chars>.keys.elems].AST),
    3, 'the bare-name hash alias has the same semantics';
is EVAL(Q[my $m = 'abc' ~~ /%<letters>=\w+/; $m<letters>.keys.elems].AST),
    3, 'a quantified character atom files each matched character';

my $constructed = RakuAST::QuotedRegex.new(
    body => RakuAST::Regex::NamedCapture.new(
        name => 'h', hash => True, regex => RakuAST::Regex::Literal.new('a')));
my $match = 'a' ~~ EVAL($constructed);
ok $match, 'a constructed hash alias matches';
is $match<h>.keys.elems, 1, 'a constructed hash alias files its key';
throws-like {
    RakuAST::Regex::NamedCapture.new(
        name => 'h', array => True, hash => True,
        regex => RakuAST::Regex::Literal.new('a'))
}, Exception, message => /'cannot combine'/,
    'hash and array capture kinds cannot be combined';

throws-like { EVAL q["a~b" ~~ / a ~ b /] }, Exception,
    message => /'Unrecognized regex metacharacter ~'/,
    'an unpaired tilde raises its syntax diagnostic before RakuAST conversion';
