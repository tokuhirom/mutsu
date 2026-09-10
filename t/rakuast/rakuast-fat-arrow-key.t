use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# raku models the two fat-arrow key spellings as different nodes: a BAREWORD key
# is a `FatArrow` carrying the key as a plain string, while a quoted or computed
# one is an ordinary `ApplyInfix` over `=>`. mutsu had them **swapped**, and
# refused the computed one outright.
#
# The cause was rendering keyed off `Expr::PositionalPair`, which does not mean
# "quoted key" -- it means "not a named argument", which a *parenthesized*
# bareword pair also is. The parser now records the parenthesization separately,
# so the two meanings no longer collide, and the rendering keys off the pair's
# own shape.
#
# Measured against rakudo 2026.07. Passes under BOTH mutsu and raku.

plan 18;

sub classes($src) { $src.AST.gist }

# --- the three key spellings -------------------------------------------------
ok classes(Q{my $p = a => 1}).contains('RakuAST::FatArrow.new('),
    'a bareword key renders as FatArrow';
ok classes(Q{my $p = a => 1}).contains('key   => "a"'),
    '... carrying the key as a plain string';
nok classes(Q{my $p = a => 1}).contains('RakuAST::ApplyInfix'),
    '... and not as an ApplyInfix';

ok classes(Q{my $p = "a" => 1}).contains('RakuAST::ApplyInfix.new('),
    'a quoted key renders as ApplyInfix';
ok classes(Q{my $p = "a" => 1}).contains('RakuAST::QuotedString'),
    '... over a QuotedString left operand';
nok classes(Q{my $p = "a" => 1}).contains('RakuAST::FatArrow'),
    '... and not as a FatArrow';

ok classes(Q{my $k = "z"; my $p = $k => 1}).contains('RakuAST::ApplyInfix.new('),
    'a computed key renders as ApplyInfix (it used to be refused)';
ok classes(Q{my $k = "z"; my $p = $k => 1}).contains('RakuAST::Var::Lexical.new("\$k")'),
    '... over the variable itself';

# --- a parenthesized pair is positional AND parenthesized --------------------
{
    my $paren = classes(Q{my $p = (a => 1)});
    ok $paren.contains('RakuAST::Circumfix::Parentheses.new('),
        'a parenthesized bareword pair keeps its parentheses';
    ok $paren.contains('RakuAST::FatArrow.new('),
        '... with the FatArrow inside';
    nok $paren.contains('RakuAST::ApplyInfix'),
        '... and no ApplyInfix anywhere';
}

# --- the same in argument position -------------------------------------------
ok classes(Q{sub f($x){}; f(a => 1)}).contains('RakuAST::FatArrow.new('),
    'a bareword named argument renders as FatArrow';
ok classes(Q{sub f($x){}; f("a" => 1)}).contains('RakuAST::ApplyInfix.new('),
    'a quoted-key argument renders as ApplyInfix';
ok classes(Q{sub f($x){}; f((a => 1))}).contains('RakuAST::Circumfix::Parentheses.new('),
    'a parenthesized argument keeps its parentheses';

# --- several pairs in one list ------------------------------------------------
{
    my $pairs = classes(Q{my %h = a => 1, b => 2});
    is $pairs.comb('RakuAST::FatArrow.new(').elems, 2, 'both pairs of a list render as FatArrow';
}

# --- write side: the two spellings keep their named/positional meaning --------
is EVAL(Q{sub f(:$a) { $a }; f(a => 7)}.AST), 7,
    'a lowered bareword pair still binds as a NAMED argument';
is EVAL(Q{sub f($p) { $p.key }; f("a" => 7)}.AST), 'a',
    'a lowered quoted-key pair still passes as a POSITIONAL Pair';
is EVAL(Q{sub f($p) { $p.value }; f((a => 7))}.AST), 7,
    'a lowered parenthesized pair is positional too';
