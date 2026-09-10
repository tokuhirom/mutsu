use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# List infixes beyond the comma and the `andthen` family (ADR-0011).
#
# raku renders a *list-associative* infix as one flat `ApplyListInfix` carrying
# every operand of the chain, where an ordinary infix is a left-nested
# `ApplyInfix`. Measured against rakudo 2026.07, the list-associative set mutsu
# can produce is: `,`, `andthen`/`orelse`/`notandthen`, the junction
# constructors `|` / `&` / `^`, and `min` / `max`. `+`, `~`, `*`, `==`, `eq`,
# `and`, `or`, `&&`, `||` and `//` are all ordinary infixes.
#
# The comma and the `andthen` family were already handled; the junctions and
# `min`/`max` rendered as nested `ApplyInfix`, which is a shape rakudo never
# produces for them.
#
# Passes under BOTH mutsu and raku.

plan 12;

sub expr-of($src) { $src.AST.statements[0].expression.gist }

# --- min / max ---------------------------------------------------------------
ok expr-of(Q{my $x = 1 min 2}).contains('RakuAST::ApplyListInfix.new('),
    '`min` renders as an ApplyListInfix';
ok expr-of(Q{my $x = 1 max 2}).contains('RakuAST::ApplyListInfix.new('),
    '`max` renders as an ApplyListInfix';

# --- a chain flattens into one operand list ---------------------------------
my $chain = expr-of(Q{my $x = 1 min 2 min 3});
ok $chain.contains('RakuAST::IntLiteral.new(1)')
    && $chain.contains('RakuAST::IntLiteral.new(2)')
    && $chain.contains('RakuAST::IntLiteral.new(3)'),
    'a `min` chain keeps all three operands';
is $chain.comb('ApplyListInfix').elems, 1,
    'a `min` chain is ONE flat list, not a nest';

# --- junction constructors ---------------------------------------------------
ok expr-of(Q{my $x = 1 | 2}).contains('RakuAST::ApplyListInfix.new('),
    '`|` renders as an ApplyListInfix';
ok expr-of(Q{my $x = 1 & 2}).contains('RakuAST::ApplyListInfix.new('),
    '`&` renders as an ApplyListInfix';
ok expr-of(Q{my $x = 1 ^ 2}).contains('RakuAST::ApplyListInfix.new('),
    '`^` renders as an ApplyListInfix';
is expr-of(Q{my $x = 1 | 2 | 3}).comb('ApplyListInfix').elems, 1,
    'a `|` chain is one flat list';

# --- ordinary infixes are unchanged -----------------------------------------
ok expr-of(Q{my $x = 1 + 2}).contains('RakuAST::ApplyInfix.new('),
    '`+` is still an ordinary ApplyInfix';
nok expr-of(Q{my $x = 1 ~ 2}).contains('ApplyListInfix'),
    '`~` is still an ordinary ApplyInfix';

# --- write side --------------------------------------------------------------
is EVAL(Q{1 min 2 min 3}.AST), 1, 'a `min` chain lowers and evaluates';
is EVAL(Q{my $x = 1; ($x ~~ 1|2)}.AST), True, 'a junction lowers and smartmatches';
