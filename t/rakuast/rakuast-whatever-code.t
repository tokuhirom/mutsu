use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `*` in `.AST`: rakudo 2026.09 *prints* every `*` -- a value (`1, *, 2`) and a
# priming leaf (`* + 1`) alike -- as `RakuAST::Term::Whatever.new`, while the
# priming leaf's `.^name` stays `RakuAST::WhateverCode::Argument` (ADR-0033
# Phase 2). The text is what `.gist`/`.raku` show, the class is what a consumer
# walking the tree sees; `EVAL` of the tree re-derives the priming from the
# position of the `*` (Phase 3), so the round trip shows the leaf was
# classified correctly. This file passes under BOTH mutsu and raku, which is
# the oracle.

plan 64;

my @a = 1, 2, 3;

# --- priming positions: Term::Whatever in the tree, a WhateverCode on EVAL ----

for (
    '* + 1', '* + *', '*.abs', '*.WHICH', '1..*-1', '@a[* - 1]',
    '-*', '?*', '*++', '* x 2', '1 x *',
    '* ~~ Int', 'Int ~~ *', '$_ ~~ *', '* !~~ Int',
    '"k" => *', '(1, 2).map(* + 1)', '(* - 1) o (* * 2)',
) -> $src {
    my $gist = EVAL(qq[Q[{$src}].AST.gist]);
    ok $gist.contains('RakuAST::Term::Whatever'),
        "$src -- '*' renders as Term::Whatever";
    nok $gist.contains('RakuAST::WhateverCode::Argument'),
        "$src -- no WhateverCode::Argument leaf";
}

# --- value positions render the same ------------------------------------------

for (
    '1, *, 2', '1..*', '1, 2 ... *', 'my $x = *', '* xx 2', '1 xx *',
    '@a[*]', '*(1)', '*.WHAT', '(a => *)', 'say *',
) -> $src {
    my $gist = EVAL(qq[Q[{$src}].AST.gist]);
    ok $gist.contains('RakuAST::Term::Whatever'),
        "$src -- '*' renders as Term::Whatever";
}

# --- hierarchy ----------------------------------------------------------------

my $arg = Q[* + 1].AST.statements[0].expression.left;
is $arg.^name, 'RakuAST::WhateverCode::Argument', 'left operand of * + 1 is WhateverCode::Argument';
is $arg.raku, 'RakuAST::Term::Whatever.new', 'but it prints as Term::Whatever';
ok $arg ~~ RakuAST::Term, 'WhateverCode::Argument isa Term';
ok $arg ~~ RakuAST::Expression, 'WhateverCode::Argument isa Expression';
ok $arg ~~ RakuAST::Node, 'WhateverCode::Argument isa Node';

my $val = Q[1, *, 2].AST.statements[0].expression.operands[1];
is $val.^name, 'RakuAST::Term::Whatever', 'a comma operand is a Term::Whatever';

# --- ** (HyperWhatever): read direction only, priming out of scope ------------

is Q[**].AST.statements[0].expression.^name, 'RakuAST::Term::HyperWhatever',
    '** is Term::HyperWhatever';
ok Q[**].AST.statements[0].expression ~~ RakuAST::Term, 'Term::HyperWhatever isa Term';

# --- full gist of the headline example ----------------------------------------

is Q[* + 1].AST.gist, q:to/END/.chomp, '* + 1 full gist';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::Term::Whatever.new,
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(1)
    )
  )
)
END

# --- the round trip classifies the leaf: a priming `*` curries, a value stays --

is EVAL(Q[(* + 1)].AST).WHAT.^name, 'WhateverCode', '* + 1 round-trips to a WhateverCode';
is EVAL(Q[(* + 1)].AST)(5), 6, 'the round-tripped WhateverCode is callable';
is EVAL(Q[(1 x *)].AST).WHAT.^name, 'WhateverCode', '1 x * round-trips to a WhateverCode';
is EVAL(Q[(1, *, 2)[1]].AST).WHAT.^name, 'Whatever', 'a comma operand stays a Whatever value';
is EVAL(Q[(1..*).WHAT].AST).^name, 'Range', 'a range endpoint stays a Whatever value';

# --- runtime no-change guard ---------------------------------------------------

is (* + 1)(5), 6, 'WhateverCode still callable after leaf-splitting';
is (1 x *).WHAT.^name, 'WhateverCode', '1 x * still autoprimes at runtime';
my $sm = (Int ~~ *);
is $sm(5), False, 'Int ~~ * still evaluates correctly at runtime';
