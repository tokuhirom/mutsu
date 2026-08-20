use v6;
use experimental :rakuast;
use Test;

# ADR-0033 Phase 2: `*` leaf classification.
#
# `RakuAST::Term::Whatever` is a `*` that stays a `Whatever` *value*;
# `RakuAST::WhateverCode::Argument` is a `*` that participates in
# Whatever-priming (`* + 1` curries; `1, *, 2` does not). The rule is
# syntactic and scope-independent (measured against the system `raku`, see
# docs/adr/0033-whatever-priming-leaf-and-derived-scope.md section 2.1) — this
# file is a dual-oracle test (ADR-0011 convention) and passes under BOTH
# mutsu and raku.
#
# Over-marking a value leaf as `Argument` is invisible at runtime (both
# variants compile identically outside src/rakuast/), so the "value leaf"
# half below is the ONLY detector for that class of bug — do not remove it.

plan 68;

my @a = 1, 2, 3;

# --- Argument leaves (participate in priming) --------------------------

for (
    '* + 1', '* + *', '*.abs', '*.WHICH', '1..*-1', '@a[* - 1]',
    '-*', '?*', '*++', '* x 2', '1 x *',
    '* ~~ Int', 'Int ~~ *', '$_ ~~ *', '* !~~ Int',
    '"k" => *', '(1, 2).map(* + 1)', '(* - 1) o (* * 2)',
) -> $src {
    my $gist = EVAL(qq[Q[{$src}].AST.gist]);
    ok $gist.contains('RakuAST::WhateverCode::Argument'),
        "$src -- '*' renders as WhateverCode::Argument";
    nok $gist.contains('RakuAST::Term::Whatever'),
        "$src -- no stray Term::Whatever leaf";
}

# --- Value leaves (regression guard for over-marking) -------------------

for (
    '1, *, 2', '1..*', '1, 2 ... *', 'my $x = *', '* xx 2', '1 xx *',
    '@a[*]', '*(1)', '*.WHAT', '(a => *)', 'say *',
) -> $src {
    my $gist = EVAL(qq[Q[{$src}].AST.gist]);
    ok $gist.contains('RakuAST::Term::Whatever'),
        "$src -- '*' stays a Term::Whatever value";
    nok $gist.contains('RakuAST::WhateverCode::Argument'),
        "$src -- not over-marked as an Argument";
}

# `[*]` is deliberately excluded above: mutsu currently parses a standalone
# `[*]` as the `[*]`-reduction metaoperator applied to an empty list (a
# pre-existing, Whatever-unrelated parse ambiguity -- there is no `*` node in
# that tree at all to classify either way), while raku's `.AST` renders it as
# a bare `Term::Whatever`. Out of scope for this ADR.

# --- Hierarchy ------------------------------------------------------------

my $arg = Q[* + 1].AST.statements[0].expression.left;
is $arg.^name, 'RakuAST::WhateverCode::Argument', 'left operand of * + 1 is WhateverCode::Argument';
ok $arg ~~ RakuAST::Term, 'WhateverCode::Argument isa Term';
ok $arg ~~ RakuAST::Expression, 'WhateverCode::Argument isa Expression';
ok $arg ~~ RakuAST::Node, 'WhateverCode::Argument isa Node';

# --- ** (HyperWhatever): read direction only, priming out of scope --------

is Q[**].AST.statements[0].expression.^name, 'RakuAST::Term::HyperWhatever',
    '** is Term::HyperWhatever';
ok Q[**].AST.statements[0].expression ~~ RakuAST::Term, 'Term::HyperWhatever isa Term';

# --- Full gist sanity check for the headline example -----------------------

is Q[* + 1].AST.gist, q:to/END/.chomp, '* + 1 full gist';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::ApplyInfix.new(
      left  => RakuAST::WhateverCode::Argument.new,
      infix => RakuAST::Infix.new("+"),
      right => RakuAST::IntLiteral.new(1)
    )
  )
)
END

# --- Runtime no-change guard: the rendering split must not alter results --

is (* + 1)(5), 6, 'WhateverCode still callable after leaf-splitting';
is (1 x *).WHAT.^name, 'WhateverCode', '1 x * still autoprimes at runtime';
my $sm = (Int ~~ *);
is $sm(5), False, 'Int ~~ * still evaluates correctly at runtime';
