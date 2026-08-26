use v6;
use experimental :rakuast;
use Test;

# todo/tickets/chained-compare-ast-node.md (ADR-0033's "Phase-4 prerequisite").
#
# A chained comparison (`a < m < b`, `a !before b before c`, ...) now has a
# genuine `Expr::ChainedCompare` marker in mutsu's internal AST instead of
# being expanded at PARSE time into `(a < m) && (m < b)` (with `m` duplicated)
# or a `DoBlock` binding a temp variable. The compiler expands the marker at
# COMPILE time instead (`crate::chain_compare::expand`), so the emitted
# bytecode is unchanged, but `.AST` can now render the chain the way rakudo
# does: a left-nested `ApplyInfix` with no `&&`/`DoBlock` wrapper.
#
# The `.AST.gist` assertions below are dual-oracle (ADR-0011 convention) and
# pass verbatim under BOTH mutsu and the system raku, EXCEPT the negated-chain
# one: a standalone negated comparison (`1 !before 2`) already renders as
# `ApplyPrefix("!", ...)` in mutsu rather than rakudo's own
# `MetaInfix::Negate` -- a separate, pre-existing gap this ticket does not
# close, so that assertion checks for the ABSENCE of the retired `&&`/DoBlock
# shape (true in both implementations) rather than an exact string match.
# Every other assertion (arity, short-circuiting, single evaluation,
# placeholder collection) is a plain behavioural check that must hold
# identically in both.

plan 24;

# --- .AST rendering: left-nested ApplyInfix, no && / DoBlock ---------------
#
# Before this ticket, even an all-literal chain like `1 < 2 < 3` rendered as
# `RakuAST::StatementPrefix::Do` (the DoBlock/temp-var expansion), not the
# duplicated-&& shape the ticket's own draft assumed -- re-verified 2026-08-26
# and pinned here so it cannot regress back to either wrong shape. Expected
# text captured verbatim from the system `raku`.

is Q[1 < 2 < 3].AST.gist, q:to/END/.chomp,
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::ApplyInfix.new(
            left  => RakuAST::IntLiteral.new(1),
            infix => RakuAST::Infix.new("<"),
            right => RakuAST::IntLiteral.new(2)
          ),
          infix => RakuAST::Infix.new("<"),
          right => RakuAST::IntLiteral.new(3)
        )
      )
    )
    END
    '1 < 2 < 3 -- .AST matches rakudo\'s left-nested ApplyInfix (no && / DoBlock)';

is Q[0 <= 3 <= 5].AST.gist, q:to/END/.chomp,
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::ApplyInfix.new(
            left  => RakuAST::IntLiteral.new(0),
            infix => RakuAST::Infix.new("<="),
            right => RakuAST::IntLiteral.new(3)
          ),
          infix => RakuAST::Infix.new("<="),
          right => RakuAST::IntLiteral.new(5)
        )
      )
    )
    END
    '0 <= 3 <= 5 -- same shape with a different comparison operator';

is Q[1 < 2 < 3 < 4].AST.gist, q:to/END/.chomp,
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::ApplyInfix.new(
          left  => RakuAST::ApplyInfix.new(
            left  => RakuAST::ApplyInfix.new(
              left  => RakuAST::IntLiteral.new(1),
              infix => RakuAST::Infix.new("<"),
              right => RakuAST::IntLiteral.new(2)
            ),
            infix => RakuAST::Infix.new("<"),
            right => RakuAST::IntLiteral.new(3)
          ),
          infix => RakuAST::Infix.new("<"),
          right => RakuAST::IntLiteral.new(4)
        )
      )
    )
    END
    '1 < 2 < 3 < 4 -- a longer chain nests the same way, three levels deep';

# An effectful middle (`$x++`) must render identically in shape to a pure one
# -- the representation no longer branches on purity at all (that branching,
# and its `(a < m) && (m < b)` duplicate-middle output, is exactly what this
# ticket retires).
{
    my $x = 5;
    my $gist = Q[1 < $x++ < 10].AST.gist;
    ok $gist.contains('ApplyInfix'), 'effectful-middle chain still renders ApplyInfix';
    nok $gist.contains('StatementPrefix::Do'), 'effectful-middle chain has no DoBlock wrapper';
    nok $gist.contains('"&&"'), 'effectful-middle chain has no && wrapper';
}

# Negated chain: check for the absence of the retired shape rather than an
# exact match (see the file header -- `!before`'s own rendering is a separate,
# pre-existing gap).
{
    my $gist = Q[1 !before 2 before 3].AST.gist;
    ok $gist.contains('ApplyInfix'), 'negated chain still renders ApplyInfix';
    nok $gist.contains('StatementPrefix::Do'), 'negated chain has no DoBlock wrapper';
    nok $gist.contains('"&&"'), 'negated chain has no && wrapper';
}

# --- runtime: middle evaluated exactly once, short-circuiting --------------

{
    my $n = 0;
    sub middle() { $n++; 5 }
    is (1 < middle() < 10), True, 'a satisfied chain evaluates the middle once (true case)';
    is $n, 1, '... exactly once';
}
{
    my $n = 0;
    sub q1() { $n++; 3 }
    is (5 < q1() < 100), False, 'short-circuiting: the second comparison is skipped';
    is $n, 1, '... the middle still ran exactly once';
}

# --- negated and mixed chains -----------------------------------------------

is (2 !before 1 before 3), True, 'negated-then-plain chain';
is (2 !before 3 before 1), False, '... false case';
is (1 < 2 == 2), True, 'mixed relational/equality chain';
is (1 < 2 == 3), False, '... false case';

# --- placeholder collection: { $^a < $^b < $^c } ----------------------------

my $ph = { $^a < $^b < $^c };
is $ph.arity, 3, 'placeholder collection sees every chain operand (arity 3)';
is $ph(1, 2, 3), True, 'placeholder chain -- satisfied';
is $ph(1, 5, 3), False, 'placeholder chain -- not satisfied';

# --- WhateverCode over a chain: exactly one priming scope -------------------

is (1 < * < 10).WHAT.^name, 'WhateverCode', 'a chained comparison with * curries';
is (1 < * < 10)(0), False, '... checking the left end';
is (1 < * < 10)(5), True, '... and inside the range';
is (1 < * && * < 10)(0), True,
    'a user-written && over the same operands is NOT a chain -- yields only its right thunk';
