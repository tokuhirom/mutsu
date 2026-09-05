use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `until` and `repeat ... until`, both directions (ADR-0011).
#
# These were *wrongly rendered* rather than refused: mutsu stores `until X` as
# `while !X` and rendered exactly that — `Statement::Loop::While` over an
# `ApplyPrefix("!")`. raku has `Statement::Loop::Until` / `Loop::RepeatUntil`
# classes and renders the *undecorated* condition.
#
# mutsu keeps an `is_until` flag alongside the negated condition, so the source
# keyword was recoverable all along; the converter strips the `!` the parser
# added and the lowerer re-plants it. Measured against rakudo 2026.07: the gists
# are byte-for-byte identical.
#
# Passes under BOTH mutsu and raku.

plan 11;

# --- read side ---------------------------------------------------------------
my $until = Q{until 1 { last }}.AST.gist;
ok $until.contains('RakuAST::Statement::Loop::Until.new('),
    '`until` renders as Loop::Until';
nok $until.contains('RakuAST::Prefix.new("!")'),
    "`until` renders its condition without the parser's negation";
nok $until.contains('Loop::While'), '`until` is not a While';

my $ru = Q{repeat { last } until 1}.AST.gist;
ok $ru.contains('RakuAST::Statement::Loop::RepeatUntil.new('),
    '`repeat ... until` renders as Loop::RepeatUntil';
nok $ru.contains('RakuAST::Prefix.new("!")'),
    '`repeat ... until` renders its condition undecorated';

# --- the `while` forms are unchanged -----------------------------------------
ok Q{while 1 { last }}.AST.gist.contains('RakuAST::Statement::Loop::While.new('),
    '`while` still renders as Loop::While';
ok Q{repeat { last } while 0}.AST.gist.contains('RakuAST::Statement::Loop::RepeatWhile.new('),
    '`repeat ... while` still renders as Loop::RepeatWhile';

# --- write side --------------------------------------------------------------
is EVAL(Q{my $i = 0; until $i >= 3 { $i = $i + 1 }; $i}.AST), 3,
    'an `until` loop lowers and runs to its condition';
is EVAL(Q{my $i = 5; until $i > 0 { $i = 0 }; $i}.AST), 5,
    'an `until` whose condition is already true never runs its body';
is EVAL(Q{my $i = 0; repeat { $i = $i + 1 } until $i >= 3; $i}.AST), 3,
    'a `repeat ... until` lowers';
is EVAL(Q{my $i = 0; while $i < 3 { $i = $i + 1 }; $i}.AST), 3,
    'a `while` loop still lowers';
