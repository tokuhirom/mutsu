use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# `BEGIN` inside an EVAL'd compilation unit, in both spellings.
#
# BEGIN runs at *compile* time, so a mainline declaration textually after it
# still wins, and a read textually before it still sees its side effects. mutsu
# implements that with a dedicated pre-pass (`run_toplevel_begin_phasers`) run
# during compilation of a program -- which the re-entrant EVAL carriers did not
# call. They ran the phaser in statement position instead, so
# `EVAL 'my $x = 0; BEGIN { $x = 1 }; $x'` answered 1 where raku answers 0.
# The RakuAST spelling refused to lower `BEGIN` at all rather than answer wrong.
#
# Both carriers now run the same pass the mainline pipeline does, in the same
# order (BEGIN before `reorder_phasers_for_eval`, so the hoisted ones are gone
# before that pass buckets declarations).
#
# Passes under BOTH mutsu and raku.

plan 10;

# --- a later declaration clobbers what BEGIN did -----------------------------
is EVAL(q{my $x = 0; BEGIN { $x = 1 }; $x}), 0,
    'string EVAL: BEGIN runs at compile time, so the mainline `my` wins';
is EVAL(Q{my $x = 0; BEGIN { $x = 1 }; $x}.AST), 0,
    'RakuAST EVAL: the same';

# --- ... and a read textually BEFORE the BEGIN sees its side effects ---------
is EVAL(q{my @a; my $c = @a.elems; BEGIN { @a = 1, 2, 3 }; $c}), 3,
    'string EVAL: a read preceding the BEGIN observes it';
is EVAL(Q{my @a; my $c = @a.elems; BEGIN { @a = 1, 2, 3 }; $c}.AST), 3,
    'RakuAST EVAL: the same';

# --- CHECK/INIT keep their own pre-mainline ordering -------------------------
is EVAL(q{my $y = 0; INIT { $y = 1 }; $y}), 0, 'string EVAL: INIT unaffected';
is EVAL(Q{my $y = 0; INIT { $y = 1 }; $y}.AST), 0, 'RakuAST EVAL: INIT unaffected';
is EVAL(q{my $z = 0; CHECK { $z = 1 }; $z}), 0, 'string EVAL: CHECK unaffected';

# --- a BEGIN that is not hoistable still runs, in place ----------------------
# The pre-run is deliberately narrow (no declarations, barewords or calls); a
# BEGIN it declines is left in the mainline rather than dropped.
is EVAL(q{BEGIN { my $q = 3 }; 9}), 9,
    'a non-hoisted BEGIN still runs and does not swallow the result';
is EVAL(q{sub g() { BEGIN { 4 } }; g()}), 4,
    'a BEGIN nested in a routine is untouched';

# --- the read direction was already right and stays right --------------------
ok Q{BEGIN { 1 }}.AST.gist.contains('RakuAST::StatementPrefix::Phaser::Begin'),
    'BEGIN still renders as StatementPrefix::Phaser::Begin';
