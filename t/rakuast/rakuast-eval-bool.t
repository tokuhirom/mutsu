use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 14 (ADR-0011): `True`/`False` literals in both directions.
# raku models a boolean literal as `RakuAST::Term::Enum.from-identifier('True')`
# (single-quoted enum identifier). The read side (`.AST`) emits that node and the
# write side (`EVAL`) lowers it back to the Bool value. Verified against Rakudo;
# passes under BOTH mutsu and raku.

plan 6;

# --- read side: the gist uses the single-quoted enum form -------------------
is Q[True].AST.gist.chomp, q:to/END/.chomp, 'True renders as Term::Enum';
    RakuAST::StatementList.new(
      RakuAST::Statement::Expression.new(
        expression => RakuAST::Term::Enum.from-identifier('True')
      )
    )
    END

# --- write side: the literals evaluate to their Bool values -----------------
is EVAL(Q[True].AST), True, 'EVAL of True is True';
is EVAL(Q[False].AST), False, 'EVAL of False is False';

# --- a boolean out of an if/else --------------------------------------------
is EVAL(Q[sub even($n) { if $n %% 2 { True } else { False } }; even(4)].AST), True,
    'a sub returning True/False from if/else';

# --- a boolean drives a loop condition --------------------------------------
is EVAL(Q[my $i = 0; while True { $i = $i + 1; last if $i >= 3 }; $i].AST), 3,
    'while True with an explicit last';

# --- negation of a False literal --------------------------------------------
is EVAL(Q[my $f = False; !$f].AST), True, 'negating a False literal yields True';
