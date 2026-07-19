use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 33 (ADR-0011): `gather { … take … }`, both directions.
# `gather` is a `StatementPrefix::Gather` (like `do`/`try`) and `take` is a bare
# call (like `die`). `Expr::Gather` and `Stmt::Take` convert and lower back.
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: `gather` renders as StatementPrefix::Gather ------------------
my $g = Q{gather { take 1; take 2 }}.AST.gist;
ok $g.contains('RakuAST::StatementPrefix::Gather.new(')
    && $g.contains('RakuAST::Name.from-identifier("take")'),
    'gather renders as StatementPrefix::Gather with take calls';

# --- gather collects each take ----------------------------------------------
is EVAL(Q{gather { take 1; take 2; take 3 }.elems}.AST), 3,
    'gather collects each take';
is EVAL(Q{gather { take 10; take 20 }.sum}.AST), 30, 'the gathered values sum';

# --- take inside a loop -----------------------------------------------------
is EVAL(Q{gather { for 1..4 -> $x { take $x * $x } }.sum}.AST), 30,
    'take inside a loop gathers each iteration';

# --- a conditional take -----------------------------------------------------
is EVAL(Q{gather { for 1..6 -> $x { take $x if $x %% 2 } }.join(",")}.AST), '2,4,6',
    'a conditional take gathers a subset';
