use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 32 (ADR-0011): `die`/`fail`, both directions. raku models these
# as bare calls (like `return`/`last`/`next`); `Stmt::Die`/`Stmt::Fail` convert
# to a `Call::Name` and lower back. Verified against Rakudo; passes under BOTH
# mutsu and raku.

plan 5;

# --- read side: `die` renders as a call -------------------------------------
ok Q{die "boom"}.AST.gist.contains('RakuAST::Name.from-identifier("die")'),
    'die renders as a call to "die"';

# --- a die inside a try is trapped ------------------------------------------
nok EVAL(Q{try { die "boom"; 1 }}.AST).defined,
    'a die inside a try yields an undefined value';

# --- the die message reaches $! ---------------------------------------------
is EVAL(Q{try { die "boom" }; $!.Str}.AST), 'boom',
    'the die message is available in $!';

# --- fail is likewise trapped -----------------------------------------------
nok EVAL(Q{try { fail "nope"; 1 }}.AST).defined,
    'a fail inside a try yields an undefined value';

# --- a conditional die drives control flow ----------------------------------
is EVAL(Q{sub f($x) { die "neg" if $x < 0; $x * 2 }; try { f(-1) } // "caught"}.AST), 'caught',
    'a conditional die is caught by a surrounding try';
