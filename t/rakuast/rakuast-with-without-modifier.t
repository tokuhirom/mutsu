use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# The `with` / `without` statement modifiers across the RakuAST boundary.
#
# raku hangs both off the modified statement as a `condition-modifier`, exactly
# like `if` / `unless` — NOT as the `loop-modifier` a real `given` gets, even
# though mutsu's parser desugars them into `given X { if $_.defined { STMT } }`.
# That desugar is ambiguous on its own: a hand-written
# `(STMT if $_.defined) given X` produces the identical internal shape, so the
# internal AST carries a marker recording which keyword the source used.
# Measured against Rakudo; every assertion below passes under BOTH mutsu and raku.

plan 20;

# --- read direction: the node class ----------------------------------------

is Q["found" with "hello"].AST.statements[0].condition-modifier.^name,
    'RakuAST::StatementModifier::With',
    'with modifier renders as StatementModifier::With';
is Q["nf" without Nil].AST.statements[0].condition-modifier.^name,
    'RakuAST::StatementModifier::Without',
    'without modifier renders as StatementModifier::Without';

# It is a *condition* modifier, not a loop modifier: `loop-modifier` is the
# field a real `given` fills, and `with` must leave it out entirely.
unlike Q[say 1 with 2].AST.gist, /'loop-modifier'/,
    'with modifier does not occupy the loop-modifier slot';
unlike Q[say 1 without Nil].AST.gist, /'loop-modifier'/,
    'without modifier does not occupy the loop-modifier slot';

# The modified statement survives as the statement's own expression, with the
# scaffolding of the desugar (the topicalizer and its `.defined` test) gone.
is Q[say 1 with 2].AST.statements[0].expression.^name,
    'RakuAST::Call::Name::WithoutParentheses',
    'the modified statement is the statement expression, not the desugar';

# --- the ambiguity the marker exists to resolve -----------------------------
# Same internal shape, different source keyword: this one really is a `given`.

is Q[(say 1 if $_.defined) given 2].AST.statements[0].loop-modifier.^name,
    'RakuAST::StatementModifier::Given',
    'an explicit given modifier is still a Given, not mistaken for a with';
like Q[(say 1 if $_.defined) given 2].AST.gist, /'loop-modifier'/,
    'an explicit given modifier fills the loop-modifier slot instead';

# A real `given` block is unaffected too.
is Q[given 2 { say 1 }].AST.statements[0].^name,
    'RakuAST::Statement::Given',
    'the given block form still renders as Statement::Given';

# --- write direction: EVAL of the round-tripped AST -------------------------

is EVAL(Q["found" with "hello"].AST), 'found',
    'with: a defined topic runs the statement and yields its value';
is EVAL(Q["found" with Nil].AST).gist, '()',
    'with: an undefined topic yields the empty Slip';
is EVAL(Q["nf" without Nil].AST), 'nf',
    'without: an undefined topic runs the statement';
is EVAL(Q["nf" without 3].AST).gist, '()',
    'without: a defined topic yields the empty Slip';

# The modifier topicalizes, so the modified statement sees `$_`.
is EVAL(Q[my $seen; $seen = $_ with 42; $seen].AST), 42,
    'with: the modified statement sees the topic as $_';
is EVAL(Q[my $seen = 0; $seen = 1 without Nil; $seen].AST), 1,
    'without: the modified statement runs when the topic is undefined';

# Like the `given` modifier, it introduces no lexical block of its own.
is EVAL(Q[(my $m = 9) with 2; $m].AST), 9,
    'with modifier round-trips without introducing a lexical scope';

# --- semantics, from source, unchanged by the marker ------------------------

is ("found" with "hello"), 'found', 'source: with runs on a defined topic';
is ("found" with Nil).gist, '()',    'source: with skips an undefined topic';
is ("nf" without Nil),     'nf',     'source: without runs on an undefined topic';
is ("nf" without 3).gist,  '()',     'source: without skips a defined topic';

# A `with` modifier still composes with the topic it binds.
{
    my $out;
    $out = "got: $_" with 42;
    is $out, 'got: 42', 'source: the with modifier topicalizes for the statement';
}
