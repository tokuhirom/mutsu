use v6;
use Test;

# RakuAST Phase 3 slice 3 (ADR-0011): positional-leaf accessors.
# A literal node exposes its payload via `.value` (`IntLiteral.value` -> Int),
# and `Var::Lexical.name` returns the sigil'd variable string. Verified against
# Rakudo; this file passes under BOTH mutsu and raku.

plan 6;

# --- IntLiteral.value -------------------------------------------------------
is Q[42].AST.statements[0].expression.value, 42, 'IntLiteral.value returns the Int';
is Q[42].AST.statements[0].expression.value.^name, 'Int', 'IntLiteral.value is an Int';

# --- RatLiteral.value / StrLiteral.value ------------------------------------
is Q[3.5].AST.statements[0].expression.value, 3.5, 'RatLiteral.value returns the Rat';
is Q["hi"].AST.statements[0].expression.segments[0].value, 'hi',
    'StrLiteral.value returns the string';

# --- Var::Lexical.name ------------------------------------------------------
my $x;
is Q[my $x; $x].AST.statements[1].expression.name, '$x',
    'Var::Lexical.name returns the sigilled variable string';

# --- a named accessor of the same spelling is NOT shadowed -------------------
is Q[say 1].AST.statements[0].expression.name.^name, 'RakuAST::Name',
    'Call::Name.name still returns the named Name node';
