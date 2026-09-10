use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST slice 26 (ADR-0011): bareword type terms `Int`/`Str`, both directions.
# A known builtin type name used as a value converts to a `Type::Simple` (read)
# and lowers back to a bareword term that evaluates to the type object (write).
# Only known types convert — a non-type bareword stays the boundary. Verified
# against Rakudo; passes under BOTH mutsu and raku.

plan 5;

# --- read side: a type term renders as Type::Simple -------------------------
ok Q{my $t = Int}.AST.gist.contains('RakuAST::Type::Simple.new(')
    && Q{my $t = Int}.AST.gist.contains('RakuAST::Name.from-identifier("Int")'),
    'a bare type name renders as Type::Simple';

# --- smartmatch against a type (the slice-18 boundary) ----------------------
is EVAL(Q{5 ~~ Int}.AST), True, '5 smartmatches Int';
is EVAL(Q{"x" ~~ Int}.AST), False, 'a string does not smartmatch Int';

# --- introspection on the type object ---------------------------------------
is EVAL(Q{Int.^name}.AST), 'Int', 'the type object has its name';

# --- a type stored in a variable --------------------------------------------
is EVAL(Q{my $t = Str; $t.^name}.AST), 'Str', 'a type object can be stored and reused';
