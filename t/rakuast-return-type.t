use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST routine return types (ADR-0011), both directions.
#
# raku models the two spellings with different nodes and mutsu's internal AST
# keeps them apart, so the converter never guesses:
#   * `sub f(--> Int)`        -> Signature.returns => Type::Simple
#   * `sub f() returns Int`   -> traits => (Trait::Returns(Type::Simple),)
#   * `sub f() of Int`        -> traits => (Trait::Of(Type::Simple),)
# A parameter-less signature renders its empty parameter list as `$( )`.
#
# Verified against Rakudo; passes under BOTH mutsu and raku.

plan 21;

# --- read side: the `-->` arrow lands in the signature ----------------------
my $arrow = Q[sub f(--> Int) { 1 }].AST.gist;
ok $arrow.contains('returns    => RakuAST::Type::Simple.new('),
    'a `-->` return type renders as Signature.returns';
ok $arrow.contains('parameters => $( )'),
    'a parameter-less signature renders its empty parameter list as $( )';
nok $arrow.contains('RakuAST::Trait::'),
    'a `-->` return type emits no trait';

my $arrow_p = Q[sub f($x --> Int) { 1 }].AST.gist;
ok $arrow_p.contains('RakuAST::ParameterTarget::Var.new(')
    && $arrow_p.contains('returns    => RakuAST::Type::Simple.new('),
    'a signature can carry both parameters and a `-->` return type';

# --- read side: the `returns` / `of` traits ---------------------------------
my $ret = Q[sub f() returns Int { 1 }].AST.gist;
ok $ret.contains('RakuAST::Trait::Returns.new('),
    '`returns Int` renders as a Trait::Returns';
nok $ret.contains('returns    =>'),
    '`returns Int` does not also fill Signature.returns';

ok Q[sub f() of Int { 1 }].AST.gist.contains('RakuAST::Trait::Of.new('),
    '`of Int` renders as a Trait::Of';

ok Q[sub f($x) returns Int { 1 }].AST.gist.contains('RakuAST::Trait::Returns.new('),
    'a parameterised sub can carry a `returns` trait';

# --- a plain parameter-less sub still omits the signature -------------------
nok Q[sub f() { 1 }].AST.gist.contains('signature'),
    'a sub with neither parameters nor a return type omits the signature';

# --- read side: an empty Signature built by hand ----------------------------
is RakuAST::Signature.new.gist.lines[1].trim, 'parameters => $( )',
    'a hand-built empty Signature renders parameters as $( )';

# --- introspection ----------------------------------------------------------
my $sig = Q[sub f(--> Int) { 1 }].AST.statements[0].expression.signature;
ok $sig ~~ RakuAST::Signature, 'the sub exposes its Signature';
is $sig.returns.name.gist, 'RakuAST::Name.from-identifier("Int")',
    'Signature.returns is reachable through the accessors';

my $trait = Q[sub f() returns Int { 1 }].AST.statements[0].expression.traits[0];
is $trait.^name, 'RakuAST::Trait::Returns', 'the sub exposes its return trait';
is $trait.type.name.gist, 'RakuAST::Name.from-identifier("Int")',
    'Trait::Returns.type is reachable through the accessors';

# --- write side: EVAL round-trips both spellings ----------------------------
is EVAL(Q[sub f(Int $x --> Int) { $x * 2 }; f(5)].AST), 10,
    'a `-->` return type round-trips through EVAL';
is EVAL(Q[sub f($x) returns Int { $x + 1 }; f(4)].AST), 5,
    'a `returns` trait round-trips through EVAL';
is EVAL(Q[sub f($x) of Int { $x + 1 }; f(4)].AST), 5,
    'an `of` trait round-trips through EVAL';

# --- pointy blocks carry a `-->` return type too ----------------------------
my $pointy = Q[my $f = -> $x --> Int { $x }].AST.gist;
ok $pointy.contains('RakuAST::PointyBlock.new('),
    'a single-parameter pointy block still renders as a PointyBlock';
ok $pointy.contains('returns    => RakuAST::Type::Simple.new('),
    'a pointy block renders its `-->` return type in the signature';
is EVAL(Q[my $f = -> $x --> Int { $x * 3 }; $f(4)].AST), 12,
    'a pointy block return type round-trips through EVAL';

# --- write side: the lowered return type is still enforced ------------------
throws-like { EVAL(Q[sub f(--> Int) { "nope" }; f()].AST) }, Exception,
    'the lowered return type is checked at run time';
