use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST method return types (ADR-0011), read direction.
#
# `RakuAST::Method` is a `RakuAST::Routine` just like `RakuAST::Sub`, so it
# carries the same return-type shape and the same three spellings:
#   * `method m(--> Int)`        -> Signature.returns => Type::Simple
#   * `method m() returns Int`   -> traits => (Trait::Returns(Type::Simple),)
#   * `method m() of Int`        -> traits => (Trait::Of(Type::Simple),)
#
# The parser records which spelling was used as a `__return_via_*` marker in
# `MethodDecl.custom_traits`; before this slice that marker was filtered out at
# parse time, so the three forms were indistinguishable and every method with a
# return type was a `.AST` coverage boundary.
#
# Passes under BOTH mutsu and raku.

plan 16;

# --- read side: the `-->` arrow lands in the signature ----------------------
my $arrow = Q[method m(--> Int) { 1 }].AST.gist;
ok $arrow.contains('RakuAST::Method.new('),
    'a method with a `-->` return type renders as a RakuAST::Method';
ok $arrow.contains('returns    => RakuAST::Type::Simple.new('),
    'a `-->` return type renders as Signature.returns';
ok $arrow.contains('parameters => $( )'),
    'a parameter-less method signature renders its empty parameter list as $( )';
nok $arrow.contains('RakuAST::Trait::'),
    'a `-->` return type emits no trait';

my $arrow_p = Q[method m($x --> Int) { 1 }].AST.gist;
ok $arrow_p.contains('RakuAST::ParameterTarget::Var.new(')
    && $arrow_p.contains('returns    => RakuAST::Type::Simple.new('),
    'a method signature can carry both parameters and a `-->` return type';

# --- read side: the `returns` / `of` traits ---------------------------------
my $ret = Q[method m() returns Int { 1 }].AST.gist;
ok $ret.contains('RakuAST::Trait::Returns.new('),
    '`returns Int` on a method renders as a Trait::Returns';
nok $ret.contains('returns    =>'),
    '`returns Int` on a method does not also fill Signature.returns';

ok Q[method m() of Int { 1 }].AST.gist.contains('RakuAST::Trait::Of.new('),
    '`of Int` on a method renders as a Trait::Of';

ok Q[method m($x) returns Int { 1 }].AST.gist.contains('RakuAST::Trait::Returns.new('),
    'a parameterised method can carry a `returns` trait';

# --- a plain method still omits the signature -------------------------------
nok Q[method m() { 1 }].AST.gist.contains('signature'),
    'a method with neither parameters nor a return type omits the signature';

# --- the same shape inside a class body -------------------------------------
my $in_class = Q[class C { method m(--> Int) { 1 } }].AST.gist;
ok $in_class.contains('RakuAST::Method.new('),
    'a method declared in a class body renders as a RakuAST::Method';
ok $in_class.contains('returns    => RakuAST::Type::Simple.new('),
    'a class method carries its `-->` return type in the signature';

# --- introspection ----------------------------------------------------------
my $sig = Q[method m(--> Int) { 1 }].AST.statements[0].expression.signature;
ok $sig ~~ RakuAST::Signature, 'the method exposes its Signature';
is $sig.returns.name.gist, 'RakuAST::Name.from-identifier("Int")',
    'Signature.returns is reachable through the method accessors';

my $trait = Q[method m() returns Int { 1 }].AST.statements[0].expression.traits[0];
is $trait.^name, 'RakuAST::Trait::Returns', 'the method exposes its return trait';
is $trait.type.name.gist, 'RakuAST::Name.from-identifier("Int")',
    'Trait::Returns.type is reachable through the method accessors';
