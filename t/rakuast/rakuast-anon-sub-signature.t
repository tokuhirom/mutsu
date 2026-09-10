use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# RakuAST anonymous subs with an explicit signature (ADR-0011), both directions.
#
# `sub ($x) { }` and `-> $a, $b { }` both parse to mutsu's `Expr::AnonSubParams`,
# so before this slice the `sub` spelling rendered as a `RakuAST::PointyBlock`.
# raku models them with different nodes, and the difference is visible in the
# signature too: a sub/method parameter carries the implicit
# `type => RakuAST::Type::Setting(Any)`, a pointy block's parameter does not.
#
# `Expr::AnonSubParams` now carries an `is_sub` flag recording which declarator
# the source wrote. It has no execution meaning — both spellings compile to the
# same bytecode — it only stops the converter from having to guess.
#
# Passes under BOTH mutsu and raku.

plan 17;

# --- read side: `sub (...)` is a nameless Sub, not a PointyBlock -------------
my $anon = Q[my $f = sub ($x) { $x }].AST.gist;
ok $anon.contains('RakuAST::Sub.new('),
    'a parameterised anonymous sub renders as a RakuAST::Sub';
nok $anon.contains('RakuAST::PointyBlock'),
    'a parameterised anonymous sub is not a PointyBlock';
nok $anon.contains('name        => RakuAST::Name'),
    'an anonymous sub carries no name field';
ok $anon.contains('type     => RakuAST::Type::Setting.new('),
    "an anonymous sub's parameters carry the implicit Type::Setting(Any)";

my $multi = Q[my $f = sub ($a, $b) { $a + $b }].AST.gist;
ok $multi.contains('RakuAST::Sub.new('),
    'a two-parameter anonymous sub renders as a RakuAST::Sub';

# --- read side: a pointy block is still a PointyBlock ------------------------
my $pointy = Q[my $f = -> $a, $b { $a }].AST.gist;
ok $pointy.contains('RakuAST::PointyBlock.new('),
    'a multi-parameter pointy block still renders as a PointyBlock';
nok $pointy.contains('RakuAST::Type::Setting'),
    "a pointy block's parameters carry no implicit Type::Setting(Any)";

ok Q[my $f = -> $x { $x }].AST.gist.contains('RakuAST::PointyBlock.new('),
    'a single-parameter pointy block still renders as a PointyBlock';

# --- read side: the parameter-less anonymous sub is unchanged ---------------
my $bare = Q[my $f = sub { 42 }].AST.gist;
ok $bare.contains('RakuAST::Sub.new('),
    'a parameter-less anonymous sub still renders as a RakuAST::Sub';
nok $bare.contains('signature'),
    'a parameter-less anonymous sub omits its signature';

# --- read side: a bare block is still a Block -------------------------------
ok Q[my $f = { 42 }].AST.gist.contains('RakuAST::Block.new('),
    'a bare block still renders as a RakuAST::Block';

# --- read side: `-->` in an anonymous sub signature -------------------------
ok Q[my $f = sub ($x --> Int) { $x }].AST.gist.contains('returns    => RakuAST::Type::Simple.new('),
    'an anonymous sub renders its `-->` return type in the signature';

# --- introspection ----------------------------------------------------------
my $sub = Q[my $f = sub ($x) { $x }].AST.statements[0].expression.initializer.expression;
is $sub.^name, 'RakuAST::Sub', 'the anonymous sub node reports as a RakuAST::Sub';
is $sub.signature.parameters.elems, 1, 'the anonymous sub exposes one parameter';

# --- write side: EVAL lowers a nameless Sub ---------------------------------
is EVAL(Q[my $f = sub ($x) { $x * 2 }; $f(9)].AST), 18,
    'a single-parameter anonymous sub round-trips through EVAL';
is EVAL(Q[my $add = sub ($a, $b) { $a + $b }; $add(3, 4)].AST), 7,
    'a two-parameter anonymous sub round-trips through EVAL';
is EVAL(Q[my $f = sub { 42 }; $f()].AST), 42,
    'a parameter-less anonymous sub round-trips through EVAL';
