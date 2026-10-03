use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# Invocant parameters across the RakuAST boundary (ADR-10723 Stage 1).
#
# `method m($self: $x)` and `method m(Foo:D: $x)` render the invocant as a
# `RakuAST::Parameter` with `invocant => True` after its type and type
# captures; a synthesized invocant (`Foo:D:`, `::?CLASS:U:`) has no target.
# A `::T` type capture declares `T` as a type name for the routine's body.
# Measured on rakudo 2026.09.
#
# Passes under BOTH mutsu and raku.

plan 15;

sub method-params($src, $i = 0) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[$i]
        .expression.signature.parameters
}

# --- read side ----------------------------------------------------------------
my @p = method-params(Q{class A { method m($s: $y) { } }});
is @p[0].invocant, True, '$s: is an invocant';
is @p[0].target.name, '$s', '... with its own target';
is @p[1].invocant, False, 'the next parameter is not';

@p = method-params(Q{class B { method m(B:D: $x) { } }});
is @p[0].invocant, True, 'B:D: is an invocant';
nok @p[0].target.defined, '... with no target';
is @p[0].type.^name, 'RakuAST::Type::Definedness', '... typed B:D';

@p = method-params(Q{class C { method m(::T C:D: $x) { } }});
is @p[0].type-captures[0].^name, 'RakuAST::Type::Capture', 'a captured invocant keeps its capture';
is @p[0].type.^name, 'RakuAST::Type::Definedness', '... and its nominal type';

my $built = RakuAST::Parameter.new(
    invocant => True,
    target   => RakuAST::ParameterTarget::Var.new(name => '$me'),
);
is $built.invocant, True, 'Parameter.new(:invocant) builds an invocant';

# --- the type-capture name in the body ----------------------------------------
my $body = Q{sub f(::T $x) { T }}.AST.statements[0].expression.body.statement-list;
is $body.statements[0].expression.^name, 'RakuAST::Type::Simple',
    'a captured type name renders as Type::Simple in the body';

# --- write side: EVAL of the round-tripped tree --------------------------------
my $class = EVAL Q{class D {
    method m(::?CLASS:D: $x) { "m $x" }
    method n($s: $y) { "n {$s.^name.chars > 0} $y" }
    method o(::?CLASS:U:) { "o" }
    method p(::T ::?CLASS:D: $x) { "p {T.^name eq self.^name}" }
}; D}.AST;
is $class.new.m(1), 'm 1', 'a typed synthesized invocant binds self';
is $class.new.n(2), 'n True 2', 'a named invocant binds its variable';
is $class.o, 'o', 'a type-object invocant method runs on the type';
is $class.new.p(3), 'p True', 'a captured invocant binds its capture';
dies-ok { $class.m(1) }, 'the :D invocant still rejects a type object';
