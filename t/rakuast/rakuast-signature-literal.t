use v6;
use Test;

# A signature literal `:(...)` is `FakeSignature(Signature(...))` in `.AST`
# (#7564, S10). The parser folds it to a `Signature` value, which keeps the
# declared parameters it was built from. This file passes under BOTH mutsu and
# raku, so raku is the oracle.

plan 29;

sub expr-of(Str $source) {
    $source.AST.statements[*-1].expression;
}

# --- read direction ---
{
    my $f = expr-of(':(Int $x, Str :$y)');
    isa-ok $f, RakuAST::FakeSignature, ':(...) is a FakeSignature';
    my $sig = $f.signature;
    isa-ok $sig, RakuAST::Signature, 'it wraps a Signature';
    is $sig.parameters.elems, 2, 'with both parameters';
    is $sig.parameters[0].target.name, '$x', 'the first parameter is $x';
    is $sig.parameters[0].type.name.canonicalize, 'Int', 'typed Int';
    is $sig.parameters[1].names.join(','), 'y', 'the second is the named :$y';
}

is expr-of(':()').signature.parameters.elems, 0, 'an empty signature literal';
is expr-of(':(Int $a --> Bool)').signature.returns.name.canonicalize, 'Bool',
    'a return type is kept';
isa-ok expr-of(':(Int :$a!, *@rest)').signature.parameters[1].slurpy,
    RakuAST::Parameter::Slurpy::Flattened, 'a slurpy parameter is kept';
is expr-of(':($x = 5)').signature.parameters[0].default.value, 5,
    'a default value is kept';

# --- as an operand and in a declaration ---
{
    my $call = expr-of(':(Int $x).WHAT');
    isa-ok $call, RakuAST::ApplyPostfix, 'a postfix call on a signature literal';
    isa-ok $call.operand, RakuAST::FakeSignature, 'its operand is the FakeSignature';
}
{
    my $infix = expr-of(':(Int $x) ~~ :(Int)');
    isa-ok $infix.left, RakuAST::FakeSignature, 'the left operand of ~~';
    isa-ok $infix.right, RakuAST::FakeSignature, 'and the right one';
}
{
    my $decl = expr-of('my $s = :(Int $x)');
    isa-ok $decl.initializer.expression, RakuAST::FakeSignature,
        'the initializer of my $s = :(...)';
}

# --- write direction: EVAL of the round-tripped tree ---
is EVAL(':(Int $x, Str :$y)'.AST).gist, '(Int $x, Str :$y)', 'EVAL gives the signature back';
is EVAL(':(Int $a --> Bool)'.AST).returns.gist, '(Bool)', 'a return type survives EVAL';
is EVAL(':(Int $a, Str $b)'.AST).params.elems, 2, 'params of the EVALed signature';
ok EVAL('(5, "x") ~~ :(Int, Str)'.AST), 'a list smart-matches an EVALed signature';
nok EVAL('(5, 6) ~~ :(Int, Str)'.AST), 'and does not match a wrong one';
ok EVAL('(a => 1).Hash ~~ :(Int :$a)'.AST), 'a hash matches a named signature';
ok EVAL('my $s = :(Int $x); (1,) ~~ $s'.AST), 'a signature kept in a variable';
is EVAL(':(Int $a, $b?)'.AST).params[1].optional.so, True, 'an optional parameter';
is EVAL(':(*@rest)'.AST).params[0].slurpy.so, True, 'a slurpy parameter';
is EVAL(':(Int $x where * > 0)'.AST).params[0].constraint_list.elems, 1,
    'a where clause is kept';
is EVAL(':(:$a, :$b)'.AST).params.map(*.named).join(','), 'True,True',
    'named parameters stay named';
is EVAL(':(&f)'.AST).params[0].name, '&f', 'a callable parameter';
is EVAL(':($ , $)'.AST).params.elems, 2, 'anonymous parameters';
is EVAL(':(Int $x)'.AST).WHAT.^name, 'Signature', 'the value is a Signature';
