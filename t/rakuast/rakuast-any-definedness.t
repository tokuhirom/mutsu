use v6;
use Test;

# The `:_` smiley is `Type::AnyDefinedness(base-type)`, next to `:D`/`:U`
# which are `Type::Definedness` (#7564, S10). This file passes under BOTH mutsu
# and raku, so raku is the oracle.

plan 20;

sub expr-of(Str $source) {
    $source.AST.statements[*-1].expression;
}

# --- read direction: a type used as a term ---
for <Int Str Any int8 HyperWhatever> -> $name {
    my $t = expr-of("{$name}:_");
    isa-ok $t, RakuAST::Type::AnyDefinedness, "{$name}:_ is a Type::AnyDefinedness";
    isa-ok $t.base-type, RakuAST::Type::Simple, "{$name}:_ has a Type::Simple base";
}

is expr-of('Int:_').base-type.name.canonicalize, 'Int', 'the base type names the type';

# --- it stays distinct from :D and :U ---
isa-ok expr-of('Int:D'), RakuAST::Type::Definedness, ':D is still a Definedness';
isa-ok expr-of('Int:U'), RakuAST::Type::Definedness, ':U is still a Definedness';

# --- as an invocant and as a parameter type ---
{
    my $call = expr-of('Any:_.WHAT');
    isa-ok $call, RakuAST::ApplyPostfix, 'Any:_.WHAT is a postfix call';
    isa-ok $call.operand, RakuAST::Type::AnyDefinedness, 'its operand is the :_ type';
}
{
    my $sub = 'sub f(Int:_ $x) { $x }'.AST.statements[0].expression;
    my $param = $sub.signature.parameters[0];
    isa-ok $param.type, RakuAST::Type::AnyDefinedness, 'a parameter type keeps :_';
}

# --- write direction: EVAL of the round-tripped tree ---
is EVAL('Int:_.WHAT.gist'.AST), '(Int)', 'EVAL of a :_ term gives the type object';
is EVAL('my sub f(Int:_ $x) { $x }; f(5)'.AST), 5, 'a :_ parameter accepts a defined value';
is EVAL('my sub f(Int:_ $x) { $x }; f(Int)'.AST).gist, '(Int)',
    'and a type object';
is EVAL('my Int:_ $x = 3; $x'.AST), 3, 'a :_ variable type keeps working';
