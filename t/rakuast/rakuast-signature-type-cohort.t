use Test;
use experimental :rakuast;

# ADR-10723 S10: declaring-signature contracts and role arguments use the
# same compiler paths after the RakuAST round trip. Read shapes were measured
# with Rakudo 2026.09; metadata omitted by its renderer must still execute.
sub decl(Str $source) { $source.AST.statements[0].expression }

my $optional = decl(Q[my ($a, $b?) := (1,)]).signature.parameters[1];
ok $optional.optional, 'optional declaration element keeps its marker';
my $defaulted = decl(Q[my ($a, $b = 5) := (1,)]).signature.parameters[1];
isa-ok $defaulted.default, RakuAST::IntLiteral, 'element default is an expression';
is $defaulted.default.value, 5, 'element default value';
is EVAL(Q[my ($a, $b?) := (1,); $b.raku].AST), 'Mu', 'unfilled optional binding';
is EVAL(Q[my ($a, Int $b?) := (1,); $b.raku].AST), 'Int', 'typed optional binding';
is EVAL(Q[my ($a, $b = 5) := (1,); $b].AST), 5, 'missing value evaluates default';
is EVAL(Q[my ($a, $b = 5) := (1, 8); $b].AST), 8, 'supplied value wins over default';
is EVAL(Q[my $n = 0; my ($a, $b = ++$n) := (1, 8); $n].AST), 0,
    'a default is evaluated only when needed';
is EVAL(Q[my $n = 0; my ($a, $b = ++$n) := (1,); $n].AST), 1,
    'default expression runs once';
dies-ok { EVAL(Q[my ($a, $b?) := (1, 2, 3)].AST) }, 'optional binding still checks arity';

my $literal = decl(Q[my ("foo") = "foo"]).signature.parameters[0];
is $literal.value, 'foo', 'literal postconstraint keeps its value';
is $literal.target.name, '$', 'literal postconstraint has an anonymous target';
isa-ok $literal.where, RakuAST::Block, 'literal postconstraint has the generated matcher';
like $literal.raku, /'RakuAST::Term::Declaration.new'/, 'matcher refers to the declaration value';
is EVAL(Q[my ($a, "foo") = 5, "foo"; $a].AST), 5, 'matching literal permits assignment';
dies-ok { EVAL(Q[my ($a, "foo") = 5, "bar"].AST) }, 'nonmatching literal rejects assignment';
is EVAL(Q[(my ($a, "foo") = 5, "foo").join(",")].AST), '5,foo',
    'literal assignment returns the declared values';
is EVAL(Q[my ($a, $b) is default(7); $b].AST), 7,
    'group default survives the omitted trait representation';

is EVAL(Q[class Captured { method m(::T: $x --> T) { $x } }; my $o = Captured.new; $o.m($o).^name].AST),
    'Captured', 'a bare capture invocant does not consume a positional';
is EVAL(Q[class CapturedZero { method m(::T: --> T) { self } }; CapturedZero.new.m.^name].AST),
    'CapturedZero', 'capture invocant with no positionals';
is EVAL(Q[class Nominal { method m(::T Nominal:D: $x --> T) { $x } }; my $o = Nominal.new; $o.m($o).^name].AST),
    'Nominal', 'capture and nominal invocant constraints remain independent';
unlike Q[class CaptureShape { method m(::T CaptureShape:D: $x) { $x } }].AST.raku,
    /'invocant'/, 'capture invocant metadata follows Rakudo constructor rendering';

my $role = Q[role Flags[:$yes, :$no, :$text] { method result { "$yes/$no/$text" } }; class UsesFlags does Flags[:yes, :!no, :text<words>] {}; UsesFlags.new.result];
my $role-tree = $role.AST;
my $tree = $role-tree.statements[1].expression.traits[0].type.args;
like $tree.raku, /'ColonPair::True.new'/, 'true type argument uses common pair representation';
like $tree.raku, /'ColonPair::False.new'/, 'false type argument uses common pair representation';
like $tree.raku, /'ColonPair::Value.new'/, 'word-quote type argument uses value pair';
is EVAL($role-tree), 'True/False/words', 'boolean and word-quote type arguments execute';
is EVAL(Q[role Named[:$n] { method result { $n } }; my $n = 7; (1 but Named[:$n]).result].AST),
    7, 'variable type argument keeps its value';

my $built = RakuAST::VarDeclaration::Signature.new(
    signature => RakuAST::Signature.new(parameters => (
        RakuAST::Parameter.new(target => RakuAST::ParameterTarget::Var.new(name => '$built-value'),
            default => RakuAST::IntLiteral.new(42)),
    )),
    initializer => RakuAST::Initializer::Bind.new(
        RakuAST::Circumfix::Parentheses.new(RakuAST::SemiList.new)),
);
is EVAL(RakuAST::StatementList.new(
    RakuAST::Statement::Expression.new(expression => $built),
    RakuAST::Statement::Expression.new(expression => RakuAST::Var::Lexical.new('$built-value')),
)), 42, 'constructed optional declaring signature uses the shared binder';
my $match-value = RakuAST::Parameter.new(value => 'foo');
is $match-value.value, 'foo', 'constructed literal parameter retains its value';
ok RakuAST::Parameter.new(default-rw => True,
    target => RakuAST::ParameterTarget::Var.new(name => '$rw')).default-rw,
    'constructed declaration parameter retains its writable default';
my $bad-decl = RakuAST::VarDeclaration::Signature.new(
    signature => RakuAST::Signature.new(parameters => ($match-value,)),
    initializer => RakuAST::Initializer::Assign.new(
        RakuAST::QuotedString.new(segments => (RakuAST::StrLiteral.new('bar'),))),
);
dies-ok { EVAL($bad-decl) }, 'constructed literal postconstraint rejects a mismatch';

is EVAL(Q[my ($a, $b = 8) := (1,) and $b + 2].AST), 10,
    'signature declaration stays the left operand of a loose logical tail';
is EVAL(Q[class Receiver {}; my $m = anon method (\SELF: |) { SELF.^name }; $m(Receiver.new)].AST),
    'Receiver', 'folded sigilless method invocant stays a declared term';

done-testing;
