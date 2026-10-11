use Test;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;

# The declaration stays lexical even when initialization is gated. The AST
# exposes the written signature, rather than the expansion's staging locals.
for (
    Q[my ($a, @b) = (1, 2, 3) if True;],
    Q[my ($a, @b) := (1, [2, 3]) unless False;],
    Q[my ($a, @b) = (.key, |.value) with ('k' => [2, 3]);],
    Q[my ($a, @b) := (1, [2, 3]) without Any;],
) -> $source {
    my $statement = $source.AST.statements[0];
    isa-ok $statement.expression, RakuAST::VarDeclaration::Signature,
        'modified list remains one signature declaration';
    is $statement.expression.signature.parameters.elems, 2,
        'written signature retains both targets';
    ok !$statement.raku.contains('__destructure_tmp__'),
        'internal staging names do not enter the public AST';
}

isa-ok Q[my ($a, $b) = (1, 2) if True].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::If, 'if retains its keyword';
isa-ok Q[my ($a, $b) := (1, 2) unless False].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::Unless, 'unless retains its keyword';
isa-ok Q[my @a = .list with (1, 2)].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::With, 'scalar declaration preserves with';
isa-ok Q[my $a = 1 without Any].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::Without, 'scalar declaration preserves without';

is EVAL(Q[my ($a, @b) = (1, 2, 3) if False; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'false list assignment still declares every target';
is EVAL(Q[my ($a, @b) := (1, [2, 3]) unless True; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'false list binding still declares every target';
is EVAL(Q[my ($a, @b) = (1, 2, 3) if True; "$a/{@b.join(',')}"].AST),
    '1/2,3', 'assignment retains greedy array semantics';
is EVAL(Q[my ($a, @b) := (1, [2, 3]) if True; "$a/{@b.join(',')}"].AST),
    '1/2,3', 'binding retains one array element';
is EVAL(Q[my @x = 2, 3; my ($a, @b) := (1, @x) unless False; @b.push(4); @x.join(',')].AST),
    '2,3,4', 'conditional list binding retains its array alias';

is EVAL(Q[my ($a, @b) = (.key, |.value) with ('k' => [2, 3]); "$a/{@b.join(',')}"].AST),
    'k/2,3', 'list initializer runs under the with topic';
is EVAL(Q[my ($a, @b) = (1, 2, 3) with Any; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'undefined with topic leaves list targets declared';
is EVAL(Q[my @x = 2, 3; my ($a, @b) := (1, @x) with 42; @b.push(4); @x.join(',')].AST),
    '2,3,4', 'with gates the complete binding expansion';
is EVAL(Q[my @x = 2, 3; my ($a, @b) := (1, @x) without Any; @b.push(4); @x.join(',')].AST),
    '2,3,4', 'without gates the complete binding expansion';
is EVAL(Q[my ($a, @b) := (1, [2, 3]) without 42; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'defined without topic leaves binding targets declared';
is EVAL(Q[my @a = .list with (1, 2); @a.join(',')].AST),
    '1,2', 'scalar aggregate declaration uses the topical initializer';
is EVAL(Q[my $a = 7 without Any; $a].AST), 7,
    'scalar initializer runs for an undefined topic';

my $named = Q[my (:$a, :@b) := %(a => 1, b => [2, 3])].AST.statements[0].expression;
isa-ok $named, RakuAST::VarDeclaration::Signature, 'named list retains a canonical signature';
is $named.signature.parameters[0].names[0], 'a', 'named parameter retains its lookup key';
is EVAL(Q[my (:$a, :@b) := %(a => 1, b => [2, 3]); "$a/{@b.join(',')}"].AST),
    '1/2,3', 'named binding uses the same expansion after lowering';
is EVAL(Q[my (:@found, :@absent) := (1, 2, 3).classify({ 'found' }); "{@found.elems}/{@absent.elems}"].AST),
    '3/0', 'absent named array stays empty';

my $declaration = Q[my ($a, @b) := (1, [2, 3])].AST.statements[0].expression;
my $constructed = RakuAST::Statement::Expression.new(
    expression => $declaration,
    condition-modifier => RakuAST::StatementModifier::If.new(RakuAST::IntLiteral.new(0)),
);
lives-ok { EVAL($constructed) }, 'constructed modified declaration shares parser expansion';

is EVAL(Q[my (:$a, :@b) := %(a => 1, b => [2, 3]) if True; "$a/{@b.join(',')}"].AST),
    '1/2,3', 'conditional named binding uses the shared named expansion';
is EVAL(Q[my (:$a, :@b) := %(a => 1, b => [2, 3]) unless True; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'false named binding still declares both targets';
is EVAL(Q[my (:$a, :@b) := $_ with %(a => 1, b => [2, 3]); "$a/{@b.join(',')}"].AST),
    '1/2,3', 'named binding reads the with topic';
is EVAL(Q[my (:$a, :@b) := %(a => 1, b => [2, 3]) without 42; "{$a.defined}/{@b.elems}"].AST),
    'False/0', 'without skips the whole named binding';
is EVAL(Q[my $calls = 0; sub candidate { ++$calls; 1 }; my ($a, $b) = (3, 4) if candidate(); "$a/$b/$calls"].AST),
    '3/4/1', 'effectful condition is evaluated once';
is EVAL(Q[my $calls = 0; my ($a, $b) = (++$calls, 4) if False; $calls].AST),
    0, 'false modifier suppresses initializer side effects';
is EVAL(Q[my @out; { my ($a, $b) = ($_, $_ + 1) if $_ %% 2; @out.push($a) if $a.defined } for 1..4; @out.join(',')].AST),
    '2,4', 'modified declaration composes with its enclosing loop';
isa-ok Q[my $a if False].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::If, 'declaration without initializer keeps its modifier';
isa-ok Q[constant $a = 3 unless True].AST.statements[0].condition-modifier,
    RakuAST::StatementModifier::Unless, 'constant keeps the written modifier';
is EVAL(Q[constant $a = 3 unless True; $a].AST), 3,
    'constant initialization remains unconditional';

is EVAL(Q[my @a = .list with (1, 2); @a.join(',')]), '1,2',
    'ordinary EVAL discards the intermediate topical initializer value';
is EVAL(Q[my ($a, @b) := (1, [2, 3]) without 42; 99]), 99,
    'skipped initializer does not obscure the following statement value';

for (
    RakuAST::StatementModifier::If, RakuAST::StatementModifier::Unless,
    RakuAST::StatementModifier::With, RakuAST::StatementModifier::Without,
    RakuAST::StatementModifier::When, RakuAST::StatementModifier::For,
    RakuAST::StatementModifier::Given, RakuAST::StatementModifier::While,
    RakuAST::StatementModifier::Until,
) -> $class {
    is $class.new(RakuAST::IntLiteral.new(7)).expression.value, 7,
        'modifier constructor retains its expression operand';
}

done-testing;
