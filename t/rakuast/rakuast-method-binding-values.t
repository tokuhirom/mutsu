use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

my $class-shape = Q[class BindingShape { our &alias = method actual($x) { $x } }].AST.statements[0].expression;
my @statements = $class-shape.body.body.statement-list.statements;
is @statements.elems, 1, 'the source body contains one variable declaration';
my $decl = @statements[0].expression;
isa-ok $decl, RakuAST::VarDeclaration::Simple, 'method assignment remains a variable declaration';
is $decl.scope, 'our', 'the variable keeps its scope';
is $decl.sigil, '&', 'the variable keeps its sigil';
is $decl.desigilname.canonicalize, 'alias', 'the variable keeps its independent name';
isa-ok $decl.initializer, RakuAST::Initializer::Assign, 'assignment initializer is retained';
my $method = $decl.initializer.expression;
isa-ok $method, RakuAST::Method, 'initializer is a Method';
is $method.name.canonicalize, 'actual', 'method keeps its independent name';

my $built-method = RakuAST::Method.new(
    name => RakuAST::Name.from-identifier('actual'),
    body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(42))
    ))
);
my $built-variable = RakuAST::VarDeclaration::Simple.new(
    scope => 'our', sigil => '&',
    desigilname => RakuAST::Name.from-identifier('alias'),
    initializer => RakuAST::Initializer::Assign.new($built-method)
);
my $built-class = RakuAST::Class.new(
    name => RakuAST::Name.from-identifier('BindingBuilt'),
    body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new(
        RakuAST::Statement::Expression.new(expression => $built-variable)
    )))
);
my $built = EVAL($built-class);
is $built.actual, 42, 'hand-built initializer publishes its class method';
my &built-alias = $built.WHO{'&alias'};
isa-ok &built-alias, Method, 'hand-built variable receives a Method';
is built-alias($built), 42, 'hand-built alias invokes its compiled body';
is &built-alias.name, 'actual', 'hand-built alias retains the method name';
for RakuAST::Method.new, RakuAST::Submethod.new -> $node {
    is $node.multiness, '', 'routine constructor has empty multiness by default';
    is $node.private, False, 'routine constructor is public by default';
    isa-ok $node.body, RakuAST::Blockoid, 'routine constructor supplies an empty body';
}
my $default-invocant = Q[method ($c = 42:) { $c }].AST.statements[0].expression.signature.parameters[0];
ok $default-invocant.invocant, 'a defaulted invocant retains its invocant flag';
is $default-invocant.default.value, 42, 'a defaulted invocant retains its initializer';

for
    Q[use MONKEY-TYPING; multi sub trait_mod:<is>(Method $m,:$paint!) { $m.wrap: method (|) { state $n=0; callsame() ~ ':' ~ ++$n } }; class BindingAugmentWrap {}; augment class BindingAugmentWrap { our &alias = method actual() is paint {'v'} }; (BindingAugmentWrap.actual, BindingAugmentWrap::alias(BindingAugmentWrap)).join(',')], 'v:1,v:2',
    Q[class BindingMultiOrigin { multi method actual(Str $x) { 'str' }; our &alias = multi method actual(Int $x) { $x } }; (&BindingMultiOrigin::alias.multi, &BindingMultiOrigin::alias.dispatcher.^name, &BindingMultiOrigin::alias.dispatcher.candidates.elems, (&BindingMultiOrigin::alias.dispatcher)(BindingMultiOrigin,'x')).join(',')], 'True,Method,2,str',
    Q[use MONKEY-TYPING; class BindingAugment {}; augment class BindingAugment { our &alias = method actual() {42} }; (BindingAugment.actual, BindingAugment::alias(BindingAugment)).join(',')], '42,42',
    Q[use MONKEY-TYPING; BEGIN our @BindingAugmentSeen; multi sub trait_mod:<is>(Method $m,:$paint!) { @BindingAugmentSeen.push($m.name) }; class BindingAugmentTrait {}; augment class BindingAugmentTrait { our &alias = method actual() is paint {42} }; (BindingAugmentTrait.actual, BindingAugmentTrait::alias(BindingAugmentTrait), @BindingAugmentSeen.elems).join(',')], '42,42,1',
    Q[class BindingDefaultReceiver { our &alias = method actual($c = BindingDefaultReceiver:) { $c === BindingDefaultReceiver } }; BindingDefaultReceiver::alias()], True,
    Q[class BindingExplicitReceiver { our &alias = method actual($c:) { $c } }; BindingExplicitReceiver::alias(42)], 42,
    Q[my &alias = method ($c = 42:) { $c }; alias()], 42,
    Q[my &alias = method ($c = 42) { $c }; alias(Any)], 42,
    Q[class BindingInvocant { our &alias = method actual($this:) { $this.WHICH eq self.WHICH } }; BindingInvocant::alias(BindingInvocant)], True,
    Q[class BindingLexicalInvocant { our &alias = method actual($self:) { $self.WHICH eq self.WHICH } }; BindingLexicalInvocant::alias(BindingLexicalInvocant)], True,
    Q[class BindingReturn { our &alias = method actual(--> Int) {42} }; (BindingReturn::alias(BindingReturn), &BindingReturn::alias.returns.^name).join(',')], '42,Int',
    Q[class BindingNamed { our &alias = method actual($x) { $x }; }; BindingNamed::alias(BindingNamed, 42)], 42,
    Q[class BindingAnonymous { our &alias = method ($x) { $x }; }; BindingAnonymous::alias(BindingAnonymous, 42)], 42,
    Q[class BindingIdentity { our &alias = method actual() { state $n=0; ++$n } }; my $o=BindingIdentity.new; ($o.actual, BindingIdentity::alias($o), $o.actual, BindingIdentity::alias($o)).join(',')], '1,1,2,2',
    Q[class BindingMy { my &alias = method actual($x) { $x }; method run { alias(self,42) } }; BindingMy.new.run], 42,
    Q[class BindingAttributes { has $.value=42; my &alias = method actual() { $!value }; method run { alias(self) } }; BindingAttributes.new.run], 42,
    Q[role BindingValueRole { my &alias = method actual() { state $n=0; ++$n }; method run { alias(self) } }; class BindingValueA does BindingValueRole {}; class BindingValueB does BindingValueRole {}; (BindingValueA.actual,BindingValueA.run,BindingValueA.actual,BindingValueA.run,BindingValueB.actual,BindingValueB.run).join(',')], '1,1,2,2,1,1',
    Q[class BindingState { state &alias = method actual($x) { $x }; method run { alias(self,42) } }; BindingState.new.run], 42,
    Q[class BindingMethodScope { our &alias = my method actual($x) { $x }; method run { actual(self,42) } }; BindingMethodScope.new.run], 42,
    Q[class BindingOurMethod { our &alias = our method actual($x) { $x } }; BindingOurMethod::actual(BindingOurMethod,42)], 42,
    Q[class BindingMulti { multi method actual(Str $x) { 'str' }; our &alias = multi method actual(Int $x) { $x }; }; BindingMulti::alias(BindingMulti,42)], 42,
    Q[role BindingRole { method m is rw { state $x=0; $x } }; class BindingA does BindingRole {}; class BindingB is BindingA {}; class BindingC does BindingRole {}; BindingA.new.m=17; (BindingA.new.m,BindingB.new.m,BindingC.new.m).join(',')], '17,17,0',
    Q[class BindingTail { method m is rw { state $ = 0 } }; BindingTail.new.m=42; BindingTail.new.m], 42,
    Q[class BindingImplicitTail { method m is rw { $ } }; BindingImplicitTail.new.m=42; BindingImplicitTail.new.m], 42,
    Q[my &f = sub {42}; try { f(:extra) }; $!.defined], True,
    Q[class BindingReceiver { our &alias = method actual() {42} }; class BindingWrong {}; try { BindingReceiver::alias(BindingWrong) }; $! ~~ X::TypeCheck::Binding::Parameter], True,
    Q[BEGIN our @BindingSeen; multi sub trait_mod:<is>(Method $m,:$paint!) { @BindingSeen.push($m.WHICH.Str) }; class BindingTraits { our &alias = method actual() is paint {42} }; @BindingSeen.elems], 1,
    Q[multi sub trait_mod:<is>(Method $m,:$paint!) { $m.wrap: method (|) { state $n=0; callsame() ~ ':' ~ ++$n } }; class BindingWrap { our &alias = method actual() is paint {'v'} }; (BindingWrap.actual, BindingWrap::alias(BindingWrap), BindingWrap.actual, BindingWrap::alias(BindingWrap)).join(',')], 'v:1,v:2,v:3,v:4'
    -> $source, $expected {
    is EVAL($source), $expected, "ordinary: $source";
    my $ast-source = $source.subst('Binding', 'AstBinding', :g).subst('X::TypeCheck::AstBinding', 'X::TypeCheck::Binding', :g);
    is EVAL($ast-source.AST), $expected, "AST: $source";
}
done-testing;
