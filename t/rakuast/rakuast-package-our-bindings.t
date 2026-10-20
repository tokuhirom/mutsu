use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

my $declaration = Q[module BindingShape { our $tag = 'P'; our sub read { $tag } }].AST.statements[0].expression;
isa-ok $declaration, RakuAST::Module, 'a module retains its declaration node';
is $declaration.body.body.statement-list.statements[0].expression.scope, 'our',
    'the package variable retains its our scope';
is $declaration.body.body.statement-list.statements[1].expression.scope, 'our',
    'the package routine retains its our scope';

for
    Q[our $tag = 'GLOBAL'; module BindingA { our $tag = 'A'; our sub read { $tag } }; BindingA::read() ~ ',' ~ BindingA::read()], 'A,A',
    Q[my $tag = 'caller'; module BindingB { our $tag = 'B'; our sub read { $tag } }; BindingB::read() ~ ',' ~ BindingB::read() ~ '|' ~ $tag], 'B,B|caller',
    Q[our $tag = 'GLOBAL'; module BindingC { our $tag = 'C'; our sub read { $tag } }; module BindingD { our $tag = 'D'; our sub read { $tag } }; BindingC::read() ~ ',' ~ BindingD::read() ~ ',' ~ BindingC::read() ~ ',' ~ BindingD::read()], 'C,D,C,D',
    Q[my $tag = 'caller'; module BindingE { our $tag = 'E'; our sub read { $tag } }; my $first = BindingE::read(); $BindingE::tag = 'changed'; $first ~ ',' ~ BindingE::read() ~ '|' ~ $tag], 'E,changed|caller',
    Q[my $tag = 'caller'; module BindingF { our $tag = 'F'; our sub read { $tag }; our sub write($value) { $tag = $value } }; my $first = BindingF::read(); BindingF::write('changed'); $first ~ ',' ~ BindingF::read() ~ '|' ~ $tag], 'F,changed|caller',
    Q[my $tag = 'caller'; module BindingG { our $tag = 'G'; our sub read { $tag }; our sub forward { read() } }; BindingG::forward() ~ ',' ~ BindingG::forward() ~ '|' ~ $tag], 'G,G|caller',
    Q[my $tag = 'caller'; module BindingH { our $tag = 'H'; our sub read { $tag } }; sub caller($tag) { BindingH::read() ~ '|' ~ $tag }; caller('argument') ~ '|' ~ $tag], 'H|argument|caller',
    Q[my $tag = 'root'; module BindingLexical { my $tag = 'lexical'; our sub read { $tag } }; BindingLexical::read() ~ ',' ~ BindingLexical::read() ~ '|' ~ $tag], 'lexical,lexical|root',
    Q[my $tag = 'root'; module BindingOuter { our sub read { $tag } }; $BindingOuter::tag = 'external'; BindingOuter::read() ~ ',' ~ BindingOuter::read()], 'root,root',
    Q[my $tag = 'root'; module BindingParam { our $tag = 'package'; our sub read($tag) { $tag } }; BindingParam::read('argument') ~ '|' ~ $tag], 'argument|root',
    Q[my $tag = 'root'; class BindingClass { our $tag = 'class'; our sub read { $tag } }; BindingClass::read() ~ ',' ~ BindingClass::read() ~ '|' ~ $tag], 'class,class|root',
    Q[my $tag = 'root'; module BindingNested { our $tag = 'outer'; module Inner { our sub read { $tag } } }; BindingNested::Inner::read() ~ ',' ~ BindingNested::Inner::read() ~ '|' ~ $tag], 'outer,outer|root',
    Q[my $tag = 'root'; module BindingGeneral { our $tag = 'general'; our sub read($n where * >= 0) { $tag ~ '/' ~ $n } }; BindingGeneral::read(1) ~ '|' ~ $tag], 'general/1|root',
    Q[my Int $tag = 0; module BindingInt { our $tag = 1; our sub read { $tag } }; BindingInt::read() ~ ',' ~ BindingInt::read() ~ '|' ~ $tag], '1,1|0',
    Q[my int $tag = 0; module BindingNative { our $tag = 2; our sub read { $tag } }; BindingNative::read() ~ ',' ~ BindingNative::read() ~ '|' ~ $tag], '2,2|0',
    Q[my @tag = 0; module BindingArray { our @tag = 1,2; our sub read { @tag.elems } }; BindingArray::read() ~ ',' ~ BindingArray::read() ~ '|' ~ @tag.join(',')], '2,2|0',
    Q[my %tag = x => 0; module BindingHash { our %tag = x => 3; our sub read { %tag<x> } }; BindingHash::read() ~ ',' ~ BindingHash::read() ~ '|' ~ %tag<x>], '3,3|0',
    Q[my $tag = 0; sub bump($x is rw) { $x += 4 }; module BindingRw { our $tag = 1; our sub change { bump($tag); $tag } }; BindingRw::change() ~ ',' ~ BindingRw::change() ~ '|' ~ $tag], '5,9|0',
    Q[my $tag = 0; module BindingCell { our $tag = 1; our sub read { $tag } }; my $first = BindingCell::read(); my $alias := $BindingCell::tag; $alias = 8; $first ~ ',' ~ BindingCell::read() ~ '|' ~ $tag], '1,8|0',
    Q[my $tag = 'root'; module BindingMethod { our $tag = 'module'; class Inner { my $tag = 'class'; our sub read { $tag } } }; BindingMethod::Inner::read() ~ ',' ~ BindingMethod::Inner::read() ~ '|' ~ $tag], 'class,class|root',
    Q[my $tag = 'root'; module BindingExpr { our $tag = 'package'; our sub write($value) { ($tag = $value) }; our sub read { $tag } }; BindingExpr::write('changed'); BindingExpr::read() ~ '|' ~ $tag], 'changed|root',
    Q[my $tag = 'root'; module BindingStmt { our $tag = 'package'; our sub write($value) { $tag = $value; 42 }; our sub read { $tag } }; BindingStmt::write('changed') ~ '|' ~ BindingStmt::read() ~ '|' ~ $tag], '42|changed|root',
    Q[module BindingReadonly { our $tag = 'package'; our sub write($value) { $tag = $value }; our sub read { $tag } }; sub caller($tag) { BindingReadonly::write('changed'); BindingReadonly::read() ~ '|' ~ $tag }; caller('argument')], 'changed|argument',
    Q[my Int $tag = 0; module BindingConstraint { our $tag = 'package'; our sub write($value) { $tag = $value }; our sub read { $tag } }; BindingConstraint::write('changed'); BindingConstraint::read() ~ '|' ~ $tag], 'changed|0',
    Q[my @tag = 0; class BindingClassArray { our @tag = 1,2; our sub read { @tag.join(',') } }; BindingClassArray::read() ~ '|' ~ @tag.join(',')], '1,2|0',
    Q[my %tag = x => 0; class BindingClassHash { our %tag = x => 3; our sub read { %tag<x> } }; BindingClassHash::read() ~ '|' ~ %tag<x>], '3|0',
    Q[my $tag = 'root'; my $outside := $tag; class BindingClassOuter { our $tag = 'class'; $outside = 'changed'; our sub read { $tag } }; BindingClassOuter::read() ~ '|' ~ $tag], 'class|changed',
    Q[my Int $tag = 0; sub replace($x is rw) { $x = 'changed' }; module BindingRwConstraint { our $tag = 'package'; our sub change { replace($tag); $tag } }; BindingRwConstraint::change() ~ '|' ~ $tag], 'changed|0',
    Q[module BindingRwReadonly { our $tag = 1; our sub change { sub bump($x is rw) { $x += 4 }; bump($tag); $tag } }; sub caller($tag) { BindingRwReadonly::change() ~ '|' ~ $tag }; caller(10)], '5|10',
    Q[module BindingOwnReadonly { our $tag = 'package'; our sub change($tag) { try { $tag = 'changed' }; $!.defined ~ '|' ~ $tag } }; BindingOwnReadonly::change('argument')], 'True|argument',
    Q[module BindingOwnTyped { our $tag = 'package'; our sub change { my Int $tag = 1; try { $tag = 'changed' }; $!.^name ~ '|' ~ $tag } }; BindingOwnTyped::change()], 'X::TypeCheck::Assignment|1',
    Q[my Int $tag = 0; module BindingDefault { our $tag; our sub read { $tag.^name } }; BindingDefault::read() ~ '|' ~ $tag], 'Any|0',
    Q[my $tag = 0; sub bump(:$x! is rw) { $x += 4 }; module BindingNamedRw { our $tag = 1; our sub change { bump(x => $tag); $tag } }; BindingNamedRw::change() ~ ',' ~ BindingNamedRw::change() ~ '|' ~ $tag], '5,9|0',
    Q[my $tag = 0; sub bump($x is rw) { $x += 4 }; sub relay($x is rw) { bump($x) }; module BindingRelay { our $tag = 1; our sub change { relay($tag); $tag } }; BindingRelay::change() ~ ',' ~ BindingRelay::change() ~ '|' ~ $tag], '5,9|0',
    Q[my $tag = 0; sub read(:$x) { try { $x = 9 }; $x }; module BindingNamedValue { our $tag = 1; our sub change { my $v = read(x => $tag); $tag = 3; $v ~ ',' ~ $tag } }; BindingNamedValue::change() ~ '|' ~ $tag], '1,3|0',
    Q[my $tag = 0; sub read(:$x is copy) { $x = 9; $x }; module BindingNamedCopy { our $tag = 1; our sub change { read(x => $tag) ~ ',' ~ $tag } }; BindingNamedCopy::change() ~ '|' ~ $tag], '9,1|0'
    -> $source, $expected {
    is EVAL($source.AST), $expected, 'AST binding identity: ' ~ $source;
    my $ordinary = $source.subst('Binding', 'OrdinaryBinding', :g);
    is EVAL($ordinary), $expected, 'ordinary binding identity: ' ~ $ordinary;
}

done-testing;
