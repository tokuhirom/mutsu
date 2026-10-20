use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;

multi sub trait_mod:<is>(Mu:U $target, :$paint!) { }
sub namespace-export-check is export { 42 }

lives-ok { EVAL('EXPORT::DEFAULT') }, 'EVAL sees its caller export namespace';
lives-ok { EVAL(q[EVAL('EXPORT::DEFAULT')]) }, 'nested EVAL retains the lexical export namespace';

my $module = Q[module NamespaceShape is paint<blue> is export {}].AST.statements[0].expression;
isa-ok $module, RakuAST::Module, 'module declaration shape';
is $module.traits.elems, 2, 'custom and export traits are retained';
is $module.traits[0].name.canonicalize, 'paint', 'written trait order is retained';
isa-ok $module.traits[0].argument, RakuAST::QuotedString, 'angle argument retains words quoting';
is $module.traits[1].name.canonicalize, 'export', 'export occurs once in written position';
my $package = Q[package NamespacePackage is paint(42) {}].AST.statements[0].expression;
isa-ok $package, RakuAST::Package, 'package declaration shape';
isa-ok $package.traits[0].argument, RakuAST::Circumfix::Parentheses, 'parenthesized trait argument shape';
my $unit = Q[unit module NamespaceUnit is paint; 42].AST.statements[0].expression;
is $unit.scope, 'unit', 'unit module scope';
is $unit.traits[0].name.canonicalize, 'paint', 'unit trait survives body absorption';
is $unit.body.body.statement-list.statements.elems, 1, 'unit body retains its runtime statement';

my $built = RakuAST::Module.new(
    name => RakuAST::Name.from-identifier('NamespaceBuilt'),
    traits => (RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('paint')),),
    body => RakuAST::Block.new(body => RakuAST::Blockoid.new(RakuAST::StatementList.new))
);
lives-ok { EVAL($built) }, 'hand-built module applies its custom trait through normal dispatch';
isa-ok RakuAST::Module.new.body, RakuAST::Block, 'module constructor supplies an empty body';
isa-ok RakuAST::Package.new.body, RakuAST::Block, 'package constructor supplies an empty body';
my $built-package = RakuAST::Package.new(
    name => RakuAST::Name.from-identifier('NamespaceBuiltPackage'),
    traits => (RakuAST::Trait::Is.new(name => RakuAST::Name.from-identifier('paint')),)
);
lives-ok { EVAL($built-package) }, 'hand-built package applies its custom trait';

my $stash-index = Q[class NamespaceStashShape {}; NamespaceStashShape::<Missing>].AST.statements[1].expression;
isa-ok $stash-index.operand, RakuAST::Term::Name, 'stash lookup retains its namespace operand';
isa-ok $stash-index.postfix, RakuAST::Postcircumfix::LiteralHashIndex,
    'a missing stash key remains a subscript rather than a qualified name';

my $manual-export = Q[use lib 't/lib'; use ManualExportStashVarMod :special; Answer];
isa-ok $manual-export.AST.statements[2].expression, RakuAST::Term::Name,
    'a static module scan recognizes a retained stash bind as a value export';
is EVAL($manual-export.AST), 42, 'a statically discovered stash export evaluates';

for
    Q[Distribution::Resource.^name], 'Distribution::Resource',
    Q[Distribution::Resources.^name], 'Distribution::Resources',
    Q[CompUnit::DependencySpecification.^name], 'CompUnit::DependencySpecification',
    Q[IO::Path::Unix.^name], 'IO::Path::Unix',
    Q[package NamespaceParent::Nested {}; NamespaceParent.gist], '(NamespaceParent)',
    Q[module NamespaceModule::Nested {}; NamespaceModule.gist], '(NamespaceModule)',
    Q[my $NamespaceScalar = 19; package NamespaceScalar::Nested {}; $NamespaceScalar], 19,
    Q[my $p = my package NamespaceValue {}; $p === NamespaceValue], True,
    Q[my $p = my module NamespaceModuleValue {}; $p === NamespaceModuleValue], True,
    Q[class NamespaceStash { class Child {} }; (NamespaceStash::<Child> === NamespaceStash::Child) && !NamespaceStash::<Missing>.defined], True,
    Q[class NamespaceStashBind { class Child {} }; NamespaceStashBind::<Child> := 42; (NamespaceStashBind::<Child> == 42) && (NamespaceStashBind::Child.^name eq 'NamespaceStashBind::Child')], True,
    Q[class NamespaceStashAlias {}; my $value = 7; NamespaceStashAlias::<Value> := $value; $value = 9; NamespaceStashAlias::<Value>], 9,
    Q[module NamespaceRelative { class NamespaceRelative::Child { our $value = 17; method value { $value } } }; NamespaceRelative::Child.new.value], 17,
    Q[module NamespaceRoleRelative { role NamespaceRoleRelative::Child { method value { 19 } } }; class NamespaceComposed does NamespaceRoleRelative::Child {}; NamespaceComposed.new.value], 19,
    Q[module NamespaceScalarHead { my $Stem = Int; class Stem::Child { method answer { 42 } } }; NamespaceScalarHead::Stem::Child.new.answer], 42,
    Q[module NamespaceScalarRoleHead { my $Stem = Int; role Stem::Child { method answer { 42 } } }; class NamespaceScalarRoleConsumer does NamespaceScalarRoleHead::Stem::Child {}; NamespaceScalarRoleConsumer.new.answer], 42,
    Q[our $seen; multi sub trait_mod:<is>(Mu:U $target, :$shade!) { $seen = $shade }; module NamespacePaint is shade<blue> {}; $seen], 'blue',
    Q[our $seen; multi sub trait_mod:<is>(Mu:U $target, :$shade!) { $seen = $shade }; package NamespacePaintPackage is shade(42) {}; $seen], 42,
    Q[our $seen; multi sub trait_mod:<is>(Mu:U $target, :$shade!) { $seen = $shade }; module NamespacePaintFlag is shade {}; $seen], True
    -> $source, $expected {
    is EVAL($source.AST), $expected, 'round-trip namespace semantics: ' ~ $source;
    my $ordinary-expected = $expected ~~ Str ?? $expected.subst('Namespace', 'OrdinaryNamespace', :g) !! $expected;
    is EVAL($source.subst('Namespace', 'OrdinaryNamespace', :g)), $ordinary-expected,
        'ordinary namespace semantics: ' ~ $source;
}

for
    Q[my $x = NamespaceLateModule; module NamespaceLateModule {}], X::Undeclared::Symbols,
    Q[my $x = NamespaceLatePackage; package NamespaceLatePackage {}], X::Undeclared::Symbols,
    Q[my $x = NamespaceLateExport; module NamespaceLateExport is export {}], X::Undeclared::Symbols,
    Q[my $x = NamespaceLateVersion; module NamespaceLateVersion:ver<1.0> {}], X::Undeclared::Symbols,
    Q[my $x = NamespaceLatePackageExport; package NamespaceLatePackageExport is export {}], X::Undeclared::Symbols,
    Q[NamespaceAbsent::Child], X::AdHoc,
    Q[module NamespaceUnknown is NamespaceNoParent {}], X::Inheritance::UnknownParent,
    Q[package NamespaceUnknownPackage is NamespaceNoParent {}], X::Inheritance::UnknownParent,
    Q[module NamespaceBuiltinParent is Hash {}], X::Method::NotFound,
    Q[package NamespaceBuiltinPackage is Hash {}], X::Method::NotFound,
    Q[module NamespaceRw is rw {}], X::Method::NotFound,
    Q[package NamespaceHidden is hidden {}], X::Method::NotFound,
    Q[my package NamespaceOuter { package Nested; 42 }], X::UnitScope::TooLate
    -> $source, $exception {
    throws-like { EVAL($source.subst('Namespace', 'OrdinaryNamespace', :g)) }, $exception, 'ordinary declaration validation: ' ~ $source;
    throws-like { EVAL($source.AST) }, $exception, 'AST declaration validation: ' ~ $source;
}

EVAL(q[{ my package NamespacePrivate::Child {} }]);
throws-like { EVAL('NamespacePrivate') },
    X::Undeclared::Symbols, 'a private qualified package does not publish a global ancestor';

throws-like { EVAL('package NamespaceSemicolon; 42') }, X::UnitScope::MustHaveUnit,
    'top-level semicolon package requires unit';

done-testing;
