use experimental :rakuast;
use Test;

# `.origin` (#15780), measured on rakudo 2026.11: the terms of a method body
# keep their origins when one is written twice (the package body is no
# longer copied before conversion); a variable glued to a prefix operator
# (`⚛$n`) is placed; an object hash (`:{ ... }`) starts at its `{`; an
# attributive parameter target (`:&!f`) visits a `Var::Attribute`.

plan 5;
sub placed($src) {
    my @l;
    sub w($n) {
        my $class = $n.^name.subst('RakuAST::', '');
        with $n.origin { @l.push($class ~ '@' ~ .from ~ '-' ~ .to) unless $class ~~ /Statement/ }
        $n.visit-children(&w);
    }
    w($src.AST);
    @l.join(' ')
}
is placed(Q[class A { has &!g; method run { &!g() ~ (&!g).WHAT.^name } }]),
    'Class@0-60 Name@6-7 Block@8-60 Blockoid@8-60 VarDeclaration::Simple@10-17 Method@19-58 Name@26-29 Blockoid@30-58 ApplyInfix@32-56 Infix@38-39 ArgList@32-56 ApplyPostfix@32-37 Var::Attribute@32-35 Call::Term@35-37 ArgList@36-36 ApplyPostfix@40-56 ApplyPostfix@40-50 Var::Attribute@41-44 Call::Method@46-50 Name@46-50 ArgList@50-50 Call::MetaMethod@52-56 ArgList@56-56',
    'repeated attribute terms in a one-line method body';
is placed(Q[class D { has $!x; method run { $!x ~ $!x } }]),
    'Class@0-45 Name@6-7 Block@8-45 Blockoid@8-45 VarDeclaration::Simple@10-17 Method@19-43 Name@26-29 Blockoid@30-43 ApplyInfix@32-41 Infix@36-37 ArgList@32-41 Var::Attribute@32-35 Var::Attribute@38-41',
    'a repeated attribute in a method body';
is placed(Q[my atomicint $n = 5; say ⚛$n]),
    'VarDeclaration::Simple@0-19 Type::Simple@3-12 Name@3-12 Initializer::Assign@16-19 IntLiteral@18-19 Call::Name::WithoutParentheses@21-28 Name@21-24 ArgList@25-28 ApplyPrefix@25-28 Prefix@25-26 Var::Lexical@26-28',
    'a variable glued to a prefix operator';
is placed(Q[my %o = :{ 1 => 2 }; say :{}.elems]),
    'VarDeclaration::Simple@0-19 Initializer::Assign@6-19 Circumfix::HashComposer@9-19 ApplyInfix@11-17 Infix@13-15 ArgList@11-17 IntLiteral@11-12 IntLiteral@16-17 Call::Name::WithoutParentheses@21-34 Name@21-24 ArgList@26-34 ApplyPostfix@26-34 Circumfix::HashComposer@26-28 Call::Method@29-34 Name@29-34 ArgList@34-34',
    'an object hash starts at its brace';
is placed(Q[class C { has &!f; submethod BUILD(:&!f = WhateverCode) { } }]),
    'Class@0-61 Name@6-7 Block@8-61 Blockoid@8-61 VarDeclaration::Simple@10-17 Submethod@19-59 Name@29-34 Signature@35-54 Parameter@35-54 ParameterTarget::Var@36-39 Var::Attribute@36-39 Type::Simple@42-54 Name@42-54 Blockoid@56-59',
    'an attributive parameter target visits its attribute';
