use Test;

# A stash lookup over a package a `use`d module provides resolves at parse
# time (`Term::Name`); an unresolvable one stays an argument-less
# `Call::Name`. Expected classes measured on Rakudo 2026.11 (#18252).

plan 8;

sub class-of(Str $code) {
    $code.AST.statements.tail.expression.initializer.expression.^name
}

is class-of('use Test; my $a = Test;'), 'RakuAST::Type::Simple',
    'a used module name is a type';
is class-of('use Test; my $a = Test::;'), 'RakuAST::Term::Name',
    'Test:: resolves';
is class-of('use Test; my $a = Test::EXPORT::;'), 'RakuAST::Term::Name',
    'Test::EXPORT:: resolves';
is class-of('use Test; my $a = Test::EXPORT::DEFAULT::;'), 'RakuAST::Term::Name',
    'Test::EXPORT::DEFAULT:: resolves';
is class-of('use Test; my $a = Test::EXPORT::ALL::;'), 'RakuAST::Term::Name',
    'Test::EXPORT::ALL:: resolves';
is class-of('use Test; my $a = Test::Nope::;'), 'RakuAST::Call::Name',
    'an unknown package under a used module stays a call';
is class-of('use NativeCall; my $a = NativeCall::Types::;'), 'RakuAST::Term::Name',
    'a package the used module declares resolves';
is class-of('use NativeCall; my $a = NativeCall::Types::CArray::;'), 'RakuAST::Term::Name',
    'so does a class inside it';
