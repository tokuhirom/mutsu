use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 14;

class TraitFixture::Tag { }
my %seen;
multi trait_mod:<is>(Code:D $handler, TraitFixture::Tag, Pair $pair) {
    %seen{$handler.name || 'anon'} = $pair;
}
multi trait_mod:<is>(Code:D $handler, TraitFixture::Tag) {
    %seen{$handler.name || 'anon'} = 'bare';
}
multi trait_mod:<is>(Method:D $handler, TraitFixture::Tag, Pair $pair) {
    %seen{$handler.name} = $pair;
}

my $typed-trait = Q[sub measured() is TraitFixture::Tag(:tag<x>) { 1 }]
    .AST.statements.head.expression.traits.head;
is $typed-trait.^name, 'RakuAST::Trait::Is', 'a qualified custom trait is Trait::Is';
is $typed-trait.type.^name, 'RakuAST::Type::Simple', 'a qualified custom trait stores a type node';
is $typed-trait.argument.^name, 'RakuAST::Circumfix::Parentheses',
    'a qualified custom trait keeps its parenthesized argument';

my $hidden-trait = Q[sub measured() is hidden-from-backtrace { 1 }]
    .AST.statements.head.expression.traits.head;
is $hidden-trait.name.parts.head.name, 'hidden-from-backtrace',
    'hidden-from-backtrace stays a named Trait::Is';

is EVAL(Q[
    sub rakuast-named() is TraitFixture::Tag(:tag<named>) { 42 }
    rakuast-named()
].AST), 42, 'a qualified type trait compiles and its routine runs';
is %seen<rakuast-named>.key, 'tag', 'the qualified type trait receives the Pair key';
is %seen<rakuast-named>.value, 'named', 'the qualified type trait receives the Pair value';

my $anonymous = EVAL(Q[sub () is TraitFixture::Tag(:tag<anon>) { 43 }].AST);
is $anonymous(), 43, 'an anonymous sub with a qualified trait runs';
is %seen<anon>.key, 'tag', 'the anonymous sub dispatches its qualified trait';

is EVAL(Q[sub rakuast-bare() is TraitFixture::Tag { 44 }; rakuast-bare()].AST), 44,
    'a bare qualified type trait compiles and its routine runs';
is %seen<rakuast-bare>, 'bare', 'a bare type trait dispatches without an argument';

class TraitFixture::Owner {
    method qualified() is TraitFixture::Tag(:tag<method>) { 45 }
}
is TraitFixture::Owner.new.qualified(), 45, 'a method with a qualified type trait runs';
is %seen<qualified>.key, 'tag', 'the method trait receives the Pair key';
is %seen<qualified>.value, 'method', 'the method trait receives the Pair value';
