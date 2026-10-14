use Test;
use NativeCall;
use experimental :rakuast;

# NativeCall's `HAS` declarator (an attribute stored by value inside a
# `repr('CStruct')` class) in RakuAST, measured on rakudo 2026.09: a
# `VarDeclaration::Simple` with `scope => "HAS"`. The round trip keeps it
# embedded, so the struct's size is C's.

plan 5;

class HasPoint is repr('CStruct') {
    has int32 $.x is rw;
    has int32 $.y is rw;
}

my $src = Q[class HasLine is repr('CStruct') { HAS HasPoint $.from; HAS HasPoint $.to; has int32 $.width is rw; }];
my $members = $src.AST.statements[0].expression.body.body.statement-list.statements;
my $from = $members[0].expression;
isa-ok $from, RakuAST::VarDeclaration::Simple, '`HAS` declares a VarDeclaration::Simple';
is $from.scope, 'HAS', '... whose scope is HAS';
is $members[2].expression.scope, 'has', 'a plain attribute keeps the `has` scope';

# A second name: rakudo declares the class of an `.AST` at `.AST` time.
my $line = EVAL $src.subst('HasLine', 'HasLine2').AST;
is nativesizeof($line), 20, 'the round trip embeds both points: 8 + 8 + 4 bytes';
isa-ok $line.new.from, HasPoint, 'an embedded member reads back as its class';
