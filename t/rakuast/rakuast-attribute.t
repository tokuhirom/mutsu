use v6;
use Test;

# RakuAST Phase 2 slice 14 (ADR-0011): class attributes (`has $.x`).
# An attribute is a VarDeclaration::Simple with `scope => "has"` and a `twigil`
# (`.` public accessor / `!` private). Expected gists captured verbatim from
# Rakudo; this file passes under BOTH mutsu and raku. Distinct class names are
# used because `.AST` registers the symbol.

plan 4;

# --- public attribute `$.x` -------------------------------------------------
my $pub = Q[class A1 { has $.x }].AST.gist;
ok $pub.contains('expression => RakuAST::VarDeclaration::Simple.new(')
    && $pub.contains('scope       => "has"')
    && $pub.contains('sigil       => "\$"')
    && $pub.contains('twigil      => "."')
    && $pub.contains('desigilname => RakuAST::Name.from-identifier("x")'),
    'has $.x -> VarDeclaration::Simple with scope has and twigil .';

# --- private attribute `$!y` uses the `!` twigil ----------------------------
is Q[class A2 { has $!y }].AST.gist.contains('twigil      => "!"'), True, 'has $!y -> twigil !';

# --- typed attribute keeps its Type::Simple (implicit type default ignored) --
my $typed = Q[class A3 { has Int $.z }].AST.gist;
ok $typed.contains('scope       => "has"')
    && $typed.contains('type        => RakuAST::Type::Simple.new(')
    && $typed.contains('RakuAST::Name.from-identifier("Int")')
    && $typed.contains('twigil      => "."')
    && !$typed.contains('initializer'),
    'has Int $.z -> Type::Simple, no spurious initializer';

# --- array attribute `@.items` ----------------------------------------------
is Q[class A4 { has @.items }].AST.gist.contains('sigil       => "\@"'), True,
    'has @.items -> array sigil';
