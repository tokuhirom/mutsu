use v6;
use Test;

# RakuAST Phase 2 slice 27 (ADR-0011): attribute build-time defaults.
# `has $.x = 5` -> the `= 5` becomes BOTH a `traits => (Trait::WillBuild(5),)`
# and an `initializer`. Expected gists captured verbatim from Rakudo; this file
# passes under BOTH mutsu and raku. Distinct class names because `.AST`
# registers the symbol.

plan 3;

# --- an attribute with a default gets a WillBuild trait + initializer --------
my $g = Q[class D1 { has $.x = 5 }].AST.gist;
ok $g.contains('traits      => (')
    && $g.contains('RakuAST::Trait::WillBuild.new(')
    && $g.contains('initializer => RakuAST::Initializer::Assign.new(')
    && $g.comb(/'RakuAST::IntLiteral.new(5)'/).elems == 2,   # WillBuild + initializer
    'has $.x = 5 -> WillBuild trait and initializer, both carrying 5';

# --- a typed attribute with a default still works ---------------------------
is Q[class D2 { has Int $.z = 10 }].AST.gist.contains('RakuAST::Trait::WillBuild.new('), True,
    'has Int $.z = 10 -> WillBuild trait present';

# --- a plain attribute (no default) has neither trait nor initializer --------
my $plain = Q[class D3 { has $.y }].AST.gist;
is $plain.contains('WillBuild') || $plain.contains('initializer'), False,
    'has $.y -> no WillBuild, no initializer';
