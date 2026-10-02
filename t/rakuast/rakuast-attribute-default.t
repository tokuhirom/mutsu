use v6;
use Test;

# RakuAST Phase 2 slice 27 (ADR-0011): attribute build-time defaults.
# `has $.x = 5` -> the `= 5` becomes BOTH an implicit `Trait::WillBuild` and
# an `initializer`. Measured on rakudo 2026.09, the gist omits the implicit
# trait and only `.traits` answers it. This file passes under BOTH mutsu and
# raku. Distinct class names because `.AST` registers the symbol.

plan 4;

sub attr(Str $src) {
    $src.AST.statements[0].expression.body.body.statement-list.statements[0].expression
}

# --- an attribute with a default gets a WillBuild trait + initializer --------
{
    my $a = attr(Q[class D1 { has $.x = 5 }]);
    ok $a.traits.elems == 1 && $a.traits[0].^name eq 'RakuAST::Trait::WillBuild'
        && $a.traits[0].gist.contains('RakuAST::IntLiteral.new(5)'),
        'has $.x = 5 -> a WillBuild trait carrying 5';
    my $g = $a.gist;
    ok $g.contains('initializer => RakuAST::Initializer::Assign.new(')
        && !$g.contains('WillBuild'),
        'and an initializer, while the gist omits the implicit trait';
}

# --- a typed attribute with a default still works ---------------------------
is attr(Q[class D2 { has Int $.z = 10 }]).traits[0].^name, 'RakuAST::Trait::WillBuild',
    'has Int $.z = 10 -> WillBuild trait present';

# --- a plain attribute (no default) has neither trait nor initializer --------
my $plain = Q[class D3 { has $.y }].AST.gist;
is $plain.contains('WillBuild') || $plain.contains('initializer'), False,
    'has $.y -> no WillBuild, no initializer';
