use experimental :rakuast;
use Test;

# An enum body of one element without a comma is that element, not a
# one-operand list, and an enum records the roles it composes as implicit
# `Trait::Does` nodes (#18611), as in Rakudo 2026.11.

plan 12;

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
sub body($src) { $src.AST.statements[0].expression.term.semilist.statements[0].expression }
sub roles($src) { $src.AST.statements[0].expression.traits.map({ .^name eq 'RakuAST::Trait::Does' ?? .type.name.canonicalize !! .^name }).join(' ') }

is body(Q[enum A1 (a=>3)]).^name, 'RakuAST::FatArrow', 'a lone pair is the pair';
is body(Q[enum A2 (a=>3,)]).^name, 'RakuAST::ApplyListInfix', 'with a comma it is a list';
is body(Q[enum A3 (:a(1))]).^name, 'RakuAST::ColonPair::Value', 'a lone colonpair keeps its spelling';
is body(Q[enum A4 (q[a])]).^name, 'RakuAST::QuotedString', 'a lone string is the string';

is placed(Q[enum E (a=>3)]),
    'Type::Enum@0-13 Name@5-6 Circumfix::Parentheses@7-13 SemiList@8-12 FatArrow@8-12 IntLiteral@11-12',
    'the lone pair is placed; the implicit traits are not';
is placed(Q[enum K (:a(1))]),
    'Type::Enum@0-14 Name@5-6 Circumfix::Parentheses@7-14 SemiList@8-13 ColonPair::Value@8-13 Circumfix::Parentheses@10-13 SemiList@11-12 IntLiteral@11-12',
    'the lone colonpair is placed';

is roles(Q[enum B1 <x y>]), 'Enumeration NumericEnumeration', 'a word list is numeric';
is roles(Q[enum B2 (a => "x")]), 'Enumeration StringyEnumeration', 'a string value is stringy';
is roles(Q[our Str enum B3 «:a<x>»]), 'Enumeration StringyEnumeration', 'so is a Str base type';
is roles(Q[enum B4 is export (a => 1)]), 'RakuAST::Trait::Is Enumeration NumericEnumeration',
    'the implicit traits come after the written ones';
unlike Q[enum B5 (a => 1)].AST.statements[0].expression.raku, /Enumeration/,
    '.raku leaves the implicit traits out';
is Q[enum B6 (a => 1)].AST.statements[0].expression.clean-clone.traits.elems, 0,
    'and so does clean-clone';
