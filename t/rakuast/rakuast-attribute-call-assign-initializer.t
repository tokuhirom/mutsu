use experimental :rakuast;
use Test;

# `has R $.r .= new(...)` (#18621): an `Initializer::CallAssign`, as in
# Rakudo 2026.11, which `visit-children` does not walk; the call on the
# declared type is the attribute's `Trait::WillBuild`, placed.

plan 8;

sub decl($src) { $src.AST.statements[0].expression.body.body.statement-list.statements[*-1].expression }

my $d = decl(Q[class R { has $!n; has R $.r .= new(:$!n) }]);
is $d.initializer.^name, 'RakuAST::Initializer::CallAssign', 'the initializer is a CallAssign';
is $d.initializer.postfixish.^name, 'RakuAST::Call::Method', 'over the method call';
is $d.traits[0].^name, 'RakuAST::Trait::WillBuild', 'the attribute has a WillBuild trait';
my @visited;
$d.visit-children: { @visited.push(.^name) };
is @visited.join(' '), 'RakuAST::Type::Simple RakuAST::Trait::WillBuild',
    'the CallAssign is not visited';
is decl(Q[class S { has $.x = 5 }]).initializer.^name, 'RakuAST::Initializer::Assign',
    '= stays an Assign';

class P { has $.v = 1 }
class R1 { has P $.p .= new(:v(5)) }
is R1.new.p.v, 5, 'the default is built';
is EVAL(Q[class P2 { has $.v = 1 }; class R2 { has P2 $.p .= new(:v(7)) }; R2.new.p.v].AST), 7,
    'EVAL of the tree builds it';
is EVAL(Q[class R3 { has Int $.y .= new }; R3.new.y].AST), 0, 'with no arguments';
