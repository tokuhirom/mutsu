use v6;
use experimental :rakuast;
use Test;

# The `Name` inside a type or term written as just its name (`Int`,
# `RakuAST::Class`, `True`) spans the same text as the term itself, and a
# `Term::Enum` visits that `Name`. This file passes under both mutsu and raku.

plan 10;

sub names-of(Str $code) {
    my @found;
    sub walk($node) {
        @found.push($node.origin.from ~ '-' ~ $node.origin.to) if $node ~~ RakuAST::Name && $node.origin;
        $node.visit-children(&walk);
    }
    walk($code.AST);
    @found;
}

is-deeply names-of('my $x = Int;'), ['8-11'], 'a type term: the Name spans the type';
is-deeply names-of('Int.new;'), ['0-3', '4-7'], 'a type invocant: the Name spans the type';
is-deeply names-of('my $r = RakuAST::Class;'), ['8-22'], 'a qualified type term';
is-deeply names-of('say True;'), ['0-3', '4-8'], 'a Term::Enum visits a Name that spans it';

my $enum = 'say True'.AST.statements[0].expression.args.args[0];
isa-ok $enum, RakuAST::Term::Enum, 'True is a Term::Enum';
my @children;
$enum.visit-children({ @children.push($_) });
is @children.elems, 1, 'a Term::Enum has one child';
isa-ok @children[0], RakuAST::Name, 'the child is a Name';
is @children[0].origin.from, 4, 'the Name starts where the term does';
is @children[0].origin.to, 8, 'the Name ends where the term does';

my $parametric = 'my $z = Array[Int];'.AST;
ok $parametric, 'a parameterized type still converts';
