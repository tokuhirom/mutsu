use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# GH #10653: a package-qualified variable (`$Foo::v`) is a
# `RakuAST::Var::Package` carrying the segmented `Name` and the sigil, not a
# `Var::Lexical` of the whole spelling. Shapes measured on Rakudo 2026.09.

plan 16;

sub expr($src) { $src.AST.statements[*-1].expression }

is expr(Q|$Foo::v|).raku, q:to/END/.chomp, 'a qualified scalar';
RakuAST::Var::Package.new(
  name  => RakuAST::Name.from-identifier-parts("Foo","v"),
  sigil => "\$"
)
END
is expr(Q|@Foo::v|).sigil, '@', 'the array sigil';
is expr(Q|%Foo::v|).sigil, '%', 'the hash sigil';
is expr(Q|&Foo::v|).sigil, '&', 'the code sigil';
is expr(Q|$A::B::c|).name.parts.map(*.name).join('|'), 'A|B|c',
    'a multi-segment name is walkable';
is expr(Q|$GLOBAL::x|).^name, 'RakuAST::Var::Package', 'a pseudo-package variable';
is expr(Q|$Foo::v = 3|).left.^name, 'RakuAST::Var::Package',
    'the target of an assignment';
is expr(Q|my $x; $x|).^name, 'RakuAST::Var::Lexical', 'an unqualified variable stays lexical';

my $hand = RakuAST::Var::Package.new(
    name  => RakuAST::Name.from-identifier-parts("GLOBAL", "vpg"),
    sigil => '$',
);
ok $hand ~~ RakuAST::Var, 'Var::Package is a Var';
ok $hand ~~ RakuAST::Term, 'Var::Package is a Term';

our $vpg = 9;
is EVAL($hand), 9, 'a hand-built Var::Package lowers through EVAL';
is EVAL(Q|package VPT1 { our $v = 11 }; $VPT1::v|.AST), 11, 'a scalar round-trips';
is EVAL(Q|package VPT2 { our @v = 1, 2 }; @VPT2::v.elems|.AST), 2, 'an array round-trips';
is EVAL(Q|package VPT3 { our %v = a => 3 }; %VPT3::v<a>|.AST), 3, 'a hash round-trips';
is EVAL(Q|package VPT4 { our $v = 1 }; $VPT4::v = 5; $VPT4::v|.AST), 5,
    'an assignment to a qualified variable round-trips';
is EVAL(Q|&CORE::uc("x")|.AST), 'X', 'a qualified code variable round-trips';
