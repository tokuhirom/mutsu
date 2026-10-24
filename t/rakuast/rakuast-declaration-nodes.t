use v6;
use experimental :rakuast;
use Test;

# Binding and `does` declarations are typed parser nodes (ADR-10723 Stage 2):
# `.AST` renders them from the node, execution lowers the same node.

plan 18;

my $bind = Q|my @a := (1, 2)|.AST.statements[0].expression;
is $bind.^name, 'RakuAST::VarDeclaration::Simple', 'bound array declaration';
is $bind.initializer.^name, 'RakuAST::Initializer::Bind', 'with a bind initializer';

my $does = Q|my %h does Role = :a(1)|.AST.statements[0].expression;
is $does.^name, 'RakuAST::VarDeclaration::Simple', 'does declaration';
is $does.traits[0].^name, 'RakuAST::Trait::Does', 'with a Trait::Does';
is $does.initializer.^name, 'RakuAST::Initializer::Assign', 'and its initializer';

my @src = 1, 2;
my @b := @src;
@b.push(3);
is @src.elems, 3, 'array bind aliases';
my $y = 5;
my $z := $y;
$z = 6;
is $y, 6, 'scalar bind aliases';
my \lit := 7;
is lit, 7, 'sigilless bind';

role R { method hi { 'hi' } }
my %h does R = :a(1);
is %h.hi, 'hi', 'role mixed into a hash';
is %h<a>, 1, 'initializer kept after the mixin';
my @c does R = 1, 2;
is @c.hi ~ @c.elems, 'hi2', 'array does with initializer';
is (my $d := 3), 3, 'a bind is still an expression';

# will, loose tail and sigilless declarations are typed nodes too
my $will = Q|my $x will leave { 1 }|.AST.statements[0].expression;
is $will.traits[0].^name, 'RakuAST::Trait::Will', 'will trait';
my $tail = Q|my $t = 1 and 2|.AST.statements[0].expression;
is $tail.^name, 'RakuAST::ApplyInfix', 'loose tail makes one infix application';
my $term = Q|my \s = 5|.AST.statements[0].expression;
is $term.^name, 'RakuAST::VarDeclaration::Term', 'sigilless declaration';
my \sl = 7;
is sl, 7, 'sigilless term value';
my $r = (my $u = 4 and 9);
is $r, 9, 'loose tail value';
my $w will leave { $_ = 1 } = 2;
is $w, 2, 'will declaration still declares and assigns';
