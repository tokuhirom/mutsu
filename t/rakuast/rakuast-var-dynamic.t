use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# GH #11331: a dynamic variable read (`$*x`) is a `RakuAST::Var::Dynamic` of
# the whole spelling, not a `Var::Lexical`. Shapes measured on Rakudo 2026.09.

plan 21;

sub expr($src) { $src.AST.statements[*-1].expression }

# Read direction.
is Q|$*x|.AST.raku, q:to/END/.chomp, 'a dynamic scalar read';
RakuAST::StatementList.new(
  RakuAST::Statement::Expression.new(
    expression => RakuAST::Var::Dynamic.new(
      "\$*x"
    )
  )
)
END
is expr(Q|@*a|).^name, 'RakuAST::Var::Dynamic', 'a dynamic array';
is expr(Q|%*h|).^name, 'RakuAST::Var::Dynamic', 'a dynamic hash';
is expr(Q|&*c|).^name, 'RakuAST::Var::Dynamic', 'a dynamic code variable';
is expr(Q|$*x = 3|).left.^name, 'RakuAST::Var::Dynamic', 'the target of an assignment';
is expr(Q|$*x.say|).operand.^name, 'RakuAST::Var::Dynamic', 'the invocant of a method call';
is expr(Q|/$*x/|).body.var.^name, 'RakuAST::Var::Dynamic', 'a regex interpolation';
is expr(Q|/<@*a>/|).body.var.^name, 'RakuAST::Var::Dynamic', 'an interpolated regex array';
is expr(Q|sub f(*%) {}; f(:$*x)|).args.args[0].value.^name, 'RakuAST::Var::Dynamic',
    'a colonpair variable value';
is expr(Q|my $x; $x|).^name, 'RakuAST::Var::Lexical', 'a lexical variable stays lexical';

# The constructor and accessors.
my $hand = RakuAST::Var::Dynamic.new('$*vd');
is $hand.raku, qq:to/END/.chomp, 'a hand-built node renders like rakudo';
RakuAST::Var::Dynamic.new(
  "\\\$*vd"
)
END
is $hand.name, '$*vd', '.name is the whole spelling';
is $hand.sigil, '$', '.sigil';
is RakuAST::Var::Dynamic.new('@*vd').sigil, '@', '.sigil of an array';
ok $hand ~~ RakuAST::Var, 'Var::Dynamic is a Var';
ok $hand ~~ RakuAST::Term, 'Var::Dynamic is a Term';

# Write direction.
my $*vd = 42;
is EVAL($hand), 42, 'a hand-built Var::Dynamic reads the dynamic variable';
sub reader() { EVAL(RakuAST::Var::Dynamic.new('$*vd')) }
{
    my $*vd = 7;
    is reader(), 7, 'the lookup is dynamic, through the caller';
}
my @*vda = 1, 2, 3;
is EVAL(RakuAST::Var::Dynamic.new('@*vda')).elems, 3, 'a dynamic array lowers';
is EVAL(Q|my $*z = 5; $*z|.AST), 5, 'a dynamic read round-trips';
is EVAL(Q|my $*z = 'abc'; 'xabcx' ~~ /$*z/ ?? ~$/ !! 'no'|.AST), 'abc',
    'a regex interpolation round-trips';
