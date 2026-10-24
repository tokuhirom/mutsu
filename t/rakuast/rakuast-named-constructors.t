use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

# Constructors of node classes `.AST` produces, with rakudo 2026.09's
# arguments, defaults and `.raku` order, and the round trip of a parsed
# tree through its own `.raku`. This file passes under both mutsu and raku.

plan 22;

my $e = RakuAST::IntLiteral.new(1);
my $b = RakuAST::Block.new;
my $n = RakuAST::Name.from-identifier("x");
my $l = RakuAST::Label.new("L");

is RakuAST::Ternary.new(condition => $e, then => $e, else => $e).^name, 'RakuAST::Ternary', 'Ternary';
is RakuAST::FatArrow.new(key => "k", value => $e).key, 'k', 'FatArrow';
is RakuAST::Statement::For.new(source => $e, body => $b).mode, 'serial', 'For defaults to serial';
is RakuAST::Statement::For.new(source => $e, body => $b, labels => ($l,)).labels[0].name, 'L',
    'labels';
is RakuAST::Term::Reduce.new(infix => RakuAST::Infix.new("+"), args => RakuAST::ArgList.new).raku,
    Q:to/END/.chomp, 'Term::Reduce keeps triangle => False';
RakuAST::Term::Reduce.new(
  triangle => False,
  infix    => RakuAST::Infix.new("+"),
  args     => RakuAST::ArgList.new()
)
END
is RakuAST::MetaInfix::Hyper.new(infix => RakuAST::Infix.new("+"), :dwim-left).raku,
    Q:to/END/.chomp, 'a Bool flag is kept only when true';
RakuAST::MetaInfix::Hyper.new(
  dwim-left => True,
  infix     => RakuAST::Infix.new("+")
)
END
is RakuAST::Assignment.new(:item).raku, 'RakuAST::Assignment.new(:item)', 'Assignment :item';
is RakuAST::Stub::Die.new.raku, 'RakuAST::Stub::Die.new', 'a stub without arguments';
is RakuAST::Term::Self.new.raku, 'RakuAST::Term::Self.new', 'Term::Self';
is RakuAST::VarDeclaration::Term.new(scope => 'my', name => $n).raku, Q:to/END/.chomp,
RakuAST::VarDeclaration::Term.new(
  name => RakuAST::Name.from-identifier("x")
)
END
    "VarDeclaration::Term drops the default scope";
is RakuAST::Label.new("L").raku, 'RakuAST::Label.new("L")', 'Label takes its name positionally';
is RakuAST::Contextualizer::Item.new($e).^name, 'RakuAST::Contextualizer::Item', 'Contextualizer::Item';
is RakuAST::Nqp.new('say', $e).raku, Q:to/END/.chomp, 'Nqp';
RakuAST::Nqp.new(
  "say",
  RakuAST::IntLiteral.new(1)
)
END
dies-ok { RakuAST::Ternary.new(condition => $e, then => $e) }, 'a required argument is required';

# A parsed tree survives EVAL of its own .raku.
for Q[1 ?? 2 !! 3], Q[my @a; for @a { }], Q[given 1 { when 1 { } }], Q[loop { last }],
    Q[sub f { ... }], Q[subset S6 of Int where * > 0], Q[my \x = 1], Q[sub f(|c) { }] -> $src {
    my $ast = $src.AST;
    is EVAL($ast.raku).raku, $ast.raku, "round trip: $src";
}
