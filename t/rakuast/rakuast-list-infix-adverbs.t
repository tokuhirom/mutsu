use v6;
use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

# ADR-10723 S10: adverbs on a list-associative infix (`$z & $y :adverb`).
# Rakudo 2026.09 keeps them in `ApplyListInfix`'s private `$!adverbs`, filled
# by `add-colonpair`: the constructor form, the accessors and `visit-children`
# do not show them, but EVAL passes them to the operator.

plan 18;

my $source = Q[sub infix:<&>($a, $b, :$adverb) { $adverb ?? 'adverb' !! 'plain' }; my ($z, $y); $z & $y :adverb];
my $ast = $source.AST;
my $apply = $ast.statements[2].expression;

is $ast.statements[2].gist, q:to/END/.chomp, 'the adverbs are not part of the constructor form';
RakuAST::Statement::Expression.new(
  expression => RakuAST::ApplyListInfix.new(
    infix    => RakuAST::Infix.new("\&"),
    operands => (
      RakuAST::Var::Lexical.new("\$z"),
      RakuAST::Var::Lexical.new("\$y"),
    )
  )
)
END

isa-ok $apply, RakuAST::ApplyListInfix, 'a list-associative infix with adverbs is an ApplyListInfix';
my @children;
$apply.visit-children({ @children.push: .^name });
is-deeply @children, ['RakuAST::Infix', 'RakuAST::Var::Lexical', 'RakuAST::Var::Lexical'],
    'visit-children does not visit the adverbs';
is EVAL($source), 'adverb', 'the source passes the adverb';
is EVAL($ast), 'adverb', 'the round trip passes the adverb too';

# Three operands: the round trip calls the operator as the source does.
my $three = Q[my @calls; sub infix:<&>(*@a, :$adverb) { @calls.push: "{@a.elems}:{$adverb // '-'}"; 0 }; 1 & 2 & 3 :adverb; @calls.join(',')];
is EVAL($three.AST), EVAL($three), 'a three-operand application round-trips';

# An adverb on a two-operand ApplyInfix still renders as its colonpairs.
my $infix = Q[sub infix:<foo>($a, $b, :$x) { $x }; 1 foo 2 :x].AST;
ok $infix.statements[1].expression.gist.contains('colonpairs'), 'an ApplyInfix shows its colonpairs';
is EVAL($infix), True, 'and passes them';

# add-colonpair on hand-built nodes.
sub infix:<&>($a, $b, :$adverb) { $adverb ?? "{$a}&{$b}:adverb" !! "{$a}&{$b}" }
my $list = RakuAST::ApplyListInfix.new(
    infix    => RakuAST::Infix.new('&'),
    operands => (RakuAST::IntLiteral.new(1), RakuAST::IntLiteral.new(2)),
);
my $before = $list.gist;
is EVAL($list), '1&2', 'a hand-built ApplyListInfix without adverbs';
is-deeply $list.add-colonpair(RakuAST::ColonPair::True.new('adverb')), Nil, 'add-colonpair answers Nil';
is $list.gist, $before, 'the added adverb does not render';
is EVAL($list), '1&2:adverb', 'the added adverb reaches the operator';

# The calling scope's operator is the one an EVAL'd unit calls.
is EVAL('1 & 2'), '1&2', 'an EVAL string calls the caller\'s operator';
is EVAL('1 & 2 :adverb'), '1&2:adverb', 'and passes it the adverb';

# `:key«$var»`: an interpolating word quote as a colonpair value.
my $user = 'u v';
is-deeply EVAL(Q[sub f(*%h) { %h }; f(:person«$user »)].AST), EVAL(Q[sub f(*%h) { %h }; f(:person«$user »)]),
    'an interpolating word-quote colonpair round-trips';
my $one = 'u';
is-deeply EVAL(Q[(:p«$one »)].AST), EVAL(Q[(:p«$one »)]), 'also with a single word';

my $binary = RakuAST::ApplyInfix.new(
    left  => RakuAST::IntLiteral.new(1),
    infix => RakuAST::Infix.new('&'),
    right => RakuAST::IntLiteral.new(2),
);
$binary.add-colonpair(RakuAST::ColonPair::True.new('adverb'));
ok $binary.gist.contains('RakuAST::ColonPair::True.new("adverb")'), 'ApplyInfix.add-colonpair adds a rendered colonpair';
is $binary.colonpairs.elems, 1, 'and it is one of its colonpairs';
