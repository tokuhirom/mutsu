use Test;
use experimental :rakuast;

# The colonpairs of a declared variable's extended name are `Name`
# colonpairs, each spelled as written, as measured on rakudo 2026.09. This
# file passes under both mutsu and raku.

plan 14;

sub desigil(Str $src) { $src.AST.statements[0].expression.desigilname }

my $n = desigil(Q[my $today:foo<g h>]);
is $n.raku, Q:to/END/.chomp, 'a word-quoted colonpair';
RakuAST::Name.from-identifier("today", colonpairs => (
  RakuAST::ColonPair::Value.new(
    key   => "foo",
    value => RakuAST::QuotedString.new(
      processors => <words val>,
      segments   => (
        RakuAST::StrLiteral.new("g h"),
      )
    )
  ),
))
END
is $n.colonpairs.elems, 1, '.colonpairs';

is desigil(Q[my $y:bar]).colonpairs[0].^name, 'RakuAST::ColonPair::True', ':bar';
is desigil(Q[my $x:a(1)]).colonpairs[0].value.^name, 'RakuAST::Circumfix::Parentheses',
    ':a(1) keeps its parentheses';
is desigil(Q[my $x:foo«a b»]).colonpairs[0].value.processors.join(' '), 'quotewords val',
    '«a b» is a quotewords quote';
is desigil(Q[my $x:foo['a','b']]).colonpairs[0].value.^name, 'RakuAST::Circumfix::ArrayComposer',
    "['a','b'] is an array composer";
is desigil(Q[my $x:<home>]).colonpairs[0].^name, 'RakuAST::QuotedString',
    'a key-less colonpair is the quote alone';
is desigil(Q[my @z:k<v>]).colonpairs[0].key, 'k', 'an array, and the key `k`';
is desigil(Q[my $p:a<b>:c]).colonpairs.map(*.^name).join(','),
    'RakuAST::ColonPair::Value,RakuAST::ColonPair::True', 'two colonpairs';

# A use of the variable is a `Var::Lexical` of its canonical name.
is Q[my $x:foo«a»; $x:foo«a»].AST.statements[1].expression.raku,
    Q[RakuAST::Var::Lexical.new("\$x:foo<a>")], 'a use keeps the canonical name';

# EVAL lowers the colonpairs back to the canonical name the uses look up.
is EVAL(Q[my $q:foo«a b» = 42; $q:foo<a b>].AST), 42, 'EVAL: «a b» is <a b>';
is EVAL(Q[my $r:bar = 7; $r:bar].AST), 7, 'EVAL: a True colonpair';
is EVAL(Q[my $t:<home> = 3; $t:<home>].AST), 3, 'EVAL: a key-less colonpair';
is EVAL(Q[my $w:foo['a','b'] = 5; $w:foo<a b>].AST), 5, "EVAL: ['a','b'] is <a b>";
