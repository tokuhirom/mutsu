use v6;
use experimental :rakuast;
use Test;

# Hash literals across the RakuAST boundary. Measured against rakudo 2026.09,
# the `{a => 1}` composer is a `Circumfix::HashComposer` and the `%(a => 1)`
# contextualizer a `Contextualizer::Hash` over a `StatementSequence`; both
# EVAL back to a Hash.

plan 17;

# --- the composer -----------------------------------------------------------
my $one = "\{a => 1}".AST.gist;
ok $one.contains('RakuAST::Circumfix::HashComposer.new(')
    && $one.contains('RakuAST::FatArrow.new(')
    && $one.contains('key   => "a"')
    && $one.contains('RakuAST::IntLiteral.new(1)'),
    'a single-entry composer is a HashComposer around a FatArrow';
nok $one.contains('RakuAST::Block.new('), 'a composer is not a Block';

my $two = "\{a => 1, b => 2}".AST.gist;
ok $two.contains('RakuAST::ApplyListInfix.new(')
    && $two.contains('key   => "a"')
    && $two.contains('key   => "b"'),
    'a two-entry composer holds a comma list of FatArrows';
ok $two.index('RakuAST::Circumfix::HashComposer.new(') < $two.index('RakuAST::FatArrow.new('),
    'the pairs live inside the composer';

my $str = "\{name => \"x\"}".AST.gist;
ok $str.contains('key   => "name"') && $str.contains('RakuAST::StrLiteral.new("x")'),
    'a composer entry with a string value';

is "\{}".AST.statements[0].expression.^name, 'RakuAST::Circumfix::HashComposer',
    'an empty composer is a HashComposer';
is "\{a => 1}".AST.statements[0].expression.expression.^name, 'RakuAST::FatArrow',
    'a composer exposes its contents as .expression';

# --- the contextualizer -----------------------------------------------------
my $ctx = "%(a => 1, b => 2)".AST.statements[0].expression;
is $ctx.^name, 'RakuAST::Contextualizer::Hash', '%(…) is a Contextualizer::Hash';
is $ctx.target.^name, 'RakuAST::StatementSequence', 'its target is a StatementSequence';
is $ctx.target.statements.elems, 1, 'holding one statement';
is "%()".AST.statements[0].expression.target.statements.elems, 0,
    'an empty contextualizer holds an empty StatementSequence';

# --- both EVAL back to a Hash -------------------------------------------------
my %h = "\{a => 1, b => 2}".AST.EVAL;
is-deeply %h, {a => 1, b => 2}, 'a composer EVALs to the hash';
isa-ok "\{a => .5}".AST.EVAL, Hash, 'a one-pair composer EVALs to a Hash, not a Block';
is-deeply "%(a => 1, b => 0)".AST.EVAL, %(a => 1, b => 0), 'a contextualizer EVALs to the hash';
is "%(a => 1, b => 0)".AST.EVAL.Set.keys.sort, ('a',), 'and works as a Hash';
isa-ok "%()".AST.EVAL, Hash, 'an empty contextualizer EVALs to a Hash';

# --- a nested literal keeps each spelling ------------------------------------
my $nested = "my \$x = \{a => %(b => 1)}".AST.gist;
ok $nested.index('RakuAST::Circumfix::HashComposer.new(') < $nested.index('RakuAST::Contextualizer::Hash.new('),
    'a contextualizer nested in a composer keeps both spellings';
