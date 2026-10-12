use v6;
use experimental :rakuast;
use Test;

plan 17;

my $single = ":\{ 0 => 42 }".AST.statements[0].expression;
is $single.^name, 'RakuAST::Circumfix::HashComposer',
    'an object hash uses the HashComposer node';
ok $single.object-hash, 'the composer retains the object-hash flag';
is $single.expression.^name, 'RakuAST::ApplyInfix',
    'a single object-hash pair is the composer expression';
is $single.EVAL{0}, 42, 'a hand-inspected object hash evaluates';
is-deeply $single.EVAL.keys.map(*.^name).list, ('Int',),
    'the object-hash key keeps its object type';

my $plain = "\{ a => 42 }".AST.statements[0].expression;
nok $plain.object-hash, 'an ordinary hash composer leaves the flag false';
is-deeply $plain.EVAL, { a => 42 }, 'an ordinary hash still EVALs as a string-keyed hash';

my $multiple = ":\{ 0 => 42, \"x\" => 7 }".AST.statements[0].expression;
is $multiple.^name, 'RakuAST::Circumfix::HashComposer',
    'a multi-pair object hash uses the HashComposer node';
ok $multiple.object-hash, 'the multi-pair composer retains its object-hash flag';
is $multiple.expression.^name, 'RakuAST::ApplyListInfix',
    'multiple pairs are kept in a comma list';
is $multiple.EVAL{0}, 42, 'the first object-hash key evaluates';
is $multiple.EVAL{'x'}, 7, 'the second object-hash key evaluates';

my $empty = ":\{\}".AST.statements[0].expression;
is $empty.^name, 'RakuAST::Circumfix::HashComposer',
    'an empty object hash uses the HashComposer node';
ok $empty.object-hash, 'an empty composer retains the object-hash flag';

my $pair = "0 => 42".AST.statements[0].expression;
my $built = RakuAST::Circumfix::HashComposer.new($pair, :object-hash);
ok $built.object-hash, 'the public constructor accepts object-hash';
is $built.EVAL{0}, 42, 'a hand-built object composer evaluates';
is-deeply $built.EVAL.keys.map(*.^name).list, ('Int',),
    'a hand-built object composer preserves typed keys';

done-testing;
