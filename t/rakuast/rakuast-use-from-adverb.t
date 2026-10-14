use Test;

# `use Foo:from<NQP>` in RakuAST, measured on rakudo 2026.09: the `:from`
# adverb is a `ColonPair::Value` in the colonpairs of the module's `Name`
# (key `from`, value a word `QuotedString`), not an `argument` of the
# statement. EVAL of the tree keeps the NQP no-op meaning.

plan 7;

my $use = Q[use NQPHLL:from<NQP>;].AST.statements[0];
isa-ok $use, RakuAST::Statement::Use, 'the statement is a Statement::Use';
is $use.module-name.colonpairs.elems, 1, 'the module name carries one colonpair';
my $pair = $use.module-name.colonpairs[0];
isa-ok $pair, RakuAST::ColonPair::Value, '... a ColonPair::Value';
is $pair.key, 'from', '... keyed `from`';
isa-ok $pair.value, RakuAST::QuotedString, '... over the written language';
ok !$use.argument.defined, 'the statement has no argument of its own';

is Q[use NQPHLL:from<NQP>; 40 + 2].AST.EVAL, 42, 'EVAL of the tree loads nothing and keeps running';
