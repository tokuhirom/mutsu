use experimental :rakuast;
use Test;

# `Name.canonicalize` of an extended name keeps its colonpairs, as rakudo
# canonicalizes them, so `.DEPARSE` of `my $three:foo = 3` keeps the `:foo`
# (#14880, the `.DEPARSE` corpus's S03-operators/ternary.t).

plan 8;

sub desigil(Str $src) { $src.AST.statements[0].expression.desigilname }

is desigil(Q[my $three:foo = 3]).canonicalize, 'three:foo', 'a key-only colonpair';
is desigil(Q[my $today:foo<g h>]).canonicalize, 'today:foo<g h>', 'a word-quoted value';
is desigil(Q[my @a:k<v>]).canonicalize, 'a:k<v>', 'on an array';
is RakuAST::Name.from-identifier('x', colonpairs => (RakuAST::ColonPair::True.new('k'),)).canonicalize,
    'x:k', 'a constructed name';
is RakuAST::Name.from-identifier('x').canonicalize, 'x', 'a name without colonpairs';

is Q[my $three:foo = 3].AST.statements[0].DEPARSE.chomp, 'my $three:foo = 3', '.DEPARSE of the declaration';
is Q[my $thing:foo = 1 ?? 2 !! 3].AST.statements[0].DEPARSE.chomp, 'my $thing:foo = 1 ?? 2 !! 3',
    'with a ternary initializer';

my $three:foo = 3;
is $three:foo, 3, 'the variable is declared and read by its extended name';
