use v6.e.PREVIEW;
use Test;
use experimental :rakuast;

plan 14;

my $plain = Q[my @a; my $delete; @a[0;1]:$delete].AST.statements[2].expression.postfix;
isa-ok $plain.colonpairs[0], RakuAST::ColonPair::Variable,
    'a bare dynamic delete adverb remains a variable colonpair';
is $plain.colonpairs[0].key, 'delete', 'the bare adverb has the delete key';

my $keyed = Q[my @a; my $delete; @a[0;1]:k:$delete].AST.statements[2].expression.postfix;
isa-ok $keyed.colonpairs[0], RakuAST::ColonPair::True,
    'the keyed multidimensional subscript retains its static adverb';
isa-ok $keyed.colonpairs[1], RakuAST::ColonPair::Variable,
    'the keyed multidimensional subscript retains dynamic deletion';

my $array = Q[my @a; my $delete; @a[0;1]:exists:$delete].AST.statements[2].expression.postfix;
isa-ok $array, RakuAST::Postcircumfix::ArrayIndex,
    'dynamic deletion remains an array postcircumfix';
is $array.index.statements.elems, 2, 'both array dimensions are retained';
isa-ok $array.colonpairs[0], RakuAST::ColonPair::True,
    'the existence adverb is a true colonpair';
is $array.colonpairs[0].key, 'exists', 'the first adverb is exists';
isa-ok $array.colonpairs[1], RakuAST::ColonPair::Variable,
    'dynamic deletion is a variable colonpair';
is $array.colonpairs[1].value.name, '$delete',
    'the dynamic adverb keeps its lexical variable';

my $hash = Q[my %h; my $delete; %h{"a";"b"}:exists:$delete].AST.statements[2].expression.postfix;
isa-ok $hash, RakuAST::Postcircumfix::HashIndex,
    'dynamic deletion on a hash retains the hash postcircumfix';
isa-ok $hash.colonpairs[1], RakuAST::ColonPair::Variable,
    'the hash form also keeps the dynamic adverb';

is $array.colonpairs[1].key, 'delete',
    'the array variable colonpair has the delete key';
is $hash.colonpairs[1].key, 'delete',
    'the hash variable colonpair has the delete key';
