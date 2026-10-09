use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

my $element = Q[my @a = 1, 2; @a[0]:k:v].AST;
is $element.statements[*-1].expression.postfix.colonpairs.elems, 2,
    'both conflicting adverbs remain on the postcircumfix';
is $element.statements[*-1].expression.postfix.colonpairs[0].^name,
    'RakuAST::ColonPair::True', 'the first adverb keeps its source form';
throws-like { EVAL($element) }, X::Adverb, what => 'element access',
    'element conflicts retain their descriptor';

my $zen = Q[my @a = 1, 2; @a[]:k:v].AST;
throws-like { EVAL($zen) }, X::Adverb, what => 'zen slice',
    'zen conflicts retain their descriptor';

my $hash = Q[my %h = a => 1; %h{}:k:v].AST;
throws-like { EVAL($hash) }, X::Adverb, what => 'slice',
    'hash zen conflicts retain their descriptor';
