use MONKEY-SEE-NO-EVAL;
use experimental :rakuast;
use Test;

plan 5;

my $single = Q[my @a = 1, 2; @a[0]:foo].AST;
is $single.statements[*-1].expression.postfix.colonpairs[0].^name,
    'RakuAST::ColonPair::True', 'an unknown adverb remains a colonpair';
throws-like { EVAL($single) }, X::Multi::NoMatch,
    'lowering retains the element candidate error';

my $slice = Q[my @a = 1, 2; @a[0, 1]:foo].AST;
throws-like { EVAL($slice) }, X::Adverb, what => 'slice',
    'lowering retains the slice adverb error';

my $multidim = Q[my @a = [1, 2]; @a[0; 1]:foo].AST;
throws-like { EVAL($multidim) }, X::Multi::NoMatch,
    'lowering retains the multidimensional candidate error';

my $angle = Q[my %h; %h<a>:foo].AST;
is $angle.statements[*-1].expression.postfix.^name,
    'RakuAST::Postcircumfix::LiteralHashIndex',
    'an unknown adverb retains angle subscript spelling';
