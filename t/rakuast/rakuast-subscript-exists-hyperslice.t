use v6.e.PREVIEW;
use Test;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;

plan 17;

my $array = Q[my @a; @a[9]:exists(False)].AST.statements[1].expression.postfix;
isa-ok $array, RakuAST::Postcircumfix::ArrayIndex,
    'an array existence check remains an array postcircumfix';
isa-ok $array.colonpairs[0], RakuAST::ColonPair::Value,
    'an explicit False remains a value colonpair';
is $array.colonpairs[0].key, 'exists', 'the adverb key is retained';
isa-ok $array.colonpairs[0].value, RakuAST::Circumfix::Parentheses,
    'the explicit value keeps its parentheses';

my $hash = Q[my %h; %h<c>:exists(False)].AST.statements[1].expression.postfix;
isa-ok $hash, RakuAST::Postcircumfix::LiteralHashIndex,
    'an angle hash subscript keeps its literal form';
isa-ok $hash.colonpairs[0], RakuAST::ColonPair::Value,
    'its explicit False is also a value colonpair';
is-deeply EVAL(Q[my @a = <a>; @a[0]:exists(False)].AST), False,
    'the array check returns False after EVAL';
is-deeply EVAL(Q[my %h = a => 1; %h<a>:exists(False)].AST), False,
    'the hash check returns False after EVAL';
isa-ok Q[my @a; @a[0]:exists(0)].AST.statements[1].expression.postfix.colonpairs[0],
    RakuAST::ColonPair::Value, 'zero remains an explicit value';

my $hyper = Q[my %h; %h{**}:kv].AST.statements[1].expression.postfix;
isa-ok $hyper, RakuAST::Postcircumfix::HashIndex,
    'a hash hyperslice is a hash postcircumfix';
isa-ok $hyper.index.statements[0].expression, RakuAST::Term::HyperWhatever,
    'its index is HyperWhatever';
isa-ok $hyper.colonpairs[0], RakuAST::ColonPair::True,
    'its adverb is a true colonpair';
is $hyper.colonpairs[0].key, 'kv', 'the kv adverb is retained';
is Q[my %h; %h{**}:tree].AST.statements[1].expression.postfix.colonpairs[0].key,
    'tree', 'the tree variant is retained';
is Q[my %h; %h{**}:deepkv].AST.statements[1].expression.postfix.colonpairs[0].key,
    'deepkv', 'the deepkv variant is retained';
is EVAL(Q[my %h; %h{**}:kv].AST).elems, 0,
    'an empty kv hyperslice executes after EVAL';
is EVAL(Q[my %h; %h{**}:deepkv].AST).elems, 0,
    'an empty deepkv hyperslice executes after EVAL';
