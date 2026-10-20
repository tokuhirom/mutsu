use v6;
use experimental :rakuast;
use MONKEY-SEE-NO-EVAL;
use Test;
plan 24;

my $scalar = Q[my $__t0 = 5; $__t0];
is $scalar.AST.statements[1].expression.^name, 'RakuAST::Var::Lexical',
    'a double-underscore scalar is a lexical node';
is EVAL($scalar.AST), 5, 'a double-underscore scalar evaluates';
my $array = Q[my @__x = 1, 2; @__x];
is $array.AST.statements[1].expression.^name, 'RakuAST::Var::Lexical',
    'a double-underscore array is a lexical node';
is EVAL($array.AST).elems, 2, 'a double-underscore array evaluates';
my $hash = Q[my %__x = a => 1; %__x];
is $hash.AST.statements[1].expression.^name, 'RakuAST::Var::Lexical',
    'a double-underscore hash is a lexical node';
is EVAL($hash.AST).elems, 1, 'a double-underscore hash evaluates';

for Q[sub __plain($x) { $x + 1 }; __plain(4)],
    Q[sub __object_hash($x) { $x + 1 }; __object_hash(4)],
    Q[sub __mutsu_make_format($x) { $x + 1 }; __mutsu_make_format(4)] -> $source {
    is $source.AST.statements[1].expression.^name, 'RakuAST::Call::Name',
        'a source routine name stays a call even when it names a helper';
    is EVAL($source.AST), 5, 'the source routine retains its call semantics';
}
my $bare = Q[sub __bare() { 7 }; __bare];
is $bare.AST.statements[1].expression.^name, 'RakuAST::Call::Name::WithoutParentheses',
    'a bare source routine retains its call spelling';
is EVAL($bare.AST), 7, 'a bare source routine evaluates';
is EVAL(Q[my $__x = 5; "value=$__x"].AST), 'value=5', 'a scalar name interpolates';
is EVAL(Q[my @__x = 1, 2; "@__x[]"].AST), '1 2', 'an array name interpolates';
is EVAL(Q[my %__x = a => 7; "%__x<a>"].AST), '7', 'a hash name interpolates';
is EVAL(Q[my $__c = 5; my &f = { $__c + 1 }; $__c = 10; f()].AST), 11,
    'a closure keeps a mutable double-underscore capture';
is EVAL(Q[sub f($__p) { $__p * 2 }; f(4)].AST), 8, 'a double-underscore parameter binds';
is EVAL(Q[my $__with_tmp_0 = 9; $__with_tmp_0].AST), 9,
    'a source variable may resemble a generated control temporary';
is EVAL(Q[my $__ANON_STATE_9000__ = 9; $__ANON_STATE_9000__].AST), 9,
    'a source reference may resemble an anonymous variable temporary';
is EVAL(Q[my @__x = 1, 2; @__x[0] = 4; @__x.sum].AST), 6,
    'a source array remains assignable through a subscript';
is EVAL(Q[my %__x = a => 1; %__x<a> = 4; %__x<a>].AST), 4,
    'a source hash remains assignable through a subscript';
is EVAL(Q[my &__g = { 7 }; &__g()].AST), 7, 'an explicit code variable remains callable';
