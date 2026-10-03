use v6;
use experimental :rakuast;
use Test;

# Subscript adverbs across the RakuAST boundary. Measured against rakudo
# 2026.09, `@a[0]:exists` / `%h{"x"}:delete:v` / `@a[1]:kv` keep their
# adverbs as the postcircumfix's `colonpairs`, in source order, and EVAL
# back to the same read (and delete).

plan 28;

sub postfix-of(Str $code) { $code.AST.statements[*-1].expression.postfix }

# --- the adverbs are the postcircumfix's colonpairs -------------------------
my $exists = postfix-of(Q|my @a; @a[0]:exists|);
is $exists.^name, 'RakuAST::Postcircumfix::ArrayIndex', ':exists stays an ArrayIndex';
is $exists.colonpairs.map(*.^name).join(','), 'RakuAST::ColonPair::True',
    'with a ColonPair::True';
is $exists.colonpairs[0].key, 'exists', 'keyed exists';

is postfix-of(Q|my @a; @a[0]:!exists|).colonpairs[0].^name, 'RakuAST::ColonPair::False',
    ':!exists is a ColonPair::False';
is postfix-of(Q|my %h; %h{"x"}:delete|).colonpairs[0].key, 'delete', ':delete';
is postfix-of(Q|my @a; @a[1]:kv|).colonpairs[0].key, 'kv', ':kv';
is postfix-of(Q|my %h; %h{"a"}:p|).colonpairs[0].key, 'p', ':p';

my $value = postfix-of(Q|my @a; my $c; @a[0]:k($c)|).colonpairs[0];
is $value.^name, 'RakuAST::ColonPair::Value', ':k($c) is a ColonPair::Value';
is $value.key, 'k', 'keyed k';

is postfix-of(Q|my %h; %h{"a"}:exists:kv|).colonpairs.map(*.key).join(','), 'exists,kv',
    ':exists:kv keeps both';
is postfix-of(Q|my %h; %h{"a"}:p:delete|).colonpairs.map(*.key).join(','), 'p,delete',
    ':p:delete keeps both';
is postfix-of(Q|my @a; @a[0]|).colonpairs.elems, 0, 'a plain subscript has none';

# --- EVAL reads (and deletes) through them ----------------------------------
is Q|my %h = a => 1; %h<a>:exists|.AST.EVAL, True, ':exists';
is Q|my @a = 1; @a[0]:!exists|.AST.EVAL, False, ':!exists';
is Q|my @a = 1; @a[5]:exists(0)|.AST.EVAL, True, ':exists(0) inverts';
is Q|my %h = a => 1, b => 2; %h{"b"}:delete; %h.keys.sort.join|.AST.EVAL, 'a', ':delete deletes';
is Q|my @a = 10, 20; @a[1]:kv|.AST.EVAL, (1, 20), ':kv';
is Q|my %h = a => 1; %h<a>:p|.AST.EVAL, (a => 1), ':p';
is Q|my @a = 10; @a[3]:k(0)|.AST.EVAL, 3, ':k(0) keeps a missing index';
is Q|my @a = 10; my $c = 0; @a[3]:k($c)|.AST.EVAL, 3, ':k($c) takes a runtime flag';
is Q|my %h = a => 1; %h<a>:exists:kv|.AST.EVAL, ('a', True), ':exists:kv';
is Q|my %h = a => 1, b => 2; my $v = %h<a>:v:delete; ($v, %h<a>:exists)|.AST.EVAL, (1, False),
    ':v:delete reads the removed element';
is Q|my %h = a => 1, b => 2; my $v = %h<a>:delete:v; ($v, %h<a>:exists)|.AST.EVAL, (1, False),
    ':delete:v is the same';
is Q|my %h = c => 3; (%h<c>:delete(0):exists, %h<c>:exists)|.AST.EVAL, (True, True),
    ':delete(0):exists does not delete';
is Q|my %h = c => 3; my $c = 1; (%h<c>:exists:delete($c), %h<c>:exists)|.AST.EVAL,
    (True, False), ':exists:delete($c) deletes when $c holds';

# --- a node built by hand ---------------------------------------------------
my $decl = Q|my @a = 1, 2|.AST.statements[0];
my $index = RakuAST::Postcircumfix::ArrayIndex.new(
    index => RakuAST::SemiList.new(
        RakuAST::Statement::Expression.new(expression => RakuAST::IntLiteral.new(4))),
    colonpairs => (RakuAST::ColonPair::True.new('exists'),));
is RakuAST::StatementList.new(
    $decl,
    RakuAST::Statement::Expression.new(expression => RakuAST::ApplyPostfix.new(
        operand => RakuAST::Var::Lexical.new('@a'),
        postfix => $index))).EVAL, False, 'a hand-built :exists node EVALs';
is RakuAST::Postcircumfix::ArrayIndex.new(
    index => RakuAST::SemiList.new).colonpairs.elems, 0,
    'colonpairs defaults to an empty list';
is $index.colonpairs[0].key, 'exists', 'a hand-built node keeps its colonpairs';
