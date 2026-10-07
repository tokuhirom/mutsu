use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

# `(loop ...)`, `(repeat ... while/until ...)` as a term: the parentheses stay
# around the statement in `.AST` (no `do` was written), and the repeat forms
# parse as terms at all.

plan 9;

my $n = 0;
my $r = (repeat { $n++ } while $n < 3);
is $n, 3, '(repeat ... while ...) runs as a term';
my $m = 0;
my $u = (repeat { $m++ } until $m >= 3);
is $m, 3, '(repeat ... until ...) runs as a term';
is (loop { last }).elems, 0, '(loop { last }).elems is empty';

my $loop = Q|my $e = (loop { last }).elems|.AST.gist;
like $loop, /'Circumfix::Parentheses'/, 'a parenthesised loop keeps the parentheses as an operand';
like $loop, /'Statement::Loop.new'/, 'and is a Statement::Loop';
unlike $loop, /'StatementPrefix::Do'/, 'without a do prefix';

my $do = Q|my $e = (do loop { last }).elems|.AST.gist;
like $do, /'StatementPrefix::Do'/, '`do loop` keeps its do prefix';

my $rep = Q|my $k = 0; (repeat { $k++ } while $k < 4); $k|.AST;
like $rep.gist, /'Statement::Loop::RepeatWhile'/, '(repeat ... while ...) is a RepeatWhile';
is EVAL($rep), 4, 'and its AST evaluates the loop';
