use v6;
use Test;
use MONKEY-SEE-NO-EVAL;

plan 14;

# Hyper prefix (`-« @a`), atomic variable access (`⚛$x`, `$x ⚛= v`), object
# hashes (`:{ ... }`), hyper assignment (`(LVALUES) »=» VALUE`) and an inline
# `subset` term are each lowered by one function where the parser built them;
# the executing finalization no longer converts them to RakuAST and back. Same
# behaviour in the program, an EVAL string and an EVAL tree.

my @a = 1, -2, 3;
is (-« @a).join(','), '-1,2,-3', 'hyper prefix minus';
is (!« (1, 0, 2)).join(','), 'False,True,False', 'hyper prefix not';

my atomicint $n = 5;
is ⚛$n, 5, 'atomic fetch';
$n ⚛= 9;
is ⚛$n, 9, 'atomic store';

my %o = :{ 1 => 'one', 'a' => 'A' };
is %o.elems, 2, 'object hash';
is %o{1}, 'one', 'with a non-string key';
is :{}.elems, 0, 'an empty object hash';

my ($x, $y);
($x, $y) »=» (3, 4);
is "$x $y", '3 4', 'hyper assignment over a literal list';
my @t = 0, 0, 0;
@t »=» 7;
is @t.join(','), '7,7,7', 'hyper assignment over an array';

my $s = subset :: of Int where * > 3;
ok 5 ~~ $s, 'an inline subset term accepts';
nok 2 ~~ $s, 'and rejects';

is EVAL(Q[my @v = 4, -5; (-« @v).join(',')]), '-4,5', 'EVAL string: hyper prefix';
is EVAL(Q[my %h = :{ 'k' => 'v' }; %h<k>].AST), 'v', 'EVAL tree: object hash';
is EVAL(Q[my @w = 1, 2; @w »=» 9; @w.join(',')].AST), '9,9', 'EVAL tree: hyper assignment';
