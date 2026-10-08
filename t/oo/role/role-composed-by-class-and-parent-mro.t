use Test;

# From Graphviz::DOT::Grammar: `class R does C is M` where `class M does C`
# must not die with "Inconsistent class hierarchy".
plan 5;

role C { method c { 'c' } }
class M does C { method m { 'm' } }
class R does C is M { }

is R.new.c, 'c', 'role method reachable';
is R.new.m, 'm', 'parent method reachable';
is R.^mro.map(*.^name).join(','), 'R,M,Any,Mu', 'MRO is R, M, Any, Mu';
ok R.new ~~ C, 'R does C';
ok R.new ~~ M, 'R is M';
