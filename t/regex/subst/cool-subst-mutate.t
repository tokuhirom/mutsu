use Test;

# `Cool.subst-mutate` (6.c; deprecated in 6.d) substitutes in the invocant's
# `.Str` and stores that `Str` back when something matched; a miss leaves the
# invocant as it was (roast 6.c/APPENDICES/A03-older-specs/01-misc.t).

plan 8;

my $p = "aab".IO;
is-deeply $p.subst-mutate("a", "x", :g)».Str, ('a', 'a'), 'IO::Path: :g returns the matches';
is-deeply $p, 'xxb', 'and the variable now holds the Str';

my $n = 123;
is ~$n.subst-mutate(/2/, 'Z'), '2', 'Int: the match';
is-deeply $n, '1Z3', 'and the Str';

my $m = 123;
is-deeply $m.subst-mutate(/9/, 'Z'), Nil, 'a miss returns Nil';
is-deeply $m, 123, 'and leaves the Int';

my $q = 'ab'.IO;
$q.subst-mutate('q', 'Z');
isa-ok $q, IO::Path, 'a miss leaves the IO::Path';

my $r = 123;
$r.substr-rw(0, 1) = '9';
is-deeply $r, '923', 'Cool.substr-rw on an Int';
