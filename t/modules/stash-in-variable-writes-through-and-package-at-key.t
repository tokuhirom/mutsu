use Test;

plan 14;

# A stash held in a variable (or passed to a sub, even `is copy`) is the
# package's live symbol table: writes through it install symbols in the package.
my package tmp {};
my $s = tmp.WHO;
$s<B> := 'b';
is tmp.WHO<B>, 'b', '$stash<k> := v installs the symbol';
$s.{'C'} := 'c';
is tmp.WHO<C>, 'c', '$stash.{k} := v';
my $k = 'D';
$s{$k} := 'd';
is tmp.WHO<D>, 'd', '$stash{$var} := v';
$s<E> = 'e';
is tmp.WHO<E>, 'e', '$stash<k> = v creates the symbol';
$s.ASSIGN-KEY('F', 'f');
is tmp.WHO<F>, 'f', 'ASSIGN-KEY on a stash';
$s.BIND-KEY('A', 'a');
is tmp.WHO<A>, 'a', 'BIND-KEY on a stash';

sub bind-in($WHO is copy, $key, $value) { $WHO.{$key} := $value; Nil }
bind-in(tmp.WHO, 'G', 'g');
is tmp.WHO<G>, 'g', 'a sub binding through an `is copy` stash parameter';

# The same through a sub called at BEGIN time, for a `my package` of an EVAL.
EVAL q|my package t5 {}; BEGIN bind-in(t5.WHO, 'Foo5', 'foo'); is t5::Foo5, 'foo', 'a term bound at BEGIN time through a sub';|;

# Stash.package_at_key mints the package a key names and binds it.
my package outer {};
my $p = outer.WHO.package_at_key('Inner');
is $p.^name, 'outer::Inner', 'package_at_key mints a package named after the stash';
is outer.WHO<Inner>.^name, 'outer::Inner', 'and binds it into the stash';
ok outer.WHO.keys.contains('Inner'), 'the stash lists it';
my $w = outer.WHO.package_at_key('Inner').WHO;
$w.{'x'} := 'y';
is outer.WHO<Inner>.WHO<x>, 'y', 'a symbol bound in the minted package reads back through its stash';
is outer.WHO.package_at_key('Inner').^name, 'outer::Inner', 'a second call finds the same package';
is outer.WHO.keys.elems, 1, 'and adds no key';
