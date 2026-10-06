use v6;
use Test;

# `my $l := (1, 2, 3)` binds the List straight to the name: there is no Scalar
# container, so `my @a = $l` flattens it. Unlike `for $l`, which is decided when
# the loop is compiled, `@a = $l` is decided at run time from a
# `__mutsu_bound_decont::<name>` marker in the env, behind a sticky
# "some marker exists" gate. A routine call used to clear that gate along with
# the one-shot mark flags, so inside any routine the List stayed one item.

plan 14;

my $list := (1, 2, 3);
my $arr  := [4, 5];
my $seq  := (1, 2, 3, 4).map({ $_ });
my $item = (7, 8, 9);

sub from-list { my @a = $list; @a.elems }
sub from-arr  { my @a = $arr;  @a.elems }
sub from-seq  { my @a = $seq;  @a.elems }
sub from-item { my @a = $item; @a.elems }

is from-list(), 3, 'a sub flattens the List its captured scalar is bound to';
is from-arr(),  2, 'a sub flattens the Array its captured scalar is bound to';
is from-seq(),  4, 'a sub flattens the Seq its captured scalar is bound to';
is from-item(), 1, 'an ordinary `=` scalar holding a List stays one item in a sub';

my @b = $list;
is @b.elems, 3, 'the declaring scope flattens the bound List (unchanged)';

my &lam = -> { my @a = $list; @a.elems };
is lam(), 3, 'an anonymous sub flattens the bound List';

sub nested { inner-read() }
sub inner-read { my @a = $list; @a.elems }
is nested(), 3, 'a routine called from a routine still sees the binding';

sub assign-later { my @a; @a = $list; @a.elems }
is assign-later(), 3, 'a plain `@a = $l` (not a declaration) flattens too';

class C { method m { my @a = $list; @a.elems } }
is C.new.m, 3, 'a method flattens the bound List';

# A declaration in the routine shadows the captured binding.
sub shadow-my { my $list = (1, 2, 3); my @a = $list; @a.elems }
is shadow-my(), 1, 'a sub-local `my $list = ...` is a Scalar item again';

sub shadow-param($list) { my @a = $list; @a.elems }
is shadow-param((1, 2, 3)), 1, 'a parameter of the same name is a Scalar item';

# A `:=` bind to an itemized value keeps the item container, in and out of a sub.
sub item-list { $(1, 2, 3) }
my $bound-item := item-list();
sub from-bound-item { my @a = $bound-item; @a.elems }
is from-bound-item(), 1, 'a captured scalar bound to an itemized List stays one item';

# A bind made inside a routine is visible to the routine's own `@a = $x`, and a
# routine's marker does not disturb a later routine reading a different one.
sub rebinder { my $x := (4, 5); my @a = $x; @a.elems }
my $late := (7, 8, 9, 10);
sub reads-late { my @a = $late; @a.elems }
is rebinder(), 2, 'a bind made inside a routine flattens in that routine';
is reads-late(), 4, 'a later routine still flattens its own captured binding';
