use v6;
use Test;
use nqp;

# nqp::defined, nqp::isconcrete, nqp::list, and nqp::unshift — the positional
# peer of the existing push_s/push_i/push_n. `nqp::unless` was already a
# compiled special form (compiler/nqp_forms.rs) once its operands stopped
# throwing on the missing ops below; see #8024, driven by Test::Async's
# HubHOW bundle registry:
#
#   nqp::unless(nqp::isconcrete($bundle-typeobjs), ($bundle-typeobjs := nqp::list()));
#   nqp::unshift($bundle-typeobjs, bundle-typeobj);

plan 12;

# -- nqp::defined / nqp::isconcrete: 0/1, false for a type object -----------

is nqp::defined(1), 1, 'nqp::defined is 1 for a concrete value';
is nqp::isconcrete(1), 1, 'nqp::isconcrete is 1 for a concrete value';
my Any $undef;
is nqp::defined($undef), 0, 'nqp::defined is 0 for an unassigned Any (a type object)';
is nqp::isconcrete($undef), 0, 'nqp::isconcrete is 0 for the same type object';
is nqp::defined(Int), 0, 'nqp::defined is 0 for a bare type object';

# -- nqp::list: an untyped VM list, readable via the existing elems/atpos ---

my $l := nqp::list(1, 2, 3);
is nqp::elems($l), 3, 'nqp::list builds a list nqp::elems can read';
is nqp::atpos($l, 1), 2, 'and nqp::atpos can index into it';

# -- nqp::unshift: insert at the front, in place -----------------------------

nqp::unshift($l, 0);
is nqp::elems($l), 4, 'nqp::unshift grows the list by one';
is nqp::atpos($l, 0), 0, 'and the new element lands at the front';
is nqp::atpos($l, 1), 1, 'shifting the old elements one slot up';

# -- the exact repro from #8024: unless + isconcrete + list + unshift -------

my $bundle;
nqp::unless(nqp::isconcrete($bundle), ($bundle := nqp::list()));
nqp::unshift($bundle, 'a');
is nqp::defined($bundle), 1, 'the #8024 idiom leaves the list defined';
is nqp::elems($bundle), 1, 'and holding the unshifted element';
