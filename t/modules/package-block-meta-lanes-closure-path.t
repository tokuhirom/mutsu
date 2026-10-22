use Test;
use lib 't/lib';
use TypedHashLaneCaller;

# A `module` block's own `my %seen{Any}` key type stays its own when its multi
# subs are called from a unit class whose same-named `my %seen{Hash}` is live
# (found via PDF::COS::Util, #12907).
plan 2;

is TypedHashLaneCaller.go((1, 2)), 1, 'List key lands in the module block Any-keyed hash';
is TypedHashLaneCaller.go([3]), 2, 'a second key is distinct';
