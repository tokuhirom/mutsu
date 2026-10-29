use Test;

plan 1;

# #15441: a cyclic object with a DESTROY submethod holds a role mixin. The cycle
# collector's DESTROY snapshot keeps the mixin's overrides node alive past the
# reclaim; dropping its role cell afterwards aborted the process at exit with
# "Gc::drop strong-count underflow".
role R { has $.x is rw = 1; }
class Holder {
    has $.m;
    has $.me is rw;
    submethod DESTROY { }
}
sub make() {
    my $h = Holder.new(m => (5 but R));
    $h.me = $h;
    $h.m.x = 7;
    1
}
make() for ^20;
pass 'cyclic holder of a role mixin with DESTROY survives collection';

done-testing;
