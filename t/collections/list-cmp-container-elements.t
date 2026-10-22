use Test;

# `cmp` on lists whose elements sit in scalar containers compares the values,
# not their strings: RakuPM::Version's `self.key cmp $other.key` sorted 0.9.2
# above 0.13.0 (#14170). Expected values are the `raku` output.

plan 8;

my $a = 9; my $b = 13;
is ($a, 1) cmp ($b, 1), Less, 'variables in list literals compare numerically';
is ($a, 1).List cmp ($b, 1).List, Less, 'also through .List';
is (9, 1) cmp ($b, 1), Less, 'a literal against a variable';
is ($a,) cmp ($b,), Less, 'one-element lists';

class V {
    has Int $.major;
    has Int $.minor;
    method key(--> List) { ($!major, $!minor, |(1,)) }
    method cmp(V $o --> Order) { self.key cmp $o.key }
}
is V.new(:major(0), :minor(9)).cmp(V.new(:major(0), :minor(13))), Less, 'attribute-backed key lists';
is V.new(:major(2), :minor(0)).cmp(V.new(:major(10), :minor(0))), Less, 'a major version compares numerically';
is V.new(:major(1), :minor(2)).cmp(V.new(:major(1), :minor(2))), Same, 'equal keys are Same';

my @sorted = <0.9.2 0.13.0 0.10.0 0.2.0>.sort({
    my ($x, $y) = ($^p, $^q).map({ .split('.').map(+*).List });
    ($x[0], $x[1], $x[2]) cmp ($y[0], $y[1], $y[2])
});
is @sorted.join(','), '0.2.0,0.9.2,0.10.0,0.13.0', 'a sort comparator built on list cmp';
