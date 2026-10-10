use lib 't/lib';
use Test;

# Regression (found via Font::AFM's t/font-metrics-times.t): a named `is copy`
# pointy parameter on a `with` whose topic is an element of the enclosing topic
# (`for $k<R> { with .<V> -> $kk is copy { } }`) wrote the enclosing `$_` back
# into the source element on exit, replacing the data table's value with the
# whole hash.  `is copy` is a detached copy: nothing may be written back.

use WithPointyCopyKern;

plan 4;

my $o = WithPointyCopyKern::Sub.new;
is $o.kern, 1, 'kern runs';
is-deeply $o.metrics, ${:KernData(${:R(${:V(-80)}), :V(${:A(-1)})})},
    'data table untouched by an is-copy pointy with';
is $o.metrics<KernData><R><V>, -80, 'element keeps its value';

my %h = R => { V => -80 };
for %h<R> { with .<V> -> $kk is copy { $kk = 5 } }
is-deeply %h, {R => {V => -80}}, 'assigning to the copy leaves the source';
