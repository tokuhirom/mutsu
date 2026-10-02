use Test;

# A stash is a Map of symbols, not a Hash of element containers: reading an
# `@`/`%` entry through `Pkg::<...>` yields the Array/Hash itself, not an
# itemized `$[...]` / `${...}`. mutsu#10757.

plan 10;

package Q { our @a = 1, 2; our %h = a => 1; our $s = 5 }

is Q::<@a>.raku, '[1, 2]', '@ entry is the Array, not itemized';
is Q::<%h>.raku, '{:a(1)}', '% entry is the Hash, not itemized';
is Q::<$s>.raku, '5', '$ entry reads its value';
is Q::<@a>.VAR.^name, 'Array', '@ entry has no Scalar container';
is Q::<$s>.VAR.^name, 'Scalar', '$ entry is still its Scalar container';
is (Q::<@a>.map({ $_ }).elems), 2, '@ entry iterates its elements';

my $k = '@a';
is Q::{$k}.raku, '[1, 2]', 'run-time key reads the bare Array too';
is Q::.values.map(*.raku).sort.join(' '), '5 [1, 2] {:a(1)}',
    'whole-stash values are bare';

our @g = 3, 4;
is GLOBAL::<@g>.raku, '[3, 4]', 'GLOBAL:: @ entry is not itemized';

Q::<@a>.push(3);
is-deeply @Q::a, [1, 2, 3], 'the stash entry is the package variable itself';
