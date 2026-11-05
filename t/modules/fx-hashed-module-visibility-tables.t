use v6;
use Test;

# The qualified-variable gate's package tables (`package_granting_modules`,
# `lexical_package_units`) and the per-class attribute tables are probed on
# every method call; they hash with Fx (#15688). Behaviour is unchanged.

plan 5;

class Counter {
    has Int $.n = 3;
    has @.xs = 1, 2;
    method bump { $!n++; self }
}
my $c = Counter.new;
is $c.bump.bump.n, 5, 'attribute defaults and updates survive the table change';
is $c.xs.elems, 2, 'an array attribute default is kept';

{
    my package Hidden { our $x = 11 }
    is $Hidden::x, 11, 'a block-lexical package variable reads back';
}

package Outer::Inner { our $y = 7 }
is $Outer::Inner::y, 7, 'a nested package variable reads back';

class D is Counter { has $.extra = 9 }
is D.new.extra, 9, 'a subclass attribute default resolves';
