use Test;

# A `for` loop parameter is a lexical of the loop body. Inside a package body
# (`package`/`module`/`unit class`) a write to it was package-qualified
# (`$P::v`), so an rw parameter's write-back to the source was lost. Found via
# the Math::Symbolic distribution.

plan 5;

package P {
    my @a = 1, 2;
    for @a <-> $v { $v = 9 }
    is-deeply @a, [9, 9], '<-> parameter writes back';

    for @a -> $v is rw { $v = 7 }
    is-deeply @a, [7, 7], 'is rw parameter writes back';

    my $x = 1;
    for $x <-> $v { $v = 3 }
    is $x, 3, '<-> over a scalar writes back';

    for @a -> $k, $v is rw { $v = 0 }
    is-deeply @a, [7, 0], 'multi-parameter is rw writes back';

    our $w = 'pkg';
    for 1 -> $w { }
    is $P::w, 'pkg', 'a loop parameter never writes the package variable';
}
