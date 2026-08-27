use Test;

plan 11;

# An `our` variable declared inside a NESTED scope is published to the package,
# so it stays readable through the package after the block has exited. The
# scalar sigil already worked; `@` and `%` resolved only through the lexical
# env, which block exit deliberately drops, so they read back empty.

{ our $s = 5 }
is $OUR::s, 5, 'our $scalar in a nested block publishes to the package';

{ our @a = 1..3 }
is-deeply @OUR::a, [1, 2, 3], 'our @array in a nested block publishes to the package';

{ our %h = x => 1 }
is-deeply %OUR::h, {x => 1}, 'our %hash in a nested block publishes to the package';

# Declare-and-then-assign, rather than declare-with-initializer.
{ our @b; @b = 4..6; }
is-deeply @OUR::b, [4, 5, 6], 'our @array assigned after its declaration publishes too';

# From inside a routine, not just a bare block.
sub decl-in-sub { our @d = 7..9 }
decl-in-sub();
is-deeply @OUR::d, [7, 8, 9], 'our @array declared inside a sub publishes';

# Still visible under its bare lexical name while the block is open.
{
    our @c = 1..3;
    is-deeply @c, [1, 2, 3], 'the bare lexical alias works inside the declaring block';
}
is-deeply @OUR::c, [1, 2, 3], 'and the package name works after the block';

# Mainline declarations are unaffected.
our @top = 1..3;
is-deeply @OUR::top, [1, 2, 3], 'a mainline our @array is unaffected';

# A real (non-pseudo) package qualifier keeps working.
package P {
    { our @p = 1..3 }
}
is-deeply @P::p, [1, 2, 3], 'a real package qualifier resolves a nested our @array';

# A nested `our` in one package must not leak into another package's namespace.
package Q {
    { our @q = 4..6 }
}
is-deeply @Q::q, [4, 5, 6], 'the second package sees its own nested our @array';
is-deeply @P::p, [1, 2, 3], 'and the first package still sees its own, unchanged';
