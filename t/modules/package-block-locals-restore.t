use Test;

# A `package`/`module` block saves and restores only the frame slots its body
# can touch (#9171). Everything an enclosing scope sees afterwards must be the
# same as when every slot was saved and restored.

plan 10;

my $x = 1;
module M1 { $x = 2 }
is $x, 2, 'a body write to an enclosing lexical survives the block';

my $y = 'outer-y';
package P1 { my $y = 'inner-y'; our sub gy { $y } }
is $y, 'outer-y', 'a body `my` does not overwrite the same-named outer lexical';
is P1::gy(), 'inner-y', 'a sub of the block still sees the body `my`';

my @a = 1, 2;
package P2 { push @a, 3 }
is-deeply @a, [1, 2, 3], 'a body mutation of an enclosing array survives';

my $z = 'top';
module M2 { our $z = 'our' }
is $z, 'top', 'a body `our` does not overwrite the outer lexical';
is $M2::z, 'our', 'the `our` variable holds its value';

my $untouched = 'u';
sub f { $untouched = 'by-sub' }
package P3 { f() }
is $untouched, 'by-sub', 'a write by a sub the body calls survives';

my $c = 0;
package P4 { my &inc = { $c++ }; inc(); inc() }
is $c, 2, 'a write by a closure the body calls survives';

my $sum = 0;
for ^3 -> $i { package P5 { $sum += $i } }
is $sum, 3, 'a package block in a loop writes the enclosing lexical each time';

my $later;
package P6 { }
$later = 'set';
is $later, 'set', 'a lexical the body never names is unaffected';
