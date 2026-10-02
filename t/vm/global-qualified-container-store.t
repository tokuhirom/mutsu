use Test;

plan 9;

# A whole-variable write through the pseudo-package-qualified name of a
# top-level `our @d` / `our %e` (`@GLOBAL::d = ...`, `%GLOBAL::e = ...`) and a
# mutating method call through it (`@GLOBAL::d.push`) must reach the very
# container the bare name holds, not a second container stored under the
# qualified spelling (#11031).

our @d;
@GLOBAL::d = 1, 2;
is @GLOBAL::d.raku, '[1, 2]', 'the qualified name reads back the assignment';
is @d.raku, '[1, 2]', 'the bare name sees a whole-array assignment';
@GLOBAL::d.push(3);
is @d.raku, '[1, 2, 3]', 'the bare name sees a .push through the qualified name';

our %e;
%GLOBAL::e = a => 1;
is %e.raku, '{:a(1)}', 'the bare name sees a whole-hash assignment';

sub assign-from-sub { @GLOBAL::d = 9 }
assign-from-sub();
is @d.raku, '[9]', 'an assignment from inside a sub reaches the bare name';

my @copy = @d;
@GLOBAL::d = 7;
is @copy.raku, '[9]', 'a copy taken before is not aliased';
is @d.raku, '[7]', 'the bare name holds the new contents';

# A slot no `our` declared is still rakudo's auto-created Scalar (#11000).
@GLOBAL::u = 1, 2;
is @GLOBAL::u.raku, '$(1, 2)', 'an undeclared qualified slot item-assigns';

# A package's own `our` keeps working through its qualified name.
package P { our @x }
@P::x = 1, 2;
is @P::x.raku, '[1, 2]', 'a package-qualified our array assigns';
