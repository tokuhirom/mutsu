use Test;

# A variable name may end in `::`; rakudo reads `$pkg::` as `$pkg` itself, so
# `$pkg::.WHO` is the stash of the type held in `$pkg`. Found in
# UML::Translators, whose namespace walker does `my $pkg2 = $pkg::.WHO;`.

plan 9;

class Outer { class Inner { } }

my $pkg = Outer;
is $pkg::.WHO.keys.sort.join(','), 'Inner', '$pkg::.WHO is the stash of $pkg';
is $pkg::.^name, 'Outer', '$pkg:: is the variable $pkg';

my $n = 41;
is $n::.succ, 42, 'method call on $n::';
is $n:: + 1, 42, '$n:: in an infix expression';

my $hr = { a => 1 };
is $hr::<a>, 1, 'subscript after $hr::';

my @arr = 1, 2, 3;
is @arr::.elems, 3, '@arr:: is @arr';

my %h = b => 2;
is %h::<b>, 2, '%h:: is %h';

package P { our $x = 7 }
is $P::x::, 7, 'trailing :: after a package-qualified name';

is $::("n"), 41, 'symbolic lookup $::(...) still works';
