use Test;

# A bind through a name computed at run time (`$::($p)::A::x := v`,
# `::($p)::A := v`) binds the last segment in the stash of the package the
# leading segments name; a leading `OUR` is the current package. These are
# roast S02-names/pseudo-6d.t's `::("OUR")` autovivify/alias tests (#18742),
# where the bind used to be dropped and its sunk lookup Failure killed the file.

plan 14;

my $our = 'OUR';

$::($our)::A42::x = 43;
is ::($our)::A42.WHO.<$x>, 43, '::("OUR") assignment autovivifies the package';
is $OUR::A42::x, 43, '... and is $OUR::A42::x';

$::($our)::A43::x := 44;
is ::($our)::A43.WHO.<$x>, 44, '::("OUR") binding autovivifies the package';
is $A43::x, 44, '... and is $A43::x at file scope';
is ::('A43').WHO.<$x>, 44, 'an autovivified package is found by its bare name';

::($our)::A44 := class { our $x = 41; };
is $::($our)::A44::x, 41, '::("OUR") can follow an aliased package';
is ::('A44').WHO.<$x>, 41, '... and the alias is the package symbol';

my $y = 5;
$::('Foo::bar') := $y;
$y = 6;
is $Foo::bar, 6, 'the bind shares the source container';

my $z = 1;
package P {
    $::('OUR')::q := $z;
    is $OUR::q, 1, 'OUR:: inside a package is that package';
}
is $P::q, 1, '... seen from outside as $P::q';

sub f3() { 'f3' }
&::('Foo')::g := &f3;
is Foo::g(), 'f3', 'a routine binds as the package symbol';

my $n = 'x';
throws-like { $::($n) := 5 }, Exception,
    message => 'This case of binding is not yet implemented',
    'an unqualified run-time name is not bindable (rakudo NYI)';
throws-like { $::('MY::x') := 5 }, Exception,
    message => 'This case of binding is not yet implemented',
    'neither is a lexical pseudo-package';

is ($::($our)::A45::x := 7), 7, 'the bind yields the bound value';
