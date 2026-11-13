use Test;

# A routine that binds a captured variable to its readonly parameter
# (`$x := $param`) can be called again: the second `:=` replaces the binding
# the first one made, whatever that binding was (#17641). The binding itself
# stays readonly for assignment. Identical under rakudo.

plan 7;

my $x;
sub bind-it($param) { $x := $param }
bind-it(1);
bind-it(2);
is $x, 2, 'a second call rebinds the captured variable';
bind-it(3);
is $x, 3, 'and a third';

my Mu $lang;
sub set-lang(Mu $l) { $lang := $l }
set-lang(Int);
set-lang(Str);
is $lang.^name, 'Str', 'a Mu parameter bound to a type object, twice';

$x := 5;
is $x, 5, 'the outer scope can rebind it too';

my $y;
sub bind-y($param) { $y := $param }
bind-y(1);
throws-like { $y = 9 }, X::AdHoc, 'the binding is still readonly for assignment';

my $code = sub ($param) { $y := $param };
$code(7);
$code(8);
is $y, 8, 'an anonymous sub, twice';

my $z;
sub bind-z(Mu $param) { $z := $param }
bind-z($_) for 1..3;
is $z, 3, 'called from a loop';
