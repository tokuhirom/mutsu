use Test;

plan 5;

# `.^mixin` of role compositions that differ only in a *value* argument are
# distinct types: renaming one leaves the other alone.
role R[$k] { }
my $a := Any.^mixin(R[1]);
my $b := Any.^mixin(R[2]);
my $c := Any.^mixin(R[1]);

is $a.^name, 'Any+{R[Int]}', 'the name shows the parameter type';
is $b.^name, 'Any+{R[Int]}', 'the second composition has the same name';
$a.^set_name("A");
is $b.^name, 'Any+{R[Int]}', 'renaming R[1] does not rename R[2]';
is $a.^name, 'A', 'the renamed composition keeps its name';
is $c.^name, 'A', 'the same value argument shares the type';
