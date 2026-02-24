use Test;

plan 4;

@*ARGS = <a b c>;
sub MAIN($a, $b, *@c);

is($a, "a", "first positional argument bound in semicolon MAIN");
is($b, "b", "second positional argument bound in semicolon MAIN");
is(~@c, "c", "slurpy positional argument bound in semicolon MAIN");
is(&?ROUTINE.name, "MAIN", "mainline after declaration runs inside MAIN");
