use Test;

# `.VAR.name` of a `:=`-bound scalar is the aliased variable's name (#16931).

plan 5;

my $x = 1;
my $c := $x;
is $x.VAR.name, '$x', 'the declared variable';
is $c.VAR.name, '$x', 'a bound alias reports the aliased variable';
my $d := $c;
is $d.VAR.name, '$x', 'a chain of binds reports the root';
sub f { my $y = 2; my $e := $y; $e.VAR.name }
is f(), '$y', 'inside a routine';
my @a = 1, 2;
my @b := @a;
is @b.VAR.name, '@a', 'an array alias keeps reporting the aliased array';
