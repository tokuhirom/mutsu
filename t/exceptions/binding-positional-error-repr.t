use Test;
plan 4;

sub msg(&c) { try c(); $!.message }

is msg({ my @s := <a b c>.Seq }),
    'Type check failed in binding; expected Positional but got Seq (("a", "b", "c").Seq)',
    'binding a Seq to @ names the Seq by its .raku';
is msg({ my @s := 5 }),
    'Type check failed in binding; expected Positional but got Int (5)',
    'binding an Int to @ shows the value';
is msg({ my @s := "abc" }),
    'Type check failed in binding; expected Positional but got Str ("abc")',
    'binding a Str to @ shows the value';
is msg({ my Int @z := (1,2).Seq }),
    'Type check failed in binding; expected Positional[Int] but got Seq ((1, 2).Seq)',
    'binding a Seq to a typed @ shows the value';
