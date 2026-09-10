use Test;
plan 3;

my Str $a = "hello";
is $a, "hello", "Str type annotation";

my Int $b = 42;
is $b, 42, "Int type annotation";

my Bool $c = True;
is $c, True, "Bool type annotation";
