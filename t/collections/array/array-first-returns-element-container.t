use Test;
plan 8;

my @a = 1, 2;
is @a.first.VAR.^name, "Scalar", "argless first on an Array is the element container";
is @a.first(* > 1).VAR.^name, "Scalar", "first with a matcher is the element container";
is (1, 2).first(* > 1).VAR.^name, "Int", "first on a List answers the value";
is @a.first(* > 1, :k).VAR.^name, "Int", ":k answers the index";

my $x := @a.first(* > 1);
$x = 7;
is-deeply @a, [1, 7], "binding the result writes into the Array";

my $copy = @a.first(* > 1);
$copy = 9;
is-deeply @a, [1, 7], "assigning the result copies the value";

is @a.first(* > 100), Nil, "no match is Nil";
is (@a.first(* > 0) + 1), 2, "the container reads as its value";
