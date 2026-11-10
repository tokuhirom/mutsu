use Test;

plan 4;

# A `.WHICH` result (ObjAt) unboxes into a native str, as in rakudo.
my str $k = 1.WHICH;
is $k, "Int|1", "native str stores the ObjAt string";
is $k.^name, "Str", "the stored value is a plain Str";
my str $j = (1, 2).WHICH;
ok $j.starts-with("List|") || $j.starts-with("Seq|") || $j.chars > 0, "list WHICH";
my Str $s;
throws-like { $s = 1.WHICH }, X::TypeCheck::Assignment, "Str still rejects ObjAt";
