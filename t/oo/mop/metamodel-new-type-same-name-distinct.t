use v6;
use Test;

plan 6;

my $a := Metamodel::ClassHOW.new_type(:name<Foo>);
$a.^add_method("who", method { "a" });
$a.^compose;

my $b := Metamodel::ClassHOW.new_type(:name<Foo>);
$b.^add_method("who", method { "b" });
$b.^compose;

is $a.who, "a", "first type keeps its own method";
is $b.who, "b", "second type of the same name has its own method";
ok $a.HOW !=== $b.HOW, "each new_type gets its own meta-object";
ok $a !=== $b, "the type objects are distinct";
is $a.^name, "Foo", "name is unchanged for the first type";
is $b.^name, "Foo", "name is unchanged for the second type";
