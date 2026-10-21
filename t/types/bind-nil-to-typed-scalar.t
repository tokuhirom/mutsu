use Test;

plan 8;

# `:=` stores the value as is: binding Nil to a `Mu`/`Any` scalar keeps the Nil
# (an assignment would reset it to the type object), and a narrower
# constraint refuses it.
my Mu $mu := Nil;
is $mu.raku, "Nil", "binding Nil to a Mu scalar keeps the Nil";
ok $mu === Nil, "and it is the Nil itself";

my Any $any := Nil;
is $any.raku, "Nil", "binding Nil to an Any scalar keeps the Nil";

throws-like { EVAL 'my Str $s := Nil; 1' }, X::TypeCheck::Binding, "binding Nil to a Str scalar is a type error";
throws-like { EVAL 'my Int $i := Nil; 1' }, X::TypeCheck::Binding, "binding Nil to an Int scalar is a type error";

sub gives-nil { }
my Mu $from-call := gives-nil();
is $from-call.raku, "Nil", "a routine's Nil result binds as Nil";

my Mu $assigned = Nil;
is $assigned.raku, "Mu", "assigning Nil still resets a Mu scalar to its type object";

my Mu $later;
$later := Nil;
is $later.raku, "Nil", "a later bind of Nil keeps it too";
