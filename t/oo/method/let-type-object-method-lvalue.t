use Test;

# `let Type.method = v` and a bare `temp Type.method;` temporize what the rw
# call yields, like `temp Type.method = v` (#18392; Red t/023-metamodel-model
# uses `temp Bla.^table`). `let` restores only when its scope fails.

plan 17;

class A {
    my $t = "a";
    method table() is rw { $t }
}

{ let A.table = "t"; is A.table, "t", "let sets the value"; False }
is A.table, "t", "a let kept by a defined result (False) stays";
A.table = "a";

{ let A.table = "u"; Nil }
is A.table, "a", "a let whose scope yields Nil is restored";

sub dies { let A.table = "boom"; die "boom" }
try dies();
is A.table, "a", "a let whose scope throws is restored";

{
    temp A.table;
    A.table = "z";
    is A.table, "z", "assigned after a bare temp";
}
is A.table, "a", "a bare temp restores at scope exit";

{ let A.table; A.table = "y"; Nil }
is A.table, "a", "a bare let restores on failure";

{ let A.table; A.table = "kept"; True }
is A.table, "kept", "a bare let keeps on success";
A.table = "a";

{ temp A.table ~= "!"; is A.table, "a!", "temp over a compound assignment" }
is A.table, "a", "restored";

# The same forms on an instance invocant.
class B { has $.x is rw = 1 }
my $b = B.new;
{ let $b.x = 5; False }
is $b.x, 5, "let \$obj.m kept on success";
$b.x = 1;
{ let $b.x = 6; Nil }
is $b.x, 1, "let \$obj.m restored on failure";
{ temp $b.x; $b.x = 7; is $b.x, 7, "assigned after a bare temp \$obj.m" }
is $b.x, 1, "a bare temp \$obj.m restores";

# Inside a method: `self` and the `$.attr` spelling.
class C {
    has $.v is rw = "orig";
    method bare-self() { temp self.v; self.v = "in"; self.v }
    method let-attr() { let $.v = "let"; Nil }
}
my $c = C.new;
is $c.bare-self, "in", "bare temp self.m";
is $c.v, "orig", "restored when the method returns";
$c.let-attr;
is $c.v, "orig", "let \$.attr restored when the method yields Nil";
