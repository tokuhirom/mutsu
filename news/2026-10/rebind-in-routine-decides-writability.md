# A `:=` rebind inside a routine now decides whether the variable is assignable

Binding a variable to a value makes it immutable, and binding it to another
variable makes assignments write through to that variable. mutsu got this
right for a rebind in the variable's own scope, but a rebind made inside a
routine left the outer variable with its old writability:

```raku
my $w = 1;
sub rw() { $w := 42 }
rw(); $w = 5;            # now dies: Cannot assign to an immutable value

my $y := 5; my $z = 10;
sub rz() { $y := $z }
rz(); $y = 3; say "$y $z";   # now 3 3, as in rakudo
```

The routine's readonly mark for the name was undone when it returned. The
binding cell that every holder of a captured, rebound variable shares now
records the rebind's decision, and assignments ask it first (ADR-11142 §7.3,
#9277).
