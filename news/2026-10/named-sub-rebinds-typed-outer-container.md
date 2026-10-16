# A sub's `:=` of an outer typed array rebinds that array

```raku
my Int @src = 1, 2, 3;
my Cool @a;
sub bind-a() { @a := @src }
bind-a();
say @a =:= @src;   # True
say @a.of;         # (Int)
```

A named sub that binds an outer `@` or `%` variable with a declared element
type now rebinds the variable itself, as it already did for an untyped one.
The typed variable used to stay out of the cell a sub's capture shares with
its declaring block, so the bind reached the block only when the sub's frame
was merged back into its caller on return (ADR-12529 §3, slice 32).
