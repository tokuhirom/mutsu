# A closure in `my @a = ...` shares the array it declares

In Raku `my @s = INIT` stores INIT into the Array the declaration put in its
pad, so a closure INIT creates reads the very array the store fills:

```raku
sub f { my @s = 1, { @s[$++] * 2 } ... *; @s[^5] }
say f();    # (1 2 4 8 16)
```

mutsu seeded a fresh empty array before such an initializer and then replaced
it with a new one at the store. To keep the closure from holding the empty
seed, the compiler made it read `@s` by name (#15387), and that read reached
the declaring routine's slot only by walking the closure frame's callers. That
caller walk is the dynamic-scope leak ADR-12529 retires.

A plain `my @a = INIT` or `my %h = INIT` whose initializer reads the variable
now compiles as `my @a; @a = INIT`. The two forms mean the same in Raku and in
mutsu, and the reassignment stores into the existing container. The closure
therefore captures the declared array as an ordinary upvalue and sees the
store, later assignments and per-iteration redeclarations.

This is #18248's third slice (shape B). With the caller walk switched off, the
only `t/` file that still needs it is the nested frame-lexical routine of
shape D.
