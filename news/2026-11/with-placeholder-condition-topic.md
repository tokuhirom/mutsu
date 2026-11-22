# `with $^v { ... }` topicalizes the placeholder's value

A placeholder used as the condition of a `with`, `without` or `orwith` block
is the enclosing block's parameter, and the body's `$_` is its value:

```raku
my $c = { with $^v { $_ * 2 } };
say $c(4);   # 8, was 2
```

The `with` desugar re-reads the condition as the topic of a scaffold `given`
inside the `if` branch, and the placeholder scan counted that copy as a
placeholder of the branch block. The `if` then bound its `.defined` result
(`True`) into `$^v`. The scan now skips the scaffold `given`'s topic, since
the condition sits in the `if` header, outside the branch (#18310).
