# Collected `for` results retain lvalue containers

A value-collecting `for` now keeps the `ContainerRef` for a bare lvalue tail in
its returned List instead of reading the variable once when the loop ends.
Consequently, both scalar assignment and `:=` binding observe later mutations:

```raku
my $g = 1; my $s = do for 1..2 { $g }; $g = 5; say $s;  # (5 5)
```

Assigning the result to a real `@` array still snapshots the values, because the
existing array coercion decontainerizes its input elements. This resolves
[#7734](https://github.com/tokuhirom/mutsu/issues/7734) and supersedes the
rejected alternative recorded in ADR-0082; see ADR-0083 for the representation
and store-boundary reasoning.
