# `%h{1;2} = 5` is rejected as "Invalid index for multi-dim assignment"

A multi-dimensional subscript on a Hash parses but cannot be assigned to:

```raku
my %hm;
%hm{1;2} = 5;
```

- raku: works. A multi-dimensional ASSOCIATIVE subscript autovivifies nested
  hashes -- `%hm.raku` becomes `{"1" => ${"2" => 5}}` -- and reading it back
  hands out a `List`, so `%hm{1;2}.VAR.^name` is `List`.
- mutsu: `Invalid index for multi-dim assignment` at run time.

Found 2026-09-02 while extending the element-`.VAR` path to multi-dimensional
subscripts (ADR-0064); the ARRAY side (`my @sh[2;2]; @sh[0;0] = 7`) works, so
this is specific to the Hash spelling.

The error text comes from the multi-dim assignment path in
`src/vm/vm_var_assign_index_named.rs`, which treats every `{a;b}` subscript as
a store into a dimensioned (shaped) container. An Associative has no shape:
its semicolon list is a chain of nested keys and each level autovivifies a
Hash.

## Repro

```
raku  -e 'my %h; %h{1;2} = 5; say %h.raku; say %h{1;2}.VAR.^name'
mutsu -e 'my %h; %h{1;2} = 5;'
```
