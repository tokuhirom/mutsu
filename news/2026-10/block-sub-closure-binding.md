# A closure keeps calling the routine of the block activation it came from

A `sub` declared in a loop body, a `.map` callback or a bare block is, in
Raku, a lexical `&name` binding of that block. A closure created there keeps
the routine of its own activation:

```raku
my @a; for 1, 2 -> $v { my $q = $v; sub h { $q }; @a.push(-> { h() }) }
say @a.map({ $_() });   # (1 2)
```

mutsu resolved `h()` by name through the routine registry, which the loop
body restores on exit, so the closure died with "Unknown function: h"; and a
`&g` taken inside a nested closure answered with the latest iteration's
lexicals. Now the compiler treats such a routine as the block's binding for
the closures nested in it: their calls go through an upvalue that the
creating frame fills with the code object of its own activation (ADR-12529
phase 2, #12850, #12851).
