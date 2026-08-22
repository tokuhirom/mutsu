# `take-rw` doesn't preserve a mutable container alias through `gather`

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Type/Mu.rakudoc:531`).

## Repro

```raku
my @a = 1...3;
sub f(@list){ gather for @list { take-rw $_ } };
for f(@a) { $_++ };
say @a;
# OUTPUT: «[2 3 4]␤»
```

- raku: `[2 3 4]` — mutating `$_` while iterating over `f(@a)`'s gathered results writes back
  through to the original `@a` elements.
- mutsu (`target/debug/mutsu`): crashes —
  ```
  Cannot resolve caller postfix:<++>(_); the parameter requires mutable arguments
    in block <unit> at ... line 2
  ```

## Analysis

`take-rw` is documented to take its argument *by reference* (as opposed to plain `take`, which
takes a value/copy) — the gathered sequence's elements should remain live aliases to the source
container. mutsu's `take-rw` appears to behave like plain `take` (or otherwise fails to mark the
resulting sequence element as a mutable container), so a later `$_++` over the gathered result has
nothing mutable to write through, and mutsu throws instead of silently doing the wrong thing.

Note this is a different, narrower gap than the general `for @arr -> $v is rw { ... }`
element-aliasing architecture question tracked in
`todo/deep/for-loop-rw-element-alias-lost-through-deferred-closure.md` /
[ADR-0045](../../docs/adr/0045-for-loop-parameters-bind-the-element-container.md) — a plain
`for @a { $_++ }` (no `gather`/`take-rw` involved) already correctly mutates `@a` in mutsu today,
confirming the base for-loop `$_` aliasing works. This bug is specific to `take-rw` not
propagating that live container reference into the value it pushes onto the `gather` sequence.
If ADR-0045's element-`ContainerRef` work lands first, it may be worth re-checking whether it also
fixes this case for free before implementing a `take-rw`-specific patch.

## Affected files (starting point)

- Wherever `take`/`take-rw` are implemented for `gather` (likely `src/vm/vm_control_ops.rs` or
  `src/runtime/` gather/take handling) — `take-rw` needs to store a live container reference
  (`ContainerRef`) for its argument instead of a plain value snapshot.
