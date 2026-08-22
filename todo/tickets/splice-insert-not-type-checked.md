# `.splice`'s inserted replacement values are never type-checked against a declared element type

`Array.splice` in mutsu inserts its replacement arguments (`args[2..]` in
`do_splice`, `src/runtime/methods_mut_dispatch.rs`) with no call to
`check_container_element_types` at all — unlike `push`/`append`/`unshift`/
`prepend`, which all call it before inserting. `splice` on a typed array
therefore silently accepts a value of the wrong type instead of dying.

## Repro

```
$ raku -e 'my Int @a = 1,2,3; @a.splice(1,0,Nil); say @a.raku;'
Type check failed in splice; expected Int but got Any (Any)
  in block <unit> at -e line 1

$ target/debug/mutsu -e 'my Int @a = 1,2,3; @a.splice(1,0,"x"); say @a.raku;'
Array[Int].new(1, x, 2, 3)   # should die: expected Int but got Str ("x")
```

## Where

`do_splice` (nested fn inside the `"splice" =>` arm,
`src/runtime/methods_mut_dispatch.rs`, around line 1029) builds `new_items`
from `args[2..]` with no type check. The sibling arms (`push` at ~:758,
`append` at ~:778, `unshift` at ~:829, `prepend` at ~:871) all call
`self.check_container_element_types(&key, &target, &values)?` before
inserting.

## Why this is separate from ADR-0049

ADR-0049 (Nil decays to the container default at the element store) fixed
`.splice`'s `Nil`-specific handling: a `Nil` replacement arg now decays to
plain `Any` (matching real raku, which — unlike push/append/unshift/prepend
— does NOT use the target's `is default(...)` value for a spliced-in `Nil`;
verified against `raku -e`). That is a narrow, Nil-only fix. This ticket is
the broader, pre-existing gap: `.splice` never type-checks ANY inserted
value (not just a decayed `Nil`/`Any`), which is a general correctness bug
independent of Nil handling.

## Fix sketch

Call `self.check_container_element_types(&key, &target, &new_items)?` (or
the equivalent for the pre-decay + pre-itemize values) in the `"splice"`
arm, mirroring the other four array-mutator arms. Needs its own repro sweep
against `t/`/roast splice tests before landing, since `do_splice` is a
free (non-method) nested fn today with no `&mut self` / `Result` return —
adding a type check will need to either move the check out into the
enclosing `"splice" =>` arm (after computing `new_items`, before draining
`items`) or thread a fallible check through the loop.
