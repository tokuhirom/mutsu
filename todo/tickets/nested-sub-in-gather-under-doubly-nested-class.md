# A method's nested `sub` is unresolvable inside `gather` when the class holds a doubly-nested package

Found 2026-07-25 in File::Ignore (`TODO_dist` T-050), the last failing file of
that dist once the `<( … )>` and per-match-`:my` fixes landed.

## Repro

```raku
class O {
    class I { class C { } }                 # <-- two levels of nesting
    method w() {
        sub r() { take 7 }
        gather r()
    }
}
say O.new.w.List;
# raku:  (7)
# mutsu: ()      (and "Unknown function: recurse" in the real dist)
```

Three things each make it go away, which brackets the cause tightly:

- **one** level of nesting works: `class O { class I { }; method w() { … } }` → `(7)`;
- calling the nested sub **outside** `gather` works:
  `method w() { sub r() { 7 }; r() }` → `7`, even with `class I { class C { } }`;
- a **top-level** sub called inside `gather` works, even with a doubly-nested
  class in scope.

So it is the combination: a `sub` declared in a method body, called from inside
`gather`, in a class that contains a package declaration nested two deep.

## Impact

`File::Ignore` `t/walk.rakutest` — its `method walk` is exactly this shape:

```raku
method walk(Str() $path) {
    sub recurse($path, $prefix) { for dir($path) { … recurse($_, "$target/") … } }
    gather recurse($path, '');
}
```

and `File::Ignore` nests `class Rule { grammar Parser {…} class RuleCompiler {…} }`.
It dies with `Unknown function: recurse`. With this fixed the dist's remaining
file passes and **T-050 closes** — the other six files are already clean.

Reproducing it standalone needs no module and no `dir()`; the snippet above is
enough.

## Where to look

The nested sub is registered under the enclosing package, and `gather` runs its
body in a context whose `current_package` differs — a second level of package
nesting appears to leave the wrong package current when the gather body
resolves a by-name call. Likely files: `runtime/registry.rs` /
`registration_class_decl.rs` (how a method-body `sub` is keyed),
`vm/vm_call_func_ops.rs` (by-name resolution and its package scoping), and the
gather/take implementation's package handling.

Note the failure is silent in the reduced form (an empty gather) and only
surfaces as `Unknown function` in the dist, so fix and pin **both** shapes.
