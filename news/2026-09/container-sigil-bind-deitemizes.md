# A container-sigil binding takes the aggregate, not an itemized holder of it

`Graph::Componentish.vertex-component` died under mutsu with `No such method 'key' for invocant of
type 'Hash'`, from a loop that is ordinary Raku:

```raku
for self.adjacency-map.kv -> $k, %v {
    for %v {
        if $_.key ∈ $vset { ... }
    }
}
```

Reduced, the bound `%v` was the wrong *kind* of thing:

```raku
my %adj = a => { b => 1, c => 2 };
for %adj.kv -> $k, %v { say %v.raku }
```

rakudo prints `{:b(1), :c(2)}`; mutsu printed `${:b(1), :c(2)}`. The leading `$` is the whole bug.

## Root cause

A real `Array`/`Hash` stores every element in a container of its own (ADR-0040), so a nested
aggregate legitimately reads back itemized — `%adj<a>.raku` really is `${:b(1), :c(2)}`, in rakudo
too. What differed is what happens when that element is *bound to a container-sigil name*. Rakudo's
binder decontainerizes: a `%`-sigil parameter is handed the `Associative` itself, never a `$`-wrapper
around one. mutsu bound the element through unchanged.

An itemized hash is one item in list context, so `for %v { }` iterated the whole hash as a single
element and `$_` was the `Hash` rather than each `Pair` — hence `.key` dying.

The `@`-sigil half was broken the same way and merely masked: the single-parameter `for` path
de-itemized nothing, and the bug only surfaced when the parameter name already resolved to a local
slot. `for %arrs.kv -> $k, @v { }; for %arrs.values -> @v { say @v.raku }` printed `$[1, 2]` for the
second loop and `[1, 2]` without the first.

The same missing decontainerization also made `my %b := $(%h)` fail outright: the implicit
`Associative` constraint on a `:=` target sees a `$`-wrapped hash as a non-Associative and rejected
it with `Type check failed in binding; expected Associative but got Hash`.

## Fix

One rule, stated once as `Value::deitemize_for_sigil_bind` and applied at every route into a
container-sigil binding:

- the positional, named and placeholder (`%^o`) parameter paths in `binding_signature.rs`, which
  covers subs, methods, pointy blocks and `is copy`;
- the single-parameter `for` binding in `vm_for_loop_body.rs`, for both sigils;
- the `%`-sigil `:=` path in `vm_var_assign_set_local.rs`, ahead of the implicit-`Associative`
  check — the multi-parameter `for` binding (`-> $k, %v`) compiles to that same `MarkBind` +
  declaration, so it is fixed by the same edit.

It strips one level and preserves identity: the itemization flag is cleared over the *same*
`HashData`/`ArrayData` `Gc`, so `%v<d> = 9` inside the loop body still reaches the caller's hash. A
`Set`/`Bag`/`Mix` bound to a `%` name has no itemization to strip and passes through untouched,
keeping its type — the `quanthash_bind_params` machinery is unaffected.

Pinned by `t/vm/binding/bind-container-sigil-deitemizes.t`, which covers all seven binding routes,
both sigils, the write-through direction, and the QuantHash/`Map` cases that must *not* change.

Closes [#9006](https://github.com/tokuhirom/mutsu/issues/9006).
