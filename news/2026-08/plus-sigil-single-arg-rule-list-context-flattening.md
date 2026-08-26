# A sigiled slurpy never aliases the caller's element containers

```raku
my @types is List = Mu, Any;
say -> *@l { @l }(@types)[0] =:= @types[0];        # raku: False
say -> +@l { @l }(@types)[0] =:= @types[0];        # raku: False, mutsu: True
say -> +l { l }(@types)[0] =:= @types[0];          # raku: True
say -> *@l is raw { @l }(@types)[0] =:= @types[0]; # raku: True
```

Only the sigilless `+l` form and an explicit `is raw` alias the caller's element
containers; a sigiled `*@l` / `+@l` rebinds each element, so `=:=` must be
`False`. mutsu reported `True` for `+@l`.

## Root cause — and why the original "conflated with `is raw`" theory was wrong

The ticket guessed that `+@l` was compiling to the same binding flag as `is raw`.
It was not. The tell was that the answer depended on **statement order**:
whichever of the four lines ran *first* reported `False`, and the ones after it
reported `True`. Both `*@l` and `+@l` flipped this way, so it was never
`+`-specific at all.

`=:=` on two index expressions compiles to `ContainerEqRaw` with
`scalar_bind_autovivify` + `bind_terminal` set, which promotes each indexed
element to a shared `ContainerRef` cell and compares cell identity. The *first*
`=:=` therefore promotes `@types[0]` into a cell **inside `@types`' own
storage**. Every later slurpy bind then copied that element out of `@types` —
`arr.to_vec()` in the one-arg branch, `out.extend(arr.iter().cloned())` in
`flatten_into_slurpy` — and what it copied was the *cell handle*. The slurpy's
element and the caller's element were then literally the same `Gc` cell, and
`=:=` correctly reported `True` about an incorrectly-shared container.

The fix is at the two slurpy-value construction sites in
`src/runtime/types/binding_signature.rs`: a sigiled `+@a` and a plain `*@a` now
read through any `ContainerRef` (`Value::into_deref`) when building their
`Array`, so each element is a fresh binding. `*@a is raw` / `is rw` — which
deliberately alias, and record that aliasing through `rw_bindings` — are left
alone, as is the sigilless `+a`, whose `List` keeps element identity by design.

The regression test asserts the four lines in both orders, so a re-appearance of
the order dependence fails rather than passing by luck.

Pinned by `t/signature-binding-gaps.t`.
