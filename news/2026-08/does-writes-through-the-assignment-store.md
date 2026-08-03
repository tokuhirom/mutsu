# `$x does R` now uses the same by-name store an assignment does

`$x does R` rebinds `$x` to the mixed-in value — the same thing `$x = ...` does
— but it stored the result with a raw `env.insert` instead of the by-name store
every assignment goes through:

```rust
self.env_mut().insert(name.clone(), updated.clone());   // was
self.set_env_with_main_alias(&name, updated.clone());   // now
```

`set_env_with_main_alias` is not a wrapper around `insert`. It is what redirects
a **compunit's own file-scope `my`** to the compunit's cell (that name's only
home while one of its routines is running — the bare env key belongs to the
scope that *loaded* the module, `news/2026-08/module-file-scope-lexical-is-not-the-callers.md`),
logs the write into an active carrier so the carrier-return writeback reconciles
it into the caller's slot, and maintains the `Main::` / dynamic aliases.

So a `does` inside a module, on the module's own lexical, landed on a key
nothing read back:

```raku
# DoesStore.rakumod
unit module DoesStore;
role R { has $.tag = "tagged" }
my $state = {:x};
sub mixin-it() is export { $state does R }
sub read-it()  is export { (try $state.tag) // "LOST" }
```

```
raku : tagged
mutsu: LOST
```

Pin: `t/does-writes-through-the-assignment-store.t` — 2 of its 4 assertions fail
without the change, and all 4 pass under `raku`. The other two pin the ordinary
file-scope and bare-block shapes, which already worked.

## What this does *not* fix

The case that led here is still open:
`todo/tickets/does-mixin-lost-across-a-callable-block.md` — a `does` on a caller
lexical performed inside a block that a routine invokes (`lives-ok { $a does R }`)
does not reach the caller, while a plain assignment in the same position does.
Three hypotheses have now been measured and eliminated: the lexical is
deliberately not boxed into a `ContainerRef` cell for an immediately-invoked
call argument; adding `OpCode::DoesVar` to the by-name write classification in
`src/opcode.rs` changes nothing observable (measured, then reverted — the
closure's `free_var_syms`/`free_var_writes` are byte-identical between the
working assignment case and the failing `does` case); and this store change,
while correct on its own, does not fix it either. The remaining difference is
somewhere after the writeback filter in
`vm_closure_dispatch::call_compiled_closure_with_topic`.
