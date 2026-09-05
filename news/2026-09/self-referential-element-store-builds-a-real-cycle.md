# A self-referential element store builds a real cycle instead of leaking a sentinel

`%h<k> = %h` and `@a[i] = @a` now store the container *itself*, the way rakudo
does, so the structure is genuinely circular and the cycle-aware renderers print
a back-reference. Previously the element store recognised the self-assignment and
wrote an internal marker pair in its place — `__mutsu_self_hash_ref => True` /
`__mutsu_self_array_ref => True` — which resolved correctly on *read* but was
invisible to every renderer, so it leaked verbatim into user-visible output:

```
my %h; %h<a> = 1; %h<self> = %h;
say %h.gist;   # was: {a => 1, self => {a => 1, self => __mutsu_self_hash_ref => True}}
               # now: (\Hash_1 = {a => 1, self => Hash_1})   -- rakudo's rendering
say %h.raku;   # was: {:a(1), :self({:a(1), :self(:__mutsu_self_hash_ref)})}
               # now: ((my %Hash_1) = {:a(1), :self(%Hash_1)})

my @a; @a[0] = 1; @a[1] = @a;
say @a.gist;   # was: [1 __mutsu_self_array_ref => True]
say @a.raku;   # was: [1, [1]]     -- a *snapshot copy*, not a back-reference
               # now: ((my @Array_1) = [1, @Array_1])
```

This was a store-side bug, not a rendering one. The `.gist` / `.raku` cycle rule
itself was already correct — a cycle that reaches a hash through an array
(`@m = 1, %n; %n<x> = @m`) rendered exactly as rakudo does, because that node is
genuinely shared. The direct spelling simply never built a cycle for the
renderer to find.

## What changed

The two element-store arms in `vm_var_assign_index_named.rs` insert `val` (the
container) directly, bypassing `itemize_value` / `hash_insert_through`, which
would copy the node and break the cycle. With the marker gone, the whole
sentinel apparatus goes with it: `self_hash_ref_marker` / `self_array_ref_marker`,
the `SELF_*_REF_SENTINEL` constants, the resolution arms in
`resolve_hash_entry` / `resolve_array_entry` / `resolve_hash_for_iteration`,
`hash_has_sentinels` and its four call sites, and `is_self_array_ref_marker` plus
its three uses in the `.raku` renderer. Three of the `hash_has_sentinels` call
sites only ever fired for the self-ref marker, so deleting them is exactly
behaviour-preserving; the fourth kept its independent `ContainerRef` check.

Two renderer bugs the new (real) cycles exposed were fixed alongside:

- The `.raku` cycle node was named `%hash_<addr>`; rakudo names it after the
  type, `%Hash_<addr>`, matching the `@Array_<addr>` the array path already
  used. An itemized hash value whose repr is a back-reference is now printed
  bare (`:self(%Hash_1)`) rather than wrapped (`:self($(%Hash_1))`) — the same
  guard `itemize_scalar_repr` already applied.
- `.raku` of a *typed* hash goes through the interpreter path
  (`dispatch_constrained_hash_raku`), because rendering its values needs method
  dispatch. That walk had no cycle detection at all, so
  `my %oh{Any}; %oh<a> = %oh; %oh.raku` — the third `lives-ok` of the whitelisted
  `roast/S09-hashes/objecthash.t` — recursed until the stack died once the store
  produced a real cycle. Both walks now share one thread-local "currently
  rendering" stack (`hash_cycle_enter` / `hash_cycle_exit` in `raku_repr`), so a
  cycle that crosses between them terminates, and the typed form gets the same
  `((my %Hash_1) = (my Any %{Any} = :a(%Hash_1)))` preamble rakudo prints.

## Pin

`t/self-referential-element-store.t` (18 assertions) covers both spellings: the
rendering, the absence of any `__mutsu` marker in it, that the stored value is
`===` the container, that a later write is visible through the self-reference
(the proof it aliased rather than snapshotted), the lone-element trailing comma
(`[@Array_1,]`), and that copying a circular hash leaves the cycle on the
original node. The whole file passes under real `raku` as well as under mutsu.

## Known remaining gap

`.Str` of a circular container is still wrong, and was before this change:
`to_string_value` renders a looping hash as `{...}` where rakudo prints the
`(\Hash_1 = …)` back-reference, and a looping *array* overflows the stack. That
one is not a regression — `@c = 42, @c` (which already built a real cycle) shows
it on `main` — and rakudo itself hangs on the array case, so there is no
reference output to match. Left as-is.
