# A `ContainerRef` holding a Hash is indistinguishable from `$`-itemization

Found 2026-09-07 while making a positional/associative slice hand out its
elements' containers
(`news/2026-09/slice-first-and-block-topic-element-containers.md`).

## The two shapes that collide

raku distinguishes them; mutsu spells both as `ContainerRef(<Hash>)`:

```raku
my %h = a => 1, b => 2;

my $hi = %h;  my %c = ($hi,);       # raku: X::Hash::Store::OddNumber -- an
                                    # itemized hash is ONE opaque element
my ($g, %rest) = f(...);            # raku: %rest gets the flattened pairs
```

The first is a `$`-scalar container holding a hash: mutsu shares the container by
reference (ADR "slice 2a") so `$hi` is a `ContainerRef` whose cell holds the raw
`Hash`. The **itemization is carried by the cell**, not by a flag inside it —
`Value::hash_is_itemized()` on the cell's contents is false.

The second reaches a hash initializer through a slice of the destructuring
staging temp (`@__destructure_tmp__[1..*]`), whose elements are now element
containers — also `ContainerRef(<Hash>)`, but here the hash must flatten.

So `build_hash_from_items_with_key_coercion` (`runtime/utils/coerce_containers.rs`,
via `map_hash_coerce::unwrap_contained_pair`) cannot decide. Unwrapping every
`ContainerRef` — which is what a value context ought to do, and what
`resolve_array_entry` does at the element read chokepoint — makes
`t/hash-itemization-flag.t` test 8 stop dying. Not unwrapping loses the
destructuring case.

## What was done instead, and why it is a stopgap

The staging temp is excluded from the element-container promotion gate
(`Interpreter::promotable_array_len`, keyed on `descriptor_name` +
`is_destructure_staging_temp` — the same exclusion ADR-0040 slice 2 already
applies to it for element itemization). That is defensible on its own terms: the
temp is not a user `Array`, it *is* the RHS list, and every destructuring target
reads a VALUE out of it.

But it only removes the one collision that was reachable. Any future producer
that hands a `Hash`-valued element container into a hash initializer hits the
same wall, and the wall is a representation gap, not a missing case.

## What the real fix looks like

Make itemization live in the value rather than in the wrapper, so a hash inside a
cell answers `hash_is_itemized()` for itself. `Value::with_hash_itemized` already
exists and `HashData` already carries the flag — what is missing is that the
scalar-container share path (`vm_var_assign_set_local.rs`'s
`MarkArrayShareSource` / `bind_source` arms) stores the *raw* hash into the cell
and relies on the read side to itemize. Once the flag is inside, every value
consumer can decontainerize unconditionally, which is the rule the rest of the
element-container work is built on.

Blast radius: every reader of `hash_is_itemized` / `is_hash_itemized`, plus the
`.raku`/`.VAR` rendering that distinguishes `${:a(1)}` from `{:a(1)}`. Worth an
ADR paragraph rather than an opportunistic change.

## Repro

```raku
my %h = a => 1, b => 2;
my $hi = %h;
my %c = ($hi,);        # must die: X::Hash::Store::OddNumber
```

Pinned today by `t/hash-itemization-flag.t` test 8 — that test is the tripwire
for anyone who tries the unconditional unwrap.
