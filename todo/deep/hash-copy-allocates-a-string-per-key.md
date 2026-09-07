# Copying a Hash allocates one `String` per key

Every value-copy of a mutsu `Hash` costs one heap allocation per key, because
`HashData::map` is a `HashMap<String, Value>` and `HashMap::clone` deep-clones each
`String` key. Raku assignment semantics make hash copies routine — `%a = %b`, binding a
`%` parameter, storing into a `%` attribute — so this is a broad, structural cost, not a
local one.

## How it was found

`todo/perf/adr0019-g3-diffuse-bless-allocation-cost.md` listed `bless:named-args` (11
allocations per `bless` on `benchmarks/bench-ctor.raku`) as its next item, and guessed the
cause: "each supplied named argument does a linear `plan.class_attrs.iter().position(...)`
scan and then a `coerce_provided_attr_value_by_sigil` clone. The scan is O(attrs x args) on
a 20-attribute class; an index on the plan would make it O(args)."

That guess was wrong, and `alloc_scope!` says so exactly. Splitting the loop into
sub-scopes attributes **all 11 allocations to the `%`-sigil coercions and none to anything
else**:

```
bless:named-args:coerce-hash     10000  55000 allocs  4730000 bytes   5.5 / entry
bless:named-args:coerce-scalar   25000      0                         0.0
bless:named-args:insert          35000      0                         0.0
bless:named-args:retain          35000      0                         0.0
```

The linear scan, the `attributes.insert`, and the `deferred_defaults.retain` allocate
nothing at all. Indexing the scan would have bought zero.

`bench-ctor` supplies two `%` attributes per construction: `provides` (1 key) and
`:meta(%_)` (6 keys). `coerce_provided_attr_value_by_sigil` sends both through
`Value::detached_container_copy`, which is `Gc::new((**arc).clone())` on the `HashData`.
That is 1 table + 1 Gc box + one `String` per key:

- 1-key hash: 1 + 1 + 1 = 3
- 6-key hash: 1 + 1 + 6 = 8
- total 11 — exactly the measured figure.

Confirmed independently by scaling: a 1000-iteration loop constructing an object with one
`%` attribute costs 58,512 allocations when the hash has 1 key and 67,508 when it has 10 —
**+8,996 for 9 extra keys x 1000 constructions, i.e. exactly one allocation per key per
copy.**

## Why it is deep, not a ticket

The fix is to make the key type cheap to clone: `Arc<str>`, `Box<str>` plus interning, or
the existing `Symbol`. Any of those touches `HashData::map` and therefore every
construction, iteration, subscript and coercion site that names `HashMap<String, Value>` —
a very large surface, with knock-on questions of its own:

- **Which type.** `Symbol` is the cheapest to clone and already exists, but hash keys are
  arbitrary runtime strings, so interning every one of them grows the global symbol table
  without bound (a hash built from user input would leak). `Arc<str>` clones in O(1)
  without that risk and is the likelier answer.
- **Object hashes.** `original_keys: Option<HashMap<String, Value>>` is keyed by `.WHICH`
  strings and has the same shape and the same problem.
- **Borrowing.** Lookups today take `&str` freely via `HashMap<String, _>`'s `Borrow`
  impl. `Arc<str>` keeps that (`Borrow<str>` holds), but any code that constructs a key by
  `to_string()` and inserts needs auditing to avoid re-introducing the allocation at the
  insert site.

## What it would be worth

On `bench-ctor` it is 11 of ~1.40M allocations per 5000 constructions (a rounding error
there — the two hashes are small). The real prize is anywhere hashes are large or copied in
a loop: `%_` slurpy materialization, hash-valued parameter binds, `%h = %g`, and the
JSON/YAML/META6 workloads whose whole shape is "build a big hash, copy it".

**Measure before committing to it.** A cheap first estimate: instrument
`HashData::clone`/`detached_container_copy` with an `alloc_scope!` and run
`benchmarks/bench-hash.raku`, `bench-yaml-parse.raku` and the zef `Ecosystems` populate
path. If hash copying is not a material share there, this stays filed and unbuilt.

## Also worth noting

`benchmarks/bench-ctor.raku`'s `method new(*%_) { self.bless(|%_, :meta(%_)) }` pays the
per-key cost at least twice per construction: once when `implicit_method_named_slurpy`
builds `%_`, and again when `:meta(%_)` copies it into the attribute. That is the
benchmark's own code shape (it mirrors `Zef::Distribution`), not a bug — but it is why this
one benchmark shows the cost at all.
