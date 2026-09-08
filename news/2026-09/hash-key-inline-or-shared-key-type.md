# A hash key type that is cheap to create and cheap to clone

`HashData::map` is a `HashMap<String, Value>` under `derive(Clone)`, so every
value-copy of a hash deep-clones each `String` key — one heap allocation per key.
[#7549](https://github.com/tokuhirom/mutsu/issues/7549) filed that, guessed
`Arc<str>` as the fix, and made a measurement the precondition for building it.
This is step 1 of that plan: the key type lands on its own, with no call site
switched over yet, so it changes no behavior.

## What the measurement said

Instrumenting `HashData`'s clone and `Value::hash_insert_through` with
`alloc_scope!` (release + `alloc-stats`) put numbers on the guess, and corrected
three of the issue's premises:

| workload | allocations in the map clone | process total | share |
| --- | ---: | ---: | ---: |
| `mzef --help` | 67 | 3,303,144 | 0.002% |
| `bench-yaml-parse` | 589 | 1,528,448 | 0.039% |
| `bench-hash` | 0 (one clone in the whole run) | 314,220 | 0% |
| `bench-ctor` | 45,000 | 1,320,732 | 3.4% |
| `%h = %g`, 200 keys x 2000 | 402,000 | 847,813 | 47.4% |

Binding a `%` parameter does **not** copy: a 200-key hash passed to `sub takes(%h)`
2000 times calls `HashData`'s clone once in total, because mutsu shares the `Gc`
and `Gc::make_mut` copies only on write. `bench-hash` likewise clones once while
inserting 10,000 keys. So hash copying is rare, and the two workloads the issue
named as its gate come in at 0.04% and 0.002%.

What is *not* rare is key creation. Every hot store site already mints a fresh
`String` per insert — `Value::hash_assign_at` does `key.to_string()`,
`EntryTerminal::insert` and the `vm_var_assign_index_named` paths do
`key.clone()` — and moves it in. That reframes the problem: `Arc<str>` is a wash
on the insert side (`Arc::from(&str)` is the same one allocation as the
`to_string()` it replaces) and only wins on the rare copy side. A key type that
stores short keys *inline* wins both, and the insert side is the half the
measured workloads actually exercise.

## The type

Ruby, Python and Perl 5 all converge on one invariant: a hash key is an
immutable, shared object, so copying a hash never copies key bytes. Perl 5 stores
keys as refcounted `HEK`s in the interpreter-global `PL_strtab`; Ruby passes
`String` keys through `rb_fstring` into a GC-reclaimable deduplicating table and
embeds strings up to 23 bytes in the `RString` itself; Python's `str` is
immutable and refcounted with its hash cached in the object, so a `dict` copy
only increfs. `String` breaks the invariant at the first clause — it is uniquely
owned, so `HashMap::clone` must copy.

`HashKey` (`src/value/hash_key.rs`) takes the two properties that need no global
state and skips the one that does: keys up to 15 bytes live inline (zero
allocation to create, zero to clone), longer keys sit behind an `Arc<str>` (one
allocation to create, an O(1) refcount bump to clone), and nothing is interned.
Deduplication is what `PL_strtab` and the fstring table add — and their
refcounting is also the answer to the issue's "interning arbitrary runtime keys
would grow the table without bound" objection, since unbounded growth is a
property of *immortal* interning rather than of interning. It stays out of scope
anyway: it buys a global table and a lock in a threaded VM for a win no
measurement has asked for.

15 bytes is not arbitrary. It puts `HashKey` at exactly `size_of::<String>()`,
which a test pins: this type is the key of every hash table in the interpreter,
so a variant that outgrew the `String` it replaces would cost more table memory
than it saves in allocations.

`Deref<Target = str>`, `Borrow<str>`, `AsRef<str>` and the `Hash`/`Eq`/`Ord`
impls all forward to `str`, so `HashMap<HashKey, _>` is looked up with a plain
`&str` exactly as `HashMap<String, _>` is. None of those three are derived, on
purpose: `Borrow<str>` obliges them to agree with `str`'s, and a disagreement
would not fail loudly — `get("k")` would silently report a present key as
missing, which in the interpreter reads as a hash that lost its contents. Two
tests pin that contract directly, alongside the inline/shared boundary (checked
against multibyte characters that straddle it), cross-representation equality,
and that cloning a long key does not copy its bytes.

## What is not done

`HashData::map` and `HashData::original_keys` are still keyed by `String`.
Switching them is step 2, and re-measuring with the same harness is step 3;
#7549 carries the plan and the threshold that decides whether they are worth
doing. Landing the type first makes that a mechanical diff and keeps it
independently reviewable.
