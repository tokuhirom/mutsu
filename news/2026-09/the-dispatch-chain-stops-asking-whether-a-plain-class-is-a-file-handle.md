# The dispatch chain stops asking whether a plain class is a file handle

Every method call on every instance resolved the receiver's class name to an
owned `String` and walked its MRO looking for `IO::Handle`. For
`class C { method m() { 1 } }` declared three lines above the call, that is a
heap allocation, a string hash and an MRO walk to discover that `C` is not a
file.

A one-bit latch answers it instead, and takes **706 instructions off a 22,528-
instruction method call**.

## What was happening

`try_compiled_method_mut_or_interpret_sym` runs a chain of **32-odd `try_*`
probes** before user-method dispatch is attempted — is the receiver an
`IO::Path`, a `QuantHash`, a `Seq`, an `IO::Socket::INET`, a `Failure`, a
builtin class, an `IO::Handle`… Almost all of them are gated cheaply. The
`IO::Path` family shares one `Self::is_io_path_lexical_class(class)` test. Three
of the four native `IO::Handle` probes require `class_name == "IO::Handle"`
exactly, and the fourth short-circuits on `method == "nl-out"`.

One did not: `try_user_io_handle_method` has **no method-name gate at all**, so
it ran on every method call on every instance, and its first act was

```rust
ValueView::Instance { class_name, .. } => class_name.resolve(),
```

— `Symbol::resolve` is `as_str().to_owned()`, a heap-allocated copy of a string
that is already `&'static`. It then called `class_mro(&class_name)`, which is
keyed by `&str` (hence the allocation) and for a builtin class re-interns every
MRO element into a fresh `Arc`. Only after all of that could it reach its own
bail: `if !has_write && !has_read { return None }`.

## What it does now

`IO_HANDLE_USER_METHOD_SEEN`, a monotonic process-global latch recording
whether any user `WRITE` or `READ` method has ever been declared. The probe
returns `None` on a clear latch before touching anything.

The soundness is the same argument the `DESTROY` sweep used a few entries ago:
not a claim about the probe, but symmetry with what can make it fire.
`has_user_method` reads the reverse index that `reindex_user_method_name`
maintains, and that function is documented as "the single reverse-index hook
every `user_candidates` mutator calls" — a class body, `augment`,
`.^add_method`. So arming it there covers every way a `WRITE` or `READ` can
appear, and a clear latch is a proof that the probe would decline.

The role-declaration site arms it too. Composition into a class would pass
through the same hook anyway, so that is belt-and-braces: an over-set only makes
the correct probe run, where a missed one would lose a user handle's output.

`t/io/io-handle-user-method-latch-late-registration.t` pins the case that
matters — a `WRITE` installed through the metamodel *after* calls have already
been dispatched. That test passing is itself the verification that
`.^add_method` arms the latch: `$h.print("hello")` can only reach the user
handler through the probe, and the probe only runs if the latch is set.

## Measurements

Baseline `f8b2cde7`. `--profile profiling` builds of the same tree with and
without the change, first run after each build discarded.

| | before | after | |
| --- | ---: | ---: | ---: |
| `$o.m()` loop, 100k | 2,472,052,688 | **2,401,448,104** | **-2.86%** |
| `f()` loop, 100k | 940,491,723 | 940,397,943 | -0.01% |
| `bench_json.raku`, 100 records | 1,843,166,199 | 1,842,930,721 | -0.01% |

**706 instructions per method call**, of 22,528.

A sub call does not move: the probe requires a `ValueView::Instance` receiver.
Neither does the JSON parse, for the same reason — `JSON::Fast` calls methods on
`Str`, `Array` and `Hash`, not on user-class instances.

## Which is the point

This is the third slice in a row to say the same thing, and it is the clearest
of the three. The chain's single most expensive probe — the only one carrying an
allocation and an MRO walk — removed entirely, for **3.1% of the call**.

That is the argument for
[#8880](https://github.com/tokuhirom/mutsu/issues/8880) stated as a measurement
rather than an opinion. There is no ordering of probe removals that reaches a
method call worth calling fast, because **the chain is the design**: a
by-name speculation per receiver kind, all thirty-odd of them re-asked on every
call, every one of them returning the same answer it returned last time. A
per-call-site cache saying "a receiver of type id N resolves to *this* method"
short-circuits the whole thing at once.

So take this one for what it is: a sound, general, tiny win — it applies to
every method call on every instance in every program that declares no user
`WRITE`/`READ` — and not a step toward the 315x.
