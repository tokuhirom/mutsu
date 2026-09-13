# Battery: JSON (fast path) — `JSON::Fast`

**Slot:** JSON (hand-written scanner) · **Chosen:** `JSON::Fast`
v0.20.1 (`timo/json_fast`, Artistic-2.0) · **Kind:** Adopted (community module,
vendored as-is, and **run as-is**)

## What it is

The Raku ecosystem's default JSON (de)serializer — a hand-written `nqp::`-level
scanner rather than a grammar, which is where its name comes from:

```raku
use JSON::Fast;
my $json = to-json({ :a(1), :b[1, 2] });
my $copy = from-json($json);
```

One file (~1,100 lines), zero runtime dependencies, 51 distinct `nqp::` ops.

It is the JSON module the corpus actually depends on: of the 1,625
distributions in `ecosystem/`, **541 have `JSON::Fast` in their resolved
dependency closure** against `JSON::Tiny`'s 32. Five `use JSON::Fast;`
statements across three bundled batteries (`Cro::HTTP`, `JSON::JWT`,
`Log::Timeline`) depend on it, and six bundled `META6.json` files declare it.

## `use JSON::Fast` runs the vendored module

It resolves through the ordinary precedence chain — `use lib` → `-I` →
`MUTSULIB` → the `mzef` site repo → the bundled floor at
`modules/JSON-Fast/lib` ([BATTERIES.md §6](../../BATTERIES.md)) — like any
other battery. There is no special case for the name anywhere in the parser, in
`use`-time gating, or in dispatch. Pinned by
`t/modules/batteries/json-module-ladder.t`.

Until 2026-09-13 mutsu answered the bare name from a native Rust implementation
instead ([#8226](https://github.com/tokuhirom/mutsu/issues/8226)). That provider
is deleted; what the retirement cost, and the four things the missing ops turned
out to be hiding, are in
[json-tiny.md](json-tiny.md#the-recorded-50-missing-nqp-ops-blocker-was-wrong-and-is-gone)
and `news/2026-09/`.

### What is still native, and why that is not an exception

`Rakudo::Internals::JSON.to-json` / `.from-json` is implemented in Rust
(`src/runtime/json.rs`, dispatched from `src/vm/vm_native_json.rs`). That is
**core Rakudo surface, not a module**: `raku -e 'say
Rakudo::Internals::JSON.to-json({a => 1})'` resolves with no `use`, so it is a
builtin like any other and ADR-0096 §D4's rung-3 ledger does not apply to it.
zef reaches for it on every metadata read (`vendor/zef/lib/Zef.rakumod`), as do
OpenSSL's `%?RESOURCES` loading and JSON::JWT.

## Upstream test suite

All **14** upstream files pass whole against the bundled `lib/` — 931
assertions — and are on `batteries-whitelist.txt`.

Getting there took five general interpreter fixes, none of them about JSON:
`Uni` becoming a codepoint store, `nqp::create` allocating storage, storage
objects unifying in both install orders, `nqp::strfromcodes` normalizing
(all #8226); deep recursion raising instead of aborting the process
([#8232](https://github.com/tokuhirom/mutsu/issues/8232), ADR-0100); `++$p`
keeping its native `is rw` reference
([#8233](https://github.com/tokuhirom/mutsu/issues/8233)); a brace being
classified by its contents rather than by what follows it, a conditional
preserving a native reference, and `Rational[Int,Int]` gaining a numeric surface
([#8282](https://github.com/tokuhirom/mutsu/issues/8282)).

## Re-vendoring recipe

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).

1. **Upstream + pin.** <https://github.com/timo/json_fast> — currently tag
   `0.20.1`, commit `72af212130f190443028e5a6b23827445c95559f`, Artistic-2.0.
2. **The vendor command.** From a clone at the new tag:

   ```sh
   git clone --depth 1 --branch <NEW-TAG> https://github.com/timo/json_fast.git /tmp/json_fast
   rsync -a --delete \
     --exclude 't/' --exclude 'xt/' --exclude '.github/' \
     --exclude '.precomp/' --exclude 'dist.ini' \
     /tmp/json_fast/lib /tmp/json_fast/META6.json \
     /tmp/json_fast/LICENSE /tmp/json_fast/README.md \
     modules/JSON-Fast/
   ```

   Keep `lib/`, `META6.json`, `LICENSE`, `README.md`; the upstream suite is
   fetched by the gate, never vendored. **Never hand-edit the result** — if
   mutsu needs a change to run it, that change goes in the interpreter.
3. **Provenance bump.** Update the version/commit above, the row in
   [BATTERIES.md §7](../../BATTERIES.md#7-bundle-index), and `batteries.lock`.
4. **Verification.** The smoke test:

   ```sh
   cargo build
   ./target/debug/mutsu -e 'use JSON::Fast; say from-json(to-json({ :a(1), :b[1, 2] })).raku'
   # {:a(1), :b($[1, 2])}
   ```

   Then confirm the bundled copy — not a stray `-I` one — is what answered, by
   moving `modules/JSON-Fast` aside and checking `use JSON::Fast` fails with
   "Could not find JSON::Fast".
5. **Re-baseline the gate.** Bump `commit` in `batteries.lock` to the matching
   upstream commit, re-run `scripts/battery-testsuite.sh --update`, and review
   the `batteries-whitelist.txt` diff. A file that dropped out is a regression
   to fix, not a smaller baseline to accept.

## Performance

**Measured, and it is bad.** 200 encodes and 200 decodes of a 2,380-byte
META6-shaped document — the path zef walks for every metadata read — on a
release build:

| | encode | decode |
|---|---|---|
| rakudo, running this same vendored module | 0.067s | 0.085s |
| mutsu, this module | **4.243s** | **10.709s** |
| mutsu, the native Rust codec (`Rakudo::Internals::JSON`, same binary) | 0.008s | 0.011s |

So mutsu runs `JSON::Fast` **~63x slower than rakudo does** on encode and ~126x
on decode, and **~530x / ~975x** slower than the native codec this PR stopped
answering `use JSON::Fast` with. That last column is a real, user-visible
slowdown for any program that decodes JSON, and it shipped deliberately.

ADR-0096 §D3 is what makes that the right call and not a regression to revert:
a measured gap justifies optimizing this module's own code path, never
substituting for it under its name. The substitution also had its own cost —
it emitted *unescaped, invalid JSON* for any string containing a quote or a
control character, which nobody noticed for months precisely because it was not
the real module. Tracked as [#8289](https://github.com/tokuhirom/mutsu/issues/8289).

### What the profile found

Two unrelated problems, not one. The table above is the second of them.

**Encoding one long string was quadratic in its length**, and is fixed:
`str-escape`'s per-codepoint scan called `nqp::elems` and `nqp::atpos_i`, and
both copied the whole element vector per call. An 8,000-character encode went
0.305s -> 0.047s. See
`news/2026-09/nqp-uni-element-access-was-quadratic.md`. It moves the table
above by about 2%, because a META6 document is many *short* strings — which is
exactly why it stayed invisible until someone varied the string length.

**The 63x/126x itself is call overhead, and is still open.** mutsu's plain `+`
is 2.3x *faster* than rakudo's; a named sub call is 4.1x slower and an `nqp::`
op about 3x. The VM loop is not the problem, calls are. Callgrind over three
decodes puts `format_inner` at 8.3% inclusive over 113,055 calls, every top
caller being dispatch-key construction (`find_compiled_function_inner`,
`candidate_search_packages`, `has_proto`, `resolve_proto_function`), plus
`__memcmp_avx2_movbe` at 3.8% and the malloc/free family at 15.5%. mutsu
resolves calls by building and comparing strings, and that is a dispatch
problem rather than a JSON one.

The earlier guess recorded here — that the per-character ops `nqp::ordat` /
`nqp::substr` / `nqp::strtocodes` were the bulk — was half right and half
wrong, and is kept as a caution: those ops are ~3x rakudo, which is real and
nowhere near 126x. Profile before assuming.

The one historical number worth keeping: the 2026-06 measurement that justified
the native path (200 META-shaped documents, >600s through `JSON::Tiny`'s grammar
against 0.49s native) was about a *grammar*. `JSON::Fast` is a hand-written
scanner, so it never had that cost profile — and indeed it is ~60x faster than
that grammar was, on the same shape of input.
