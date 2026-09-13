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

Unmeasured against the deleted native implementation, deliberately. ADR-0096 §D3
already settles what a gap would and would not license: a measured shortfall
justifies optimizing this module's own code path, never substituting for it
under its name. If `JSON::Fast` turns out slow under mutsu, that is a
`todo:perf` finding about the interpreter, not a reason to reinstate a provider.

The one number worth recording is the shape of the old argument: the 2026-06
measurement that justified the native path (200 META-shaped documents, >600s
through `JSON::Tiny`'s grammar against 0.49s native) was about a *grammar*.
`JSON::Fast` is a hand-written scanner, so it never had that cost profile.
