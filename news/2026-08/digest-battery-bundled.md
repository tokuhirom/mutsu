# `Digest` is bundled — MD5, SHA-1/2/3, RIPEMD and HMAC with no install

mutsu now ships grondilu's `Digest` distribution (`zef:grondilu`, v1.1.0,
Artistic-2.0) in `modules/Digest/`, so a plain `use` works with no `zef install`
and no network:

```raku
use Digest::MD5;    md5("abc")        # 900150983cd24fb0…
use Digest::SHA1;   sha1("abc")       # a9993e364706816a…
use Digest::SHA2;   sha256("abc")     # ba7816bf8f01cfea…   (also 224/384/512)
use Digest::SHA3;   sha3_256("abc")   # 3a985da74fe225b2…   (also SHAKE)
use Digest::RIPEMD; rmd160("abc")     # 8eb208f7e05d987a…
use HMAC;           hmac(key => …, msg => …, hash => &sha1, block-size => 64)
```

Digests are table stakes for the bundle's yardstick ("a small web blog with the
shipped binary alone"): session ids, ETags, cache keys, file checksums and API
request signing all need one.

## Why this dist

A measured survey over the Zef index (14787 dist releases, 2026-08-04) put it
first on reverse dependencies among pure-Raku providers (26), with zero
dependencies of its own. The two `*::Native` alternatives are faster but compile
a C library **at install time** via `LibraryMake`, which a bundled battery
cannot do; `OpenSSL::Digest` would need the host `libssl` for something as basic
as hashing a string. Full reasoning, including why each alternative lost, is in
[docs/batteries/digest.md](../../docs/batteries/digest.md).

## What it cost to make it run

This battery is the extreme case of BATTERIES.md rung 2 — *grow the interpreter,
never patch the module*. The dist is dense idiomatic Raku over native buffers
(`blob32`/`buf64`, `Z+` over Blobs, `reduce` with a routine reference,
placeholder dispatch tables, an infinite `constant @`, `start` blocks per
compression round), and running it correctly took **five rounds of general
interpreter fixes** — well over twenty of them, each pinned by its own `t/` file
and none Digest-specific (`news/2026-08/digest-*.md`,
`news/2026-08/slurpy-single-argument-rule-and-friends.md`,
`news/2026-08/state-vars-belong-to-the-block-clone.md` and their siblings). The
last two landed with this bundling work:

- **`BEGIN` inside a module routine ran once per execution**, not once
  (`news/2026-08/begin-in-a-module-routine-runs-once.md`). `Digest::SHA2` rebuilt
  its 64-word round-constant table on every round. Fixing it took `t/sha.t` from
  97.9s to 1.5s and `t/rfc4231.t` from a timeout to 5.9s.
- **A native array stopped truncating after any thread was spawned**
  (`news/2026-08/native-array-push-after-a-start.md`) — which made a `sha1` call
  return a *wrong digest* if `rmd160` (which uses `start`) had run first.

## Gate status

3 of the 4 upstream test files are whitelisted and run on every release:
`md5.t` (2.2s), `sha.t` (1.5s), `rfc4231.t` (5.9s). `ripemd.t` produces the
correct digest for all 9 vectors but takes ~513s on its 1 MB input, over the
gate's 120s per-file budget; the throughput gap is tracked in
`todo/tickets/digest-ripemd-start-per-block-overhead.md`.

Smoke pin: `t/digest-battery.t` — one published vector per algorithm, resolved
with no `-I`.
