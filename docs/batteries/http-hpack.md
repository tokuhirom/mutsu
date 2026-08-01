# Battery: HTTP/2 header compression — `HTTP::HPACK`

**Slot:** HTTP/2 header compression (HPACK) · **Chosen:** `HTTP::HPACK`
(`auth<zef:raku-community-modules>`, v1.0.3, Artistic-2.0) · **Kind:** Adopted
(community module, vendored as-is)

## What it is

An implementation of RFC 7541 HPACK — the header compression format used by
HTTP/2 — with encoder/decoder classes, the static and dynamic tables, and
Huffman coding:

```raku
use HTTP::HPACK;

my @headers = HTTP::HPACK::Decoder.new.decode-headers($blob);
my $blob = HTTP::HPACK::Encoder.new(:huffman).encode-headers(@headers);
```

Single file (~480 lines), zero dependencies. Originally by Jonathan
Worthington (Edument), now maintained under `raku-community-modules`.

## Why it is bundled

**It is a hard dependency of Cro::HTTP** (its HTTP/2 support). The Cro
campaign (`docs/batteries/web-framework.md`) needs every module in
Cro::HTTP's `depends` working under mutsu.

**Selection.** Dictated by the Cro dependency edge, like the other Cro-dep
batteries (`Crypt::Random`, `IO::Path::ChildSecure`, `Base64`); it is also
the only HPACK implementation in the ecosystem.

**Interpreter work it drove** (rung 2 — grow mutsu, never patch the module):

- **`xx` thunks its left side** — the expression is re-evaluated for every
  repetition, where mutsu evaluated once and repeated the value. The old
  behavior was gated on a whitelist of "known side-effecting calls"
  (`rand`, `.push`, …), exactly the incomplete static analysis CLAUDE.md
  warns about — HPACK's `decode-str($packed, $idx) xx 2` (read a header's
  name, then its value, advancing the rw offset) is a plain user sub call
  the whitelist could never enumerate. Now any non-pure-value lhs
  re-evaluates; a small literal count unrolls inline (which also sidesteps
  the closure rw-writeback gap filed as
  `todo/tickets/closure-rw-arg-writeback.md`). Pin: `t/xx-thunk-reeval.t`.

Upstream tests: 2 files, 57 subtests (the full RFC 7541 appendix-C example
suite, Huffman coding and dynamic-table eviction included) — all pass under
mutsu, matching raku. Smoke: `t/http-hpack-battery.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `HTTP::HPACK` | <https://github.com/jnthn/p6-http-hpack> | v1.0.3 | `c52e0065` (2025-06-03) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`
for attribution. Upstream `t/`, `xt/`, `doc/`, CI config and precomp
artifacts are excluded — the release gate fetches the tests fresh at the
pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/HTTP-HPACK/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/HTTP-HPACK/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use HTTP::HPACK; say HTTP::HPACK::Decoder.new.decode-headers(Buf.new(0x82))[0].name'  # :method
```

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`.
Vendored verbatim with `LICENSE` / `META6.json` / `README` / `Changes`
preserved for attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
