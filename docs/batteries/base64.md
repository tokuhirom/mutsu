# Battery: Base64 — `Base64`

**Slot:** Base64 encoding · **Chosen:** `Base64`
(`auth<github:ugexe>`, v0.1.0, Artistic-2.0) · **Kind:** Adopted (community
module, vendored as-is)

## What it is

Base64 encoding/decoding with the standard and URI-safe alphabets,
padding control, and Blob/Str input:

```raku
use Base64;

encode-base64("Hello", :str);            # "SGVsbG8="
encode-base64($blob, :uri, :str);        # URI-safe alphabet (- and _)
encode-base64("A", :str, :!pad);         # no padding
decode-base64("SGVsbG8=", :bin);         # Buf
```

Single file (~45 lines), zero dependencies, by the author of zef.

## Why it is bundled

**It is a hard dependency of Cro::HTTP** (WebSocket handshake keys and
basic-auth headers). The Cro campaign (`docs/batteries/web-framework.md`)
needs every module in Cro::HTTP's `depends` working under mutsu.

**Selection.** Dictated by the Cro dependency edge (like `Crypt::Random` and
`IO::Path::ChildSecure`): Cro depends on `Base64` — the `github:ugexe` dist
is what zef resolves for that name — so no survey was run. Note the already
bundled `MIME::Base64` covers the *MIME* flavor for `HTTP::UserAgent`; the
two dists are different APIs (`encode-base64` vs `MIME::Base64.encode-str`)
and Cro wants this one.

**Interpreter work it drove** (rung 2 — grow mutsu, never patch the module).
The module is dense idiomatic Raku (`samewith`, `|c`, `LAST` phasers,
`rotor(:partial)`, `state` in expressions), and six general fixes fell out:

- **Buf/Blob are Positional in list context** — `.rotor`/`for` over a Blob
  iterated it as ONE item instead of its bytes. Pin:
  `t/buf-positional-list-context.t`.
- **`@`/`%` parameter type constraints apply to the ELEMENTS** — the default
  in `Str:D :@alpha = @chars64std` was type-checked as a whole Array against
  `Str:D` and died. Pin: `t/typed-aggregate-param-constraint.t`.
- **Multi dispatch on a typed named aggregate param** — `:alpha(@u)` never
  matched a `Str:D :@alpha` candidate (the container's declared value type
  was compared against the smiley-carrying constraint). Same pin.
- **`(my/state $x = init) op= rhs` leaked a VM stack slot** — the compile
  emitted a superfluous `Dup`, corrupting an enclosing expression's operands
  (`65 +< ((state $m = 24) -= 8)` shifted 16 by 16). Pin:
  `t/paren-decl-compound-assign.t`.
- **A placeholder in a given/with body binds the TOPIC** — `do with EXPR
  { $^a ... }` bound `$^a` to the desugared defined-check's Bool. Pin:
  `t/given-with-placeholder-topic.t`.
- **Sub-form `grep` returns a Seq** like the method form, so a `--> Seq`
  return constraint passes. Pin: `t/grep-sub-returns-seq.t`.

Upstream tests: 2 files, 8 subtests (the big one is itself 7 subtests of
~60 assertions) — all pass under mutsu, matching raku. Smoke:
`t/base64-battery.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `Base64` | <https://github.com/ugexe/Raku-Base64> | v0.1.0 | `681a50fa` (2022-09-06) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md` for
attribution. Upstream `t/` and CI config are excluded — the release gate
fetches the tests fresh at the pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/Base64/lib/
cp <checkout>/{META6.json,LICENSE,README.md} modules/Base64/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use Base64; say encode-base64("Hello", :str)'   # SGVsbG8=
mutsu -e 'use Base64; say decode-base64("SGVsbG8=", :bin).decode'  # Hello
```

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`.
Vendored verbatim with `LICENSE` / `META6.json` / `README` preserved for
attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
