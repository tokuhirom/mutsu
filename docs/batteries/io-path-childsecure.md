# Battery: secure path join — `IO::Path::ChildSecure`

**Slot:** Secure path join · **Chosen:** `IO::Path::ChildSecure`
(`auth<zef:raku-community-modules>`, v1.2, Artistic-2.0) · **Kind:** Adopted
(community module, vendored as-is)

## What it is

One exported subroutine — a secure alternative to `IO::Path.child` that
fully resolves the result and guarantees it stays inside the invocant:

```raku
use IO::Path::ChildSecure;

"foo".IO.&child-secure: 'meow';   # IO::Path
"foo".IO.&child-secure: '../';    # Failure (X::IO::NotAChild)
```

Fails with `X::IO::Resolve` when the path cannot be fully resolved, and with
`X::IO::NotAChild` when the result escapes the parent — the standard guard
against `../` path-traversal when joining untrusted input. Single file,
zero dependencies.

## Why it is bundled

**It is a hard dependency of Cro::HTTP** (the static-file-serving router
uses it to stop request paths escaping the docroot). The Cro campaign
(`docs/batteries/web-framework.md`) needs every module in Cro::HTTP's
`depends` working under mutsu; this was the smallest one still missing.
It also directly serves the batteries yardstick ("a small web blog can be
written with the bundle alone") — serving a file for a URL is exactly the
path-traversal shape this guards.

**Selection.** Dictated by the Cro dependency edge (like `Crypt::Random`);
no survey was run because a substitute would not satisfy `Cro::HTTP`'s
`depends`. It is the community's canonical module for the job (originally
Zoffix Znet's, now maintained under `raku-community-modules`).

**Interpreter work it drove** (rung 2 — grow mutsu, never patch the module):

- `X::IO::Resolve` and `X::IO::NotAChild` are now registered exception
  types, constructible from user code (`X::IO::NotAChild.new: :path, :child`)
  with the rakudo message texts. Previously only mutsu-internal code could
  produce them; user-code `.new` died with `X::Method::NotFound`. Pin:
  `t/x-io-resolve-notachild.t`.

Upstream tests: 2 files, 11 subtests — all pass under mutsu, matching raku.
Smoke: `t/io-path-childsecure-battery.t`.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `IO::Path::ChildSecure` | <https://github.com/raku-community-modules/IO-Path-ChildSecure> | v1.2 | `9f01c370` (2022-04-22) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`
for attribution. Upstream `t/`, `xt/`, `logotype/`, CI config and precomp
artifacts are excluded — the release gate fetches the tests fresh at the
pinned commit.

```sh
rsync -a --exclude '.precomp' <checkout>/lib/ modules/IO-Path-ChildSecure/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/IO-Path-ChildSecure/
# then bump batteries.lock, re-run the gate, refresh the Pages manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use IO::Path::ChildSecure; say $*TMPDIR.&child-secure("x").^name'   # IO::Path
mutsu -e 'use IO::Path::ChildSecure; say $*TMPDIR.&child-secure("../x") ~~ Failure'  # True
```

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`
(Copyright 2017-2018 Zoffix Znet, 2019-2022 Raku Community). Vendored
verbatim with `LICENSE` / `META6.json` / `README` / `Changes` preserved for
attribution, source unmodified (per
[BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
