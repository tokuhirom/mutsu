# Vendored runtime dependencies

Unlike the top-level `roast/`, `raku-doc/`, and `old-design-docs/` trees (test
and documentation data, tracked in `vendor.lock`), the directories here are
**runtime** dependencies that mutsu ships and executes: they are bundled into
the distribution so that a freshly installed mutsu is immediately useful with no
network access.

## `zef/` — the bundled package manager (drives `mzef`)

mutsu ships the real upstream [Zef](https://github.com/ugexe/zef) as its package
manager rather than reimplementing one. The `mzef` command (`src/bin/mzef.rs`)
is a thin shim that runs this vendored tree under the mutsu interpreter:

```text
mzef install <dist>   ==>   mutsu -I vendor/zef/lib vendor/zef/bin/zef install <dist>
```

| Field      | Value                                                          |
| ---------- | -------------------------------------------------------------- |
| Upstream   | <https://github.com/ugexe/zef>                                 |
| Version    | 1.1.3                                                          |
| Commit     | `0aa54f53b55662d3a7a3b89981d34b9de97422f1` (2026-05-24)        |
| License    | **Artistic-2.0** (see `zef/LICENSE`), `auth<zef:ugexe>`        |

**License compliance:** Artistic-2.0 permits verbatim copying and redistribution
of the package. The upstream `LICENSE`, `META6.json`, and `README.md` are kept
in `zef/` unchanged to preserve attribution and the license text. No zef source
file is modified — mutsu adapts to zef, not the other way around (zef is the
project's compatibility north star, PLAN.md §1 B2).

### What is included

Only the files needed at runtime are vendored: `bin/`, `lib/`, `resources/`
(the default `config.json`), plus `LICENSE` / `META6.json` / `README.md` for
attribution. Upstream's own test suites (`t/`, `xt/`), CI config (`.github/`),
and any precompilation artifacts are **excluded**.

### Re-vendoring a newer zef

There is no `scripts/update-vendor.sh` entry for this tree (that tool is for the
whole-tree test/doc mirrors). To bump zef, `rsync` a fresh upstream checkout in
and update the table above:

```sh
rsync -a --delete \
  --exclude='.git' --exclude='.github' --exclude='t/' --exclude='xt/' \
  --exclude='*.precomp' --exclude='.precomp/' --exclude='lib/.precomp/' \
  --exclude='tmp/' --exclude='*.moarvm' --exclude='*.jar' \
  <zef-checkout>/ vendor/zef/
```

After bumping, re-run the `mzef` E2E (a real `mzef install <non-bundled-dist>`
into a fresh `$HOME`, then `use` it) and `cargo test --test mzef_shim`.
