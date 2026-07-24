# Battery: the HTTP client's dependency layer — `URI`, `MIME::Base64`, `HTTP::Status`, `DateTime::Parse`, `File::Directory::Tree`

**Slot:** HTTP client dependency layer · **Kind:** Adopted (community modules,
vendored as-is) · **Licenses:** Artistic-2.0 / Artistic-2.0 / Artistic-2.0 / MIT /
Artistic-2.0

These are the libraries the chosen HTTP client, `HTTP::UserAgent`
([http-client.md](http-client.md)), depends on at runtime. They are bundled as a
layer because none of them is useful to sequence separately: `HTTP::UserAgent`
cannot ship until all of them do, and each is independently useful to a program
that ships with mutsu (URL parsing, base64, status-code names, HTTP date
parsing, recursive directory removal).

## Status: working (every upstream test passes)

All five resolve with **zero config** (`use URI;` — no `-I`, no install) and
their **complete upstream test suites pass against the bundled copy**:

| Module | Upstream suite | Result |
| --- | --- | --- |
| `URI` | 14 files | 14/14 |
| `MIME::Base64` | 4 files | 4/4 |
| `HTTP::Status` | 3 files | 3/3 |
| `DateTime::Parse` | 3 files | 3/3 |
| `File::Directory::Tree` | 1 file | 1/1 |

They are registered in [`batteries.lock`](../../batteries.lock) and every file is
in [`batteries-whitelist.txt`](../../batteries-whitelist.txt), so the release-time
gate (`scripts/battery-testsuite.sh`, run by `release.yml`) re-runs all 25 files
against the shipped library on every release; a regression blocks the release.
Nothing was patched into the vendored sources — they run on mutsu unmodified.

## Why these modules

They were not "selected" from a candidate field in the usual sense: they are the
declared `depends` of `HTTP::UserAgent`, and the
[adoption policy](../../BATTERIES.md#1-adoption-policy--community-first-adopt-as-is)
says to run the genuine module rather than route around its dependencies. The
alternatives were therefore not other URL/base64 libraries but *other ways to
satisfy the dependency*, and each lost for the same reason:

- **Reimplement them natively in the interpreter** (a `URI` builtin, a native
  `MIME::Base64`) — rejected: rung 3 of the adoption policy is a last resort, and
  running the real dist is exactly the compatibility signal we want. It would also
  fork the ecosystem's API surface for no gain, since these dists already pass on
  mutsu.
- **Leave them to `mzef install`** — rejected: it defeats the batteries premise
  ("no install step, no network"), and `HTTP::UserAgent` would then be bundled but
  unusable out of the box.
- **Swap `HTTP::UserAgent` for a client with fewer dependencies** (`HTTP::Tiny`) —
  still tracked as the alternative in [http-client.md](http-client.md), but it
  does not remove this layer: `URI` and the TLS stack are needed either way, and
  `HTTP::UserAgent` is the more complete client.

Each is small, dependency-light (only `File::Temp` → `File::Directory::Tree` has
a bundled dependency of its own), and permissively licensed.

## Not bundled from this layer, and why

- **`Encode`** (`github:sergot`, needed by `HTTP::UserAgent` for non-UTF-8
  charsets) — **blocked on licensing**, not on behaviour: its full upstream suite
  (7/7) passes on mutsu, but the dist carries **no license at all** (no `LICENSE`
  file, `META6.json` has no `license` key, no statement in the README or sources).
  [BATTERIES.md §4](../../BATTERIES.md#4-license-policy) makes a compatible
  license a hard gate, so it cannot be redistributed inside mutsu until upstream
  states one. Resolving this is an upstream-contribution task (rung 4 of the
  policy): ask <https://github.com/sergot/perl6-encode> to add an explicit
  license.
- **`File::Temp`** — Artistic-2.0 and otherwise ready (2/3 upstream files pass),
  held back only by a live mutsu bug: `t/03-tempfile` loads the module through
  `'use File::Temp; &tempfile, &tempdir'.EVAL`, and a module loaded inside `EVAL`
  loses its file-scoped helper subs afterwards (`Unknown function: make-temp`).
  Bundled once that is fixed, so the gate stays all-green.

## Known drift (outside the upstream suites)

`URI::Query` wraps each value in a `Proxy` container. Rendering a *list* that
holds a `Proxy` does not FETCH through it in mutsu, so `say $u.query<x>` prints
`(Proxy)` where raku prints `(1)`; indexing the element (`$u.query<x>[0]`)
FETCHes correctly and matches raku. Not covered by `URI`'s own suite (which
compares element values, not the list gist). The general fix — FETCH a `Proxy`
element while building a collection's `gist`/`Str` — needs the interpreter, since
value rendering is otherwise pure; tracked as follow-up work.

## Dependency + license facts

```
HTTP::UserAgent
├─ URI                    v0.3.8   (Artistic-2.0, zero deps)
├─ MIME::Base64           v1.2.5   (Artistic-2.0, zero deps)
├─ HTTP::Status           v0.0.5   (Artistic-2.0, zero deps)
├─ DateTime::Parse        v0.9.3   (MIT, zero deps)
├─ File::Temp             v0.0.12  (Artistic-2.0)   -- pending, see above
│  └─ File::Directory::Tree v0.2.1 (Artistic-2.0, zero deps)
├─ Encode                 v0.0.4   (NO LICENSE)     -- blocked, see above
└─ IO::Socket::SSL                 (MIT)            -- already bundled
```

`HTTP::Status`'s `META6.json` says `NOASSERTION`, but its README states
"free software; you can redistribute it and/or modify it under the Artistic
License 2.0" (Copyright 2012-2020 Timothy Totten; 2021, 2022, 2025 Elizabeth
Mattijsen), which clears the gate. `DateTime::Parse`'s `META6.json` has no
`license` key either, but the dist ships an MIT `LICENSE` file.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump a module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `URI` | <https://github.com/raku-community-modules/URI> | v0.3.8 | `f4489248` |
| `MIME::Base64` | <https://github.com/raku-community-modules/MIME-Base64> | v1.2.5 | `7964e26f` |
| `HTTP::Status` | <https://github.com/raku-community-modules/HTTP-Status> | v0.0.5 | `71cc3c76` |
| `DateTime::Parse` | <https://github.com/sergot/datetime-parse> | v0.9.3 | `4ad4ea1d` |
| `File::Directory::Tree` | <https://github.com/raku-community-modules/File-Directory-Tree> | v0.2.1 | `b34d800a` |

```sh
# 1. Clone the new upstream revision, then copy the runtime tree + attribution.
#    Upstream tests/CI/precomp artifacts are deliberately NOT vendored: the
#    release gate fetches them fresh at the pinned commit (BATTERIES.md §3).
rsync -a --exclude '.precomp' <checkout>/lib/ modules/<Dist>/lib/
cp <checkout>/{META6.json,LICENSE,README.md,Changes} modules/<Dist>/   # those that exist

# 2. Bump the module's `commit` row in batteries.lock and the table above.
# 3. Re-run the gate and review the diff (a newly failing file is a regression
#    to fix, not to whitelist away):
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt

# 4. Refresh the Pages manifest:
python3 scripts/gen-batteries-manifest.py
```

A *deployed* mutsu can also take a patched module without a re-vendor —
`mzef install URI` shadows the bundled copy. Re-vendoring is for the next
release, so fresh installs ship the fix too.

## API sketch

```raku
use URI;
my $u = URI.new('https://example.com:8443/a/b?x=1#frag');
say $u.scheme, $u.host, $u.port, $u.path, $u.query, $u.fragment;
say $u.query<x>[0];                        # 1  (query is a URI::Query, Associative)

use MIME::Base64;
say MIME::Base64.encode-str('hello');      # aGVsbG8=
say MIME::Base64.decode-str('aGVsbG8=');   # hello

use HTTP::Status;
say get_http_status_msg(404);              # Not Found
say is-client-error(404);                  # True

use DateTime::Parse;
say DateTime::Parse.new('Sun, 06 Nov 1994 08:49:37 GMT').Date;   # 1994-11-06

use File::Directory::Tree;
mktree 'a/b/c';
rmtree 'a';
```

## License

- `URI` — Artistic-2.0. `MIME::Base64` — Artistic-2.0. `File::Directory::Tree` —
  Artistic-2.0. `HTTP::Status` — Artistic-2.0 (stated in the README).
  `DateTime::Parse` — MIT.
- Vendored verbatim with `LICENSE` / `META6.json` / `README` preserved; sources
  unmodified (per [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
