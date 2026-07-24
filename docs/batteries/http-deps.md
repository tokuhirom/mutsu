# Battery: the HTTP client's dependency layer — `URI`, `MIME::Base64`, `HTTP::Status`, `DateTime::Parse`, `Encode`, `File::Temp`, `File::Directory::Tree`

**Slot:** HTTP client dependency layer · **Kind:** Adopted (community modules,
vendored as-is) · **Licenses:** Artistic-2.0 / Artistic-2.0 / Artistic-2.0 / MIT /
**license clarification pending** / Artistic-2.0 / Artistic-2.0

These are the libraries the chosen HTTP client, `HTTP::UserAgent`
([http-client.md](http-client.md)), depends on at runtime. They are bundled as a
layer because none of them is useful to sequence separately: `HTTP::UserAgent`
cannot ship until all of them do, and each is independently useful to a program
that ships with mutsu (URL parsing, base64, status-code names, HTTP date
parsing, character-set decoding, temp files, recursive directory removal).

## Status: working (every upstream test passes)

All seven resolve with **zero config** (`use URI;` — no `-I`, no install) and
their **complete upstream test suites pass against the bundled copy**:

| Module | Upstream suite | Result |
| --- | --- | --- |
| `URI` | 14 files | 14/14 |
| `MIME::Base64` | 4 files | 4/4 |
| `HTTP::Status` | 3 files | 3/3 |
| `DateTime::Parse` | 3 files | 3/3 |
| `Encode` | 7 files | 7/7 |
| `File::Temp` | 3 files | 3/3 |
| `File::Directory::Tree` | 1 file | 1/1 |

They are registered in [`batteries.lock`](../../batteries.lock) and every file is
in [`batteries-whitelist.txt`](../../batteries-whitelist.txt), so the release-time
gate (`scripts/battery-testsuite.sh`, run by `release.yml`) re-runs all 35 files
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

Each is small and dependency-light (only `File::Temp` → `File::Directory::Tree`
has a bundled dependency of its own); all are permissively licensed except
`Encode`, whose license is still being clarified (next section).

## ⚠️ `Encode`: license clarification pending upstream

`Encode` is bundled, but its license status is **not yet settled**, and that must
stay visible until it is. The dist carries **no license statement at all**: no
`LICENSE` file, no `license` key in `META6.json`, nothing in the README or the
sources. [BATTERIES.md §4](../../BATTERIES.md#4-license-policy) normally makes a
stated, compatible license a hard gate.

We are shipping it ahead of that statement as a deliberate, time-boxed call:
`HTTP::UserAgent` needs it, the author (Filip Sergot, `github:sergot`) is a
long-standing Perl/Raku community member whose other dists in this same layer
(`DateTime::Parse`) are MIT, and the omission reads as an oversight rather than
an intent to restrict. **The clarification is being tracked upstream at
<https://github.com/sergot/perl6-encode/issues/17>.**

Follow-ups, in order of what the answer turns out to be:

- **A permissive license is stated upstream** — re-vendor to pick up the
  `LICENSE` file, record it here and in the [bundle
  index](../../BATTERIES.md#7-bundle-index), and delete this section.
- **No answer, or a license we cannot redistribute** — `Encode` must come back
  out of `modules/`, and `HTTP::UserAgent`'s non-UTF-8 charset path needs another
  route.

Until then, treat this as the one bundled library whose redistribution basis is
provisional; do not cite it as precedent for relaxing §4.

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
├─ Encode                 v0.0.4   (license pending, zero deps)  -- see above
├─ File::Temp             v0.0.12  (Artistic-2.0)
│  └─ File::Directory::Tree v0.2   (Artistic-2.0, zero deps)
└─ IO::Socket::SSL                 (MIT)            -- already bundled
```

`HTTP::Status`'s `META6.json` says `NOASSERTION`, but its README states
"free software; you can redistribute it and/or modify it under the Artistic
License 2.0" (Copyright 2012-2020 Timothy Totten; 2021, 2022, 2025 Elizabeth
Mattijsen), which clears the gate. `DateTime::Parse`'s `META6.json` has no
`license` key either, but the dist ships an MIT `LICENSE` file. `File::Temp`
ships no `LICENSE` file, but declares `Artistic-2.0` in `META6.json` and repeats
it in its README, so there is nothing ambiguous to resolve.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump a module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `URI` | <https://github.com/raku-community-modules/URI> | v0.3.8 | `f4489248` |
| `MIME::Base64` | <https://github.com/raku-community-modules/MIME-Base64> | v1.2.5 | `7964e26f` |
| `HTTP::Status` | <https://github.com/raku-community-modules/HTTP-Status> | v0.0.5 | `71cc3c76` |
| `DateTime::Parse` | <https://github.com/sergot/datetime-parse> | v0.9.3 | `4ad4ea1d` |
| `Encode` | <https://github.com/sergot/perl6-encode> | v0.0.4 | `f61acc36` |
| `File::Temp` | <https://github.com/raku-community-modules/File-Temp> | v0.0.12 | `ad3445e0` |
| `File::Directory::Tree` | <https://github.com/raku-community-modules/File-Directory-Tree> | v0.2 | `b34d800a` |

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

use Encode;
say Encode::decode('iso-8859-2', buf8.new(0xa3));                # Ł

use File::Directory::Tree;
mktree 'a/b/c';
rmtree 'a';
```

## License

- `URI` — Artistic-2.0. `MIME::Base64` — Artistic-2.0. `File::Directory::Tree` —
  Artistic-2.0. `File::Temp` — Artistic-2.0. `HTTP::Status` — Artistic-2.0
  (stated in the README). `DateTime::Parse` — MIT.
- `Encode` — **unstated upstream; clarification pending** at
  <https://github.com/sergot/perl6-encode/issues/17>. See
  [the section above](#️-encode-license-clarification-pending-upstream) for the
  reasoning and the two exit paths.
- Vendored verbatim with `LICENSE` / `META6.json` / `README` preserved; sources
  unmodified (per [BATTERIES.md §4](../../BATTERIES.md#4-license-policy)).
