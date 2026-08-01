# Batteries — mutsu's bundled standard library

mutsu aims to be a **batteries-included Raku implementation**: install the single
binary and you can write practical programs immediately — JSON, HTTP, templating,
a database layer, file utilities — with **no `zef install` step and no network
access** required. This document is the **policy** for what we bundle and how we
decide. Each bundled library additionally has its own **selection record** under
[`docs/batteries/`](docs/batteries/) capturing why it was chosen, what the
alternatives were, and its license.

Related: [`PLAN.md` §1 (Batteries)](PLAN.md) is the task ledger; this file is the
standing policy those tasks implement.

## 1. Adoption policy — community-first, adopt-as-is

The ordering below is strict. Do not skip to a lower rung because a higher one
looks like more work up front — the whole point is to stay aligned with the real
Raku ecosystem rather than accreting a private dialect.

1. **Adopt a community library verbatim.** The first choice for any battery is an
   existing, actively-usable Raku module, **vendored unchanged**. We adapt mutsu
   to the module, never the module to mutsu — exactly as we already do for the
   bundled Zef (`vendor/zef/`, our compatibility north star).

2. **If it does not run on mutsu, grow mutsu's core — not the library.** When a
   chosen community module hits a gap (a missing builtin, an incomplete NativeCall
   feature, a parser edge case), the fix goes into the **interpreter**, as a
   general improvement that also helps every other real dist. We do **not** patch
   the vendored source, and we do **not** reimplement the module to route around
   the gap. Running the genuine upstream module *is* the compatibility win.

3. **Self-implement and dual-live only as a genuine last resort.** If no community
   library is viable even after reasonable core work — or none exists — we may
   ship a homegrown module ("dual-lived": usable on mutsu today, and ideally
   publishable to the ecosystem later). This rung is the exception, and every use
   of it must be justified in the library's selection record.

4. **When mutsu has influence, push improvements upstream.** The clean long-term
   shape is not a pile of private forks. Once mutsu carries weight in the
   ecosystem, prefer contributing enabling work back upstream over maintaining
   mutsu-only divergences.

### Consequence: prefer "make the real module run" over "make a shim"

A shim that reimplements a slice of a library is tempting because it is quick, but
it (a) diverges from the ecosystem, (b) has to be maintained forever, and (c)
earns us none of the compatibility signal that running the real module does. When
weighing "grow the core (rung 2)" vs. "write a shim (rung 3)", the core work is
the **gain** even when it is larger; the shim is the **risk** (a private dialect,
a maintenance tail). See the "gain vs. risk" framing in `CLAUDE.md`.

## 2. Selection criteria

When choosing among community candidates for a battery slot, weigh, in roughly
this order:

- **License compatibility** (see §4) — a hard gate. If it is not
  redistributable under a permissive/compatible license, it cannot be bundled.
- **Dependency weight** — prefer few or zero dependencies. A single-file,
  zero-dependency module is dramatically easier to vendor, cache, and keep
  working than one that drags in a large tree.
- **Proven behaviour on mutsu** — does it load and run today, or how far is it?
  A candidate that already `use`s cleanly beats a "better" one that needs a
  multi-session core campaign just to load.
- **API fit and idiom** — a clean, Raku-idiomatic public API that downstream
  code (and other batteries) can build on.
- **Use-case coverage** — the guiding yardstick is **"a small web blog can be
  written with the bundle alone."** A battery earns its slot by moving that goal
  forward.

Record how these applied in the library's selection record; the alternatives you
rejected, and *why*, are as important as the winner.

**The procedure that turns these criteria into numbers is
[docs/batteries/selection-method.md](docs/batteries/selection-method.md)** — how
to enumerate the field from the Zef indices, which metrics to collect (including
the reverse-dependency count), and the rule that a `raku` baseline is measured
*before* mutsu so a dead upstream module is never mistaken for a mutsu bug.
Follow it for every new slot; a readiness claim nobody just re-measured is not
evidence.

## 3. Vendoring and resolution

- **Vendor unchanged, with attribution.** Copy the upstream `lib/` (plus
  `LICENSE`, `META6.json`, and `README` for attribution) verbatim, following the
  `vendor/zef/` precedent. Exclude upstream test suites, CI config, and precomp
  artifacts. Do not hand-edit vendored sources.
- **Zero-config `use`.** An installed mutsu must resolve bundled modules with no
  environment setup. The mechanism is a **built-in default module search path**
  (a `modules/` tree shipped alongside the binary, registered in the interpreter
  the same way `add_default_site_repo()` registers the default site repo).
- **The bundle is the *floor*, not a ceiling — it must be the LOWEST-priority
  source.** Explicit `-I` / `MUTSULIB` / project-local paths, **and the user's
  `mzef`-managed site repo**, all take priority over the bundled `modules/` tree.
  This ordering is deliberate: it is the [independent-update
  mechanism](#6-security-updates-and-independent-updatability) — a newer
  (e.g. security-patched) version installed with `mzef` shadows the bundled copy.
  Bundled modules therefore keep their versioned `META6.json` so version selection
  can prefer a newer installed candidate.
- **Provenance.** Record the upstream URL, version, commit, and license for each
  vendored tree (as `vendor/README.md` does for Zef).

### Updating a vendored module (must be documented per library)

Because a bundled module may need a **security or bug-fix update independently of
the mutsu binary** (see [§6](#6-security-updates-and-independent-updatability)),
**every battery's record must spell out its exact re-vendoring recipe** — a future
maintainer must be able to bump it without reverse-engineering how it was
imported. The recipe, kept in the library's `docs/batteries/<lib>.md`, states:

1. **Upstream + pin** — the source URL and the current version/commit.
2. **The vendor command** — the concrete copy step (an `rsync` recipe like
   `vendor/README.md`'s for Zef), with the exact excludes (upstream `t/`, `xt/`,
   `.github/`, precomp artifacts) and the includes to preserve (`lib/`,
   `resources/`, `LICENSE`, `META6.json`, `README`).
3. **Provenance bump** — update the version/commit/date in the record and the
   [bundle index](#7-bundle-index).
4. **Verification** — the smoke test to re-run after the bump (`use` the module +
   a representative call), so a bad bump is caught.
5. **Re-baseline the test-suite gate** — bump the library's `commit` in
   `batteries.lock` to the matching upstream commit and re-run
   `scripts/battery-testsuite.sh --update`, then **review the
   `batteries-whitelist.txt` diff**. A test file that dropped out of the
   whitelist is a regression to fix, not a smaller baseline to accept.

Vendored sources are **never hand-edited**; an update is always a clean re-vendor
of a new upstream release. If mutsu needs a change to run the module, that change
goes in the interpreter (rung 2), not in the vendored copy.

### Verification: the bundled-library test-suite gate

Upstream test suites are excluded from the vendored copy (above), so "does the
shipped library actually run under this mutsu?" is verified by a dedicated gate:
it fetches every battery's upstream suite at the commit pinned in
`batteries.lock`, runs it against the *bundled* library and a release `mutsu`,
and fails if any file listed in `batteries-whitelist.txt` regressed. It is a
per-file baseline (the `roast-whitelist.txt` philosophy), not an all-green wall,
so suites with known gaps still pin their passing files. Tests that
unconditionally reach a **third-party service** are listed in
`batteries-exclude.txt` and never run: a release must not be blocked by someone
else's outage.

`release.yml`'s `batteries` job is the authoritative run and blocks the publish
job. Because that is too late to *learn* about a regression (a whitelist can rot
for as long as nobody cuts a release — it did, see
`news/2026-07/batteries-gate-runs-post-merge.md`), `ci.yml`'s `test` job runs the
same gate on a push to `main` and on a PR that touches the batteries themselves.
An ordinary PR skips it. Full details:
[docs/batteries/testsuite-gate.md](docs/batteries/testsuite-gate.md).

## 4. License policy

Bundling redistributes the library's source inside mutsu's distribution, so
license compliance is mandatory, not optional.

- **Only permissive / redistribution-friendly licenses may be bundled.** Artistic
  2.0, MIT, BSD, Apache-2.0 and equivalents are fine (most of the Raku ecosystem
  is Artistic 2.0). A copyleft or non-redistributable license disqualifies a
  candidate from bundling — pick a differently-licensed alternative instead.
- **Preserve the upstream license text and attribution.** Keep the upstream
  `LICENSE` and author/`auth` metadata in the vendored tree, unmodified.
- **Record the license** in the library's selection record and in the bundle
  index below.
- **mutsu's own license** must remain compatible with everything it bundles.
- **A dist that simply *states* no license is a hard case, not automatically a
  copyleft one.** Much of the older ecosystem omits the field by oversight. If a
  needed dependency has no statement, the default is still "do not bundle" — but
  it may be bundled **provisionally** when all of: (a) the omission plausibly is
  an oversight (the author's sibling dists are permissive), (b) the clarification
  is **actually being pursued upstream**, with the issue linked from the
  library's record and the [bundle index](#7-bundle-index), and (c) the record
  spells out the exit path if the answer is unfavourable (remove it; route
  around it). `Encode` is the one library currently in this state — see
  [http-deps.md](docs/batteries/http-deps.md). It is an exception with a
  deadline, not a relaxation of the rule.

## 5. Documentation requirement

"Well-documented" is an explicit project goal, so **documentation is mandatory
when adding a battery**. Each bundled library needs, under `docs/batteries/`:

- an **overview** (what it is; that no install is needed);
- a **selection record** — this is a hard requirement, and its rationale must be
  **clear and self-contained**: *why this library was chosen*, **the alternatives
  considered and the specific reason each lost**, and the license. A future reader
  must be able to reconstruct the decision without re-doing the investigation. The
  alternatives-and-why are as load-bearing as the winner.
- **provenance and an update procedure** (see [§3](#3-vendoring-and-resolution)):
  upstream URL, version, commit, and the exact re-vendoring recipe;
- an **API reference** and **example code**.

Use [`docs/batteries/http-client.md`](docs/batteries/http-client.md) as the
template for new records.

### Publish the bundle on the Pages site

The bundled-library set is a headline feature ("batteries included"), so it must
be **visible on the project's Pages site** (`site/`, deployed by
`.github/workflows/pages.yml`), not only in the repo:

- A **Batteries page** (`site/batteries.html`) lists every bundled library
  with a one-line description, its license, and its status.
- It is reachable from the site navigation — add an entry to the `NAV` array in
  [`site/assets/i18n.js`](site/assets/i18n.js) (with an `i18n` label key)
  and give it an active-page state, exactly like the existing pages.
- **Each entry links to its `docs/batteries/<lib>.md` record** so the full
  documentation is reachable from the site. `docs/batteries/*.md` stays the single
  source of truth (linked as GitHub-rendered markdown); do not fork the prose into
  the HTML page — the page carries only the list + summaries + links.
- Note the deploy trigger: `pages.yml` only republishes on `site/**` changes
  (or manual `workflow_dispatch`). A `docs/batteries/` edit alone will not
  redeploy the site, and that is fine because the page links out to GitHub rather
  than embedding the docs. Publish real rows only when the battery actually works
  — do not advertise a not-yet-functional library on the public site.

## 6. Security updates and independent updatability

A bundled library **must be updatable independently of the mutsu binary** when a
security patch lands — users must not have to wait for a new mutsu release to get
a fixed dependency. This is a hard requirement (it is why the bundle is the
lowest-priority source, above). Updates come at two layers:

- **Native system libraries ride the OS.** For batteries that bind a system
  library via NativeCall (e.g. `OpenSSL` → system `libssl`/`libcrypto`), the
  security-critical native code is **not shipped by us** — mutsu `dlopen`s the
  host's library, so its CVEs are patched by the OS package manager
  (`apt upgrade libssl3`, etc.) on its own cadence, independent of mutsu. This is
  a deliberate advantage of binding the system library over statically linking a
  vendored implementation (which would pin the code to our binary and force a
  mutsu re-release for every upstream CVE).
- **Bundled Raku modules are overridable via `mzef`.** The vendored `modules/`
  tree is only the *floor*. A patched module version installed with
  `mzef install <Module>` lands in the higher-priority site repo and **shadows the
  bundled copy** by version selection — no mutsu release required. Re-vendoring the
  patched version into the tree (for the next release, so fresh installs get it too)
  is a separate, isolated operation per the [vendoring policy](#3-vendoring-and-resolution):
  update one module + its provenance record, never the interpreter core.

Document, in each battery's record, which layer(s) its updates come through.

## 7. Bundle index

Legend: **Adopted** = community module vendored as-is · **Dual-lived** = homegrown
(last resort) · **Native** = provided by the interpreter core.

Build order is **bottom-up**: the TLS foundation first, then its dependency
layer, then the HTTP client on top. All three are now bundled, so a program can
make a real `https://` request using nothing but the shipped binary.

| Battery | Provider | Kind | License | Status | Record |
| --- | --- | --- | --- | --- | --- |
| TLS / HTTPS socket (foundation) | `OpenSSL` + `IO::Socket::SSL` | Adopted | MIT / MIT | **Working** — vendored + zero-config `use`; a real `https://` GET runs end-to-end. Needs system `libssl` at runtime. | [tls-openssl.md](docs/batteries/tls-openssl.md) |
| HTTP client dependency layer | `URI` + `MIME::Base64` + `HTTP::Status` + `DateTime::Parse` + `Encode` + `File::Temp` + `File::Directory::Tree` | Adopted | Artistic-2.0 ×5 / MIT / **`Encode`: pending** | **Working** — vendored + zero-config `use`; all 35 upstream test files pass against the bundled copy. ⚠️ `Encode` ships ahead of a stated license — [clarification pending upstream](https://github.com/sergot/perl6-encode/issues/17), see the record | [http-deps.md](docs/batteries/http-deps.md) |
| HTTP client | `HTTP::UserAgent` (`zef:sergot`); `HTTP::Tiny` alt. | Adopted | MIT / Artistic-2.0 | **Working** — vendored + zero-config `use`; all 27 upstream test files pass against the bundled copy (23 are gated; 3 need a test-only dep the harness cannot fetch, 1 is network-excluded — see the record) | [http-client.md](docs/batteries/http-client.md) |
| Template engine | `Template::Mustache` (`zef:raku-community-modules`) | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; 11/13 upstream files pass, including the whole official mustache spec suite. Chosen over `Template6` / `Jinja2` / 7 others by a measured survey | [templates.md](docs/batteries/templates.md) |
| Test helpers | `Test::Util::ServerPort` | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; both upstream test files pass. Lets a bundled-only test suite bind a listener without hardcoding a port, and makes `HTTP::UserAgent`'s local-server tests gateable | [test-helpers.md](docs/batteries/test-helpers.md) |
| YAML | `YAMLish` (`zef:leont`) | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; all 5 upstream files (81/81 subtests) pass against the bundled copy, matching raku. Parser *and* emitter, safe-by-default loading, and its only dependency (`MIME::Base64`) was already bundled | [yaml.md](docs/batteries/yaml.md) |
| JSON | native `to-json` / `from-json` | Native | — | Working | — |
| Secure randomness (CSPRNG) | `Crypt::Random` (`github:skinkade`) | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; all 3 upstream test files pass against the bundled copy, matching raku. OS entropy (`/dev/urandom`) via the new `nqp::open`/`readfh`/`closefh` ops; a hard dependency of Cro::HTTP | [crypt-random.md](docs/batteries/crypt-random.md) |
| Secure path join | `IO::Path::ChildSecure` (`zef:raku-community-modules`) | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; both upstream test files (11 subtests) pass against the bundled copy, matching raku. Guards `../` path traversal when joining untrusted input; a hard dependency of Cro::HTTP | [io-path-childsecure.md](docs/batteries/io-path-childsecure.md) |
| Base64 encoding | `Base64` (`github:ugexe`) | Adopted | Artistic-2.0 | **Working** — vendored + zero-config `use`; both upstream test files pass against the bundled copy, matching raku. Standard + URI-safe alphabets, padding control; a hard dependency of Cro::HTTP (WebSocket keys, basic auth). Distinct from the MIME-flavored `MIME::Base64` above | [base64.md](docs/batteries/base64.md) |
| Database (SQLite) | `DBIish` (`zef:raku-community-modules`) + `NativeLibs` + `NativeHelpers::Blob` | Adopted | BSD-2-Clause / Artistic-2.0 / Artistic-2.0 | **Working** — vendored + zero-config `use`; a real SQLite database can be opened, queried, and written to. 5 of 9 generic/SQLite upstream test files pass cleanly (gated); 4 remain partially blocked on a missing `Any.Int`/`.Num`/`Str.Buf` coercion family — see the record. Full upstream parity already holds against live PostgreSQL/MySQL (not gated — needs a server). Needs system `libsqlite3` at runtime | [database.md](docs/batteries/database.md) |

Other modules with a proven working record (Template::Mustache, HTTP::Parser,
HTTP::Server::Tiny, NativeCall MVP, the Zef CLI) are tracked in `PLAN.md` §1 and
are folded into this index as their bundling + documentation is finalized.

A row marked **Selected, not yet bundled** is a decision recorded ahead of the
vendoring: the candidate has won its survey and the evidence is written down, but
it does not run on mutsu yet, so shipping it would mean shipping a battery the
release gate cannot verify. Such a row carries no `modules/` tree and no
`batteries.lock` entry until its blockers are cleared.
