# Bundle the HTTP client's dependency layer

`HTTP::UserAgent` — the HTTP-client battery mutsu is heading for — declares six
runtime dependencies beyond the already-bundled `IO::Socket::SSL`. Their upstream
test suites were run against mutsu first, before any bundling decision, and the
result was better than expected: five of them pass **completely**, unmodified.

Those six are now vendored into `modules/` and resolve with zero config
(`use URI;` — no `-I`, no `zef install`, no network):

| Module | Version | License | Upstream suite |
| --- | --- | --- | --- |
| `URI` | 0.3.8 | Artistic-2.0 | 14/14 |
| `MIME::Base64` | 1.2.5 | Artistic-2.0 | 4/4 |
| `HTTP::Status` | 0.0.5 | Artistic-2.0 | 3/3 |
| `DateTime::Parse` | 0.9.3 | MIT | 3/3 |
| `Encode` | 0.0.4 | pending (see below) | 7/7 |
| `File::Directory::Tree` | 0.2 | Artistic-2.0 | 1/1 |

Every one of those 32 files is registered in `batteries.lock` /
`batteries-whitelist.txt`, so the release-time gate re-runs them against the
shipped library on every release and a regression blocks the release. The gate is
now 50/50. Nothing was patched into the vendored sources — the genuine community
dists run on mutsu as-is, which is the point (BATTERIES.md §1).

**`Encode` ships ahead of a stated license.** The dist carries no license
statement at all — no `LICENSE` file, no `license` key in `META6.json`, nothing in
the README or sources — which BATTERIES.md §4 normally treats as a hard gate. It
is bundled provisionally because the omission reads as an oversight (the author's
sibling dist in this same layer, `DateTime::Parse`, is MIT) and the clarification
is being pursued upstream at
<https://github.com/sergot/perl6-encode/issues/17>. §4 grew an explicit
"provisional bundling" clause for exactly this shape — a linked upstream inquiry
plus a stated exit path — so the exception carries a deadline rather than
softening the rule. If the answer is unfavourable, `Encode` comes back out.

**`File::Temp`** is Artistic-2.0 and follows separately: its `t/03-tempfile`
loads the module through `'use File::Temp; &tempfile, &tempdir'.EVAL`, which
surfaced a real interpreter bug — a module loaded inside `EVAL` lost its
file-scoped helper subs once the `EVAL` returned (`Unknown function: make-temp`).
With that fixed its suite is 3/3.

One rendering gap turned up on the way: the Pages batteries listing's mini
Markdown renderer only understood *fenced* code blocks, so `DateTime::Parse`'s
indented Synopsis rendered as a paragraph. It now handles indented blocks too
(CommonMark-style: they may not interrupt a paragraph), and the e2e test picks a
README that actually carries an example instead of whichever library sorts first.

The selection record — including why satisfying these dependencies natively or via
`mzef install` was rejected, the provenance table, and the re-vendoring recipe —
is `docs/batteries/http-deps.md`. `t/http-deps-battery.t` pins the zero-config
resolution and a smoke slice of each module's API.
