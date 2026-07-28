# Battery: YAML — `YAMLish`

**Slot:** YAML parse/emit · **Chosen:** `YAMLish`
(`auth<zef:leont>`, v0.1.3, Artistic-2.0) · **Kind:** Adopted (community module,
to be vendored as-is) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard gate)
→ dependency weight → proven behaviour on mutsu → API fit → **safe-by-default
loading** (see [security](#security-safe_load-by-default)).

The procedure that produced the table below is
[selection-method.md](selection-method.md).

## Status: bundled

`YAMLish` ships at `modules/YAMLish/` and resolves with **zero config**:

```raku
use YAMLish;
my %data = load-yaml("name: mutsu\ntags: [raku, yaml]\n");   # safe_load-equivalent
say %data<tags>[0];                                          # raku
say save-yaml(%data);                                        # emitter
```

**All 5 upstream test files pass — 81 of 81 subtests, the same as `raku`** — and
every file is pinned in `batteries-whitelist.txt`, so a regression in any of them
blocks a release.

Getting there was the normal "fix mutsu first" outcome
([selection-method.md §5](selection-method.md)): the module started at 0/5 files
(it would not even `use`), and the survey's real output was the work list in
[what blocked it](#what-blocked-it-on-mutsu-the-work-list) below. Every fix landed
in the interpreter; the vendored source is untouched.

## API

`YAMLish` exports four subs. `load-yaml` **is** the safe loader — there is no
unsafe sibling (see [security](#security-safe_load-by-default)).

| Sub | Purpose |
| --- | --- |
| `load-yaml(Str $input, :$schema = Schema::Core, :%tags)` | Parse a single-document YAML string into Raku data. |
| `load-yamls(Str $input, :$schema = Schema::Core, :%tags)` | Parse a multi-document (`---`-separated) stream; returns a sequence of documents. |
| `save-yaml($document, :$sorted = True)` | Serialize one value to a `---`/`...` delimited YAML document. |
| `save-yamls(**@documents, :$sorted = True)` | Serialize several values into one multi-document stream. |

```raku
use YAMLish;

# Parse: block and flow collections, quoted and block scalars, anchors/aliases.
my %conf = load-yaml(q:to/YAML/);
    ---
    name: mutsu
    versions: [0.17, 0.18]
    description: >
      a minimal Raku
      implementation
    ...
    YAML
say %conf<versions>[1];        # 0.18

# Multi-document.
say load-yamls("---\na: 1\n---\na: 2\n").elems;      # 2

# Emit (keys sorted by default, so output is reproducible).
say save-yaml({ b => 2, a => 1 });
# ---
# a: 1
# b: 2
# ...
```

`:%tags` lets a program add its own tag handlers; `:$schema` selects the scalar
resolution schema (`Schema::JSON`, `Schema::Core` — the default — or
`Schema::Extra`, which also resolves dates and datetimes).

## The field it was chosen from

Enumerated from the local REA + fez indices (`~/.zef/store/{rea,fez}/*.json`,
the same data `mzef` uses), filtered on name/description/tags for YAML, with the
Sparrowdo-VSTS generators and `YAMLScript` (a *programming-in-YAML* language, not
a data parser) excluded as out-of-slot. Reverse-dependency counts are over the
same indices.

| Candidate | Version | Released | License | Runtime deps | Dependents | raku | mutsu |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **`YAMLish`** | 0.1.3 | 2026-07-04 | **Artistic-2.0** | `MIME::Base64` (**already bundled**) | **459** | **5/5** | **0/5** (load-blocked) → now **5/5** |
| `YAML` | 0.1 | 2021-01-28 | Artistic-2.0 | `TestML` (test-only) | 2 | not measured | not measured |
| `YAMLStar` | 0.1.17 | 2026-07-21 | **None** | 0 | 0 | — | — |
| `LibYAML` | 0.2.1 | 2017-06-12 | Artistic-2.0 | `NativeCall` (binds system `libyaml`) | 0 | — | — |
| `YAML::Parser::LibYAML` | 0.0.6 | 2021-01-19 | **None** | `LibraryMake` + NativeCall | 0 | — | — |
| `Config::Parser::yaml` | 1.0.1 | 2017-10-14 | **GPL-3.0** | `Config`, `YAMLish` | (wrapper) | — | — |

`YAMLish`'s 459 dependents is not a rounding artefact — it is the ecosystem's
de-facto YAML module (it is also a common test/config dependency), so it wins the
"proven ecosystem standing" axis by three orders of magnitude over anything else
in the slot.

## Why `YAMLish`, and why each alternative lost

- **`YAMLish`** wins every criterion the policy ranks: Artistic-2.0 (clears the
  hard license gate), **pure Raku** (no NativeCall, so no system-library or FFI
  dependency), maintained (v0.1.3, 2026-07-04, by `leont`), and by far the most
  depended-on YAML dist. Its **only** runtime dependency, `MIME::Base64`, is
  **already bundled** by mutsu (the HTTP dependency layer), so it adds **zero new
  vendored deps**. It is both a **parser and an emitter** (`load-yaml` /
  `load-yamls` / `to-yaml`).
- **`YAMLStar`** (ingy, the original YAML author; a fresh pure-Raku YAML 1.2
  loader) is technically appealing but is **out on the license hard gate — no
  license is declared anywhere** ([BATTERIES.md §4](../../BATTERIES.md#4-license-policy)),
  and it has 0 dependents. Revisit only if it gains a permissive license.
- **`YAML::Parser::LibYAML`** and **`Config::Parser::yaml`** are hard-gated out —
  the former declares no license, the latter is **GPL-3.0** (copyleft, not
  bundleable). `Config::Parser::yaml` depends on `YAMLish` anyway.
- **`LibYAML`** is a NativeCall binding to the system `libyaml`. Even setting
  aside its age (2017) and 0 dependents, binding a native library is a heavier
  bundling story than a single pure-Raku file, and it would gate on mutsu's
  NativeCall completeness rather than on ordinary language features.
- **`YAML`** (raku-community-modules) is Artistic-2.0 but old (v0.1, 2021), has
  only 2 dependents, and its test suite pulls in `TestML`. It is the fallback if
  `YAMLish` ever proves unfixable, not the lead.

## Security: `safe_load` by default

The concern with any YAML loader is unsafe deserialization — a document that
instantiates arbitrary objects or runs code (Python's `yaml.load` vs
`yaml.safe_load`; Perl's `YAML::XS` object tags). **`YAMLish` has no such mode**,
which is exactly what a bundled default should be:

- Tag resolution goes through a **fixed callback table** (`%default-tags`), whose
  handlers produce only plain data — `Str`, `Int`, `Rat`, `Any`, a decoded
  `Blob` (`binary`), `List`, `Hash`, `Set`. There is **no `EVAL`, no arbitrary
  class construction, and no attacker-controlled `require`** (the one `require`,
  for `MIME::Base64` behind the `!!binary` tag, is hardcoded).
- An **unknown scalar tag** falls back to the raw string value; an unknown
  mapping/sequence tag has no callback and simply **fails**, rather than
  constructing anything.
- The only extension point is the caller-supplied `:%tags` map — **opt-in and
  under the program's control**, never driven by the document. This is the safe
  design: `load-yaml` *is* the safe loader; there is no unsafe sibling to reach
  for by mistake.

So on the axis the user flagged, `YAMLish` is not merely acceptable — being
safe-by-default with no unsafe-load path is a positive reason to pick it.

## What blocked it on mutsu (the work list)

All five files started at 0/5 — they died identically at **module-load time**,
before any test ran. Every item below was fixed **in the interpreter**; none of
them was YAMLish-specific, and the vendored source was never touched.

1. **`* => *` did not curry into a `WhateverCode`** — the original load blocker
   (`news/2026-07/whatever-curry-through-fatarrow.md`,
   `t/whatever-curry-fatarrow.t`). `YAMLish`'s `flatten-tags` runs on `use`:

   ```raku
   %tags.kv.map({ |$^value.kv.map($^namespace ~ * => *) })
   ```

   In Raku, `=>` participates in Whatever-currying, so `$^namespace ~ * => *` is
   a 2-arg `WhateverCode`. mutsu used to build a literal `Pair(Whatever,
   Whatever)`, which `.map` rejected with `X::Cannot::Map: Cannot map a Pair to a
   Seq`, aborting the `use`. Fixed at the `=>` construction site.

2. **A placeholder var inside a nested `WhateverCode` was mis-collected as its
   parameter** — exposed once #1 was fixed
   (`news/2026-07/placeholder-var-inside-nested-whatevercode.md`,
   `t/placeholder-in-nested-whatevercode.t`). In the same `flatten-tags` line,
   `$^namespace` is a placeholder of the outer block, but the inner
   `$^namespace ~ * => *` curries and mutsu swept `$^namespace` into the inner
   WhateverCode's signature. Fixed by descending `collect_placeholders_shallow`
   through WhateverCode closures. **With #1 and #2, `use YAMLish` loaded.**

3. **`Grammar.parse` did not dispatch inside the module.** `nextwith` from an
   overridden grammar `parse` had no MRO candidate to defer to; a sibling grammar
   did not inherit the CORE `Grammar`; module-local type shadowing resolved a
   builtin type ahead of the package's own. Plus five grammar/regex gaps
   (`$<name>` slot-first, proto `:<int>` variants, `<|w>`, reduce-time `$/.hash`,
   mid-pattern `$` anchor) and general `<?subrule>` lookahead.

4. **Block collections.** A `{ … }` block that does not `make` now runs *inline
   during matching* (Rakudo semantics) rather than at reduce time, `:my` lexicals
   thread through the capture store, and a subrule argument naming one is no
   longer pre-rendered to `Nil`.

5. **Post-parse gaps**: a nested type name (`module M { class A::B }`) is
   registered under its enclosing package; an `@`-sigilled parameter de-itemizes;
   a user-defined method outranks a same-named builtin.

6. **The last 53 subtests** — block scalars, anchors, the whole round-trip file,
   `%TAG` directives — came from six more general regex bugs, written up in
   `news/2026-07/yamlish-upstream-suite-passes.md`: a lookaround's body is part of
   the same regex (so bound parameters interpolate into it, and its keyword may be
   followed by a newline); `:my` lexicals reach the sub-patterns of their own
   regex; a mid-pattern `$` is end-of-*string*; a goalpost (`~`) takes the greedy
   inner match; a `::`-qualified subrule is resolved relative to its package; and
   zero iterations still mark their captures as quantified.

## Provenance and update procedure

Per [BATTERIES.md §3](../../BATTERIES.md#updating-a-vendored-module-must-be-documented-per-library).
To bump the module, re-vendor — do **not** hand-edit the vendored tree:

| Module | Upstream | Pinned version | Commit |
| --- | --- | --- | --- |
| `YAMLish` | <https://github.com/Leont/yamlish> (`zef:leont`) | v0.1.3 (2026-07-04) | `2a1d04ab` (tag `0.1.3`) |

What is vendored: `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`.
Upstream `t/`, `xt/`, `test-suite/`, `dist.ini`, `TODO.md` and `.precomp`
artifacts are excluded — the release gate fetches the tests fresh at the pinned
commit. `MIME::Base64` is already bundled, so this adds no new dependency tree.

```sh
rsync -a --exclude 't/' --exclude 'xt/' --exclude 'test-suite/' \
      --exclude '.github/' --exclude '.precomp/' --exclude 'dist.ini' \
      --exclude 'TODO.md' <checkout>/ modules/YAMLish/
# then bump the commit in batteries.lock, re-run the gate, refresh the manifest:
cargo build --release && scripts/battery-testsuite.sh --update
git diff batteries-whitelist.txt
python3 scripts/gen-batteries-manifest.py
```

Verification after a bump:

```sh
mutsu -e 'use YAMLish; say load-yaml("a: [1, 2]\n")<a>[1]'   # 2
mutsu -e 'use YAMLish; say save-yaml({ b => 2, a => 1 })'
```

## Security updates

Per [BATTERIES.md §6](../../BATTERIES.md#6-security-updates-and-independent-updatability)
the bundled copy is the lowest-priority source, so `mzef install YAMLish`
shadows it without a mutsu release. `YAMLish` is pure Raku and binds no system
library, so there is no native-layer update path to track.

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`. Clears the
[§4](../../BATTERIES.md#4-license-policy) hard gate. Vendored verbatim with its
`LICENSE` / `META6.json` / `README` preserved for attribution, source unmodified.
