# Battery: YAML — `YAMLish` (selected; blocked on interpreter work)

**Slot:** YAML parse/emit · **Chosen:** `YAMLish`
(`auth<zef:leont>`, v0.1.3, Artistic-2.0) · **Kind:** Adopted (community module,
to be vendored as-is) · **Yardstick:**
[BATTERIES.md §2](../../BATTERIES.md#2-selection-criteria) — license (hard gate)
→ dependency weight → proven behaviour on mutsu → API fit → **safe-by-default
loading** (see [security](#security-safe_load-by-default)).

The procedure that produced the table below is
[selection-method.md](selection-method.md).

## Status: decided, not yet bundled

`YAMLish` is the choice, but it does **not run on mutsu yet** — it must not be
advertised as a working battery until the blocking interpreter bugs are fixed
(per [BATTERIES.md §5](../../BATTERIES.md), only working libraries get a public
row). Under `raku` all 5 upstream test files pass; under mutsu the module fails
to `use` at all. This is the normal "fix mutsu first" outcome
([selection-method.md §5](selection-method.md)): the survey's real output is a
work list, and the winner is unambiguous.

Target API once it runs:

```raku
use YAMLish;
my %data = load-yaml("name: mutsu\ntags: [raku, yaml]\n");   # safe_load-equivalent
say to-yaml(%data);                                          # emitter
```

## The field it was chosen from

Enumerated from the local REA + fez indices (`~/.zef/store/{rea,fez}/*.json`,
the same data `mzef` uses), filtered on name/description/tags for YAML, with the
Sparrowdo-VSTS generators and `YAMLScript` (a *programming-in-YAML* language, not
a data parser) excluded as out-of-slot. Reverse-dependency counts are over the
same indices.

| Candidate | Version | Released | License | Runtime deps | Dependents | raku | mutsu |
| --- | --- | --- | --- | --- | --- | --- | --- |
| **`YAMLish`** | 0.1.3 | 2026-07-04 | **Artistic-2.0** | `MIME::Base64` (**already bundled**) | **459** | **5/5** | **0/5** (load-blocked) |
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

## What blocks it on mutsu (the work list)

Measured 2026-07-25 against `target/debug/mutsu` with `-I lib -I
modules/MIME-Base64/lib`. All 5 files die identically at **module-load time**,
before any test runs, so the file counts understate how close it is — it is
currently a two-bug load path, not five independent failures.

1. **`* => *` did not curry into a `WhateverCode`** — the original load blocker,
   now **FIXED** (`news/2026-07/whatever-curry-through-fatarrow.md`,
   `t/whatever-curry-fatarrow.t`). `YAMLish`'s `flatten-tags` runs on `use`:

   ```raku
   %tags.kv.map({ |$^value.kv.map($^namespace ~ * => *) })
   ```

   In Raku, `=>` participates in Whatever-currying, so `$^namespace ~ * => *` is
   a 2-arg `WhateverCode`. mutsu used to build a literal `Pair(Whatever,
   Whatever)`, which `.map` rejected with `X::Cannot::Map: Cannot map a Pair to a
   Seq`, aborting the `use`. Fixed at the `=>` construction site.

1.5. **A placeholder var inside a nested `WhateverCode` is mis-collected as its
   parameter** — the *new* load blocker exposed once #1 was fixed. In the same
   `flatten-tags` line, `$^namespace` is a placeholder of the outer block but the
   inner `$^namespace ~ * => *` now curries, and mutsu sweeps `$^namespace` into
   the inner WhateverCode's signature, dying with `Placeholder variable
   '$^namespace' cannot override existing signature`. Filed:
   `todo/tickets/placeholder-var-inside-nested-whatevercode.md`. This is a
   placeholder-scoping bug, independent of `=>`.

2. **`Grammar.parse($input)` fails to dispatch inside the full module** —
   deeper, not yet root-caused. After the load blocker is patched past locally,
   `load-yaml` reaches `Grammar.parse($input)` and dies with
   `X::Method::NotFound: Unknown method value dispatch (fallback disabled):
   parse`. This is **not** simply "a user grammar named `Grammar` shadowing the
   core type" — an isolated `grammar Grammar { token TOP {\d+} };
   Grammar.parse("123")` works fine under mutsu, so the failure is
   context-dependent (the module defines four grammars — `Grammar`,
   `Schema::JSON`, `Schema::Core`, `Schema::Extra` — with inheritance and heavy
   actions) and still needs reduction. Filed:
   `todo/tickets/yamlish-grammar-parse-dispatch.md`. The YAML grammar
   (lib/YAMLish.rakumod:150–783) is large and action-heavy, so further
   grammar-feature gaps may surface once this one is cleared.

## Provenance (for when it is vendored)

| Module | Upstream | Pinned version | Auth |
| --- | --- | --- | --- |
| `YAMLish` | <https://github.com/Leont/yamlish> (`zef:leont`) | v0.1.3 (2026-07-04) | `zef:leont` |

To vendor: copy `lib/` plus `META6.json`, `LICENSE`, `README.md`, `Changes`;
exclude upstream `t/`, `xt/`, precomp artifacts (the release gate fetches the
tests fresh). `MIME::Base64` is already bundled, so no new dependency tree.

## License

**Artistic-2.0** — declared in `META6.json` and shipped as `LICENSE`. Clears the
[§4](../../BATTERIES.md#4-license-policy) hard gate. To be vendored verbatim with
its `LICENSE` / `META6.json` / `README` preserved for attribution.
