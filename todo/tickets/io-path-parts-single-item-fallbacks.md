# `IO::Path::Parts` single-item fallback methods diverge from Rakudo

The last open item of PLAN.md §8.15 (a repr-focused oracle sweep of built-in
object constructors, found 2026-07-22). Moved here when discovered findings
became per-file `todo/` entries; everything else in that section landed
(`Proc.raku`, `IO::Spec::Unix` instance repr, `IO::Path::Parts` positional repr,
the `[ident]` reduction mis-parse, the one-element-Iterable trailing comma, and
`[Z]`-returns-a-Seq — pins `t/proc-gist-not-exitcode.t`,
`t/io-spec-instance-repr.t`, `t/io-path-parts-repr.t`,
`t/bareword-array-not-reduction.t`, `t/repr-residues-2.t`).

## Repro

For an `IO::Path::Parts` instance with three parts:

| call | mutsu | raku |
|------|-------|------|
| `.elems` | 3 | 1 |
| `.list` / `.List` | 3 pairs | `(self,)` |
| `.keys` | `(volume dirname basename)` | `(0)` |
| `.values` | 3 parts | `(self,)` |
| `.pairs` / `.kv` | 3 pairs | `(0 => self)` |
| `for $parts { }` | 3 pairs | self once (because raku's `.list` is `(self,)`) |

## Assessment — deliberately deferred

These are Rakudo accidents: `.elems == 1` on a three-part container is
nonsensical, mutsu's behaviour is arguably more useful, and no roast test
exercises them. Matching Rakudo here is risky and low value, so this is an
**intentional divergence** unless a dist-compat consumer actually needs it.
Recorded so the divergence is visible rather than forgotten; close this file by
deleting it if the decision is reaffirmed, or implement it if a dist needs it.

## Affected files

`src/runtime/methods_instance_ops.rs` (the native `IO::Path::Parts` arm added
for the positional `.raku`/`.gist` form).

## Also still open, from the same sweep

`IO::Path::Parts.Str` and `IO::Spec::Unix.Str` render `IO::Path::Parts()` /
`IO::Spec::Unix()` where Rakudo renders the `<addr>` object-address form. That
form is non-deterministic and a global divergence, intentionally not matched.
