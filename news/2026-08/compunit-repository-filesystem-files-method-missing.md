# `CompUnit::Repository::FileSystem.files` implemented, with real version-range matching

`$repo.files('bin/zef', :ver<419.0+>)` died with
`No such method 'files' for invocant of type 'CompUnit::Repository::FileSystem'`.
`CompUnit::Repository::Installation` had a `.files`, but the FileSystem
repository — the one backing `use lib` / `-I` / `MUTSULIB` — had never grown
one.

## What `.files` is

Per `raku-doc/doc/Type/CompUnit/Repository/FileSystem.rakudoc`, `.files` is the
non-module twin of `.candidates`: its first argument is a *file path*
(`bin/zef`, `resources/config.json`), not a module short-name, and it returns
the META of every distribution at the prefix that both matches the given
`:auth`/`:ver`/`:api` and actually provides a file at that path. Measured
against rakudo v2026.06, each returned element is the distribution's META
**Hash**, which is what makes the documented `.head.<name>` spelling work.

## The fix

New `src/runtime/methods_distribution_cur_files.rs` implements `cur_fs_files`,
wired into the FileSystem method dispatch in `methods_instance_ops.rs`. It reads
the META6.json at the prefix (falling back to the parent directory, so a
`-Ilib`-style prefix still finds its distribution root — the same fallback
`cur_fs_candidates` already used), applies the selectors, and asks
`build_dist_files_hash` — the existing map of every file the distribution
provides — whether the requested path is among them.

The interesting part was `:ver`. The pre-existing depspec matcher compared
version selectors by **plain string equality**, which is wrong in both
directions: `:ver<0.4.0+>` failed to match a distribution at `0.4.0`, and
`v1.0` failed to match `1.0`. `.files` therefore got a real
`version_selector_matches` built on the existing `version_cmp_parts` ordering: a
trailing `+` means "this version or later", `-` means "or earlier", a `*` part
is a wildcard that truncates the comparison, and a bare selector compares equal
by version semantics rather than by spelling. The older string-equality matcher
in `matches_depspec` is left alone deliberately — it drives live module
resolution for zef, and re-basing that on range matching is a separate change
with its own blast radius.

Verified against the doc's own examples over the vendored zef tree:
`.files('bin/zef', :ver<0.4.0+>).head.<name>` is `zef`,
`.files('bin/zef', :ver<419.0+>)` is empty, matching rakudo exactly.

Pin: `t/eval-compunit-introspection.t` builds a throwaway distribution in
`$*TMPDIR` and exercises exact `:ver`, open-ended `:ver`, an out-of-range
`:ver`, a non-matching `:auth`, and a file the distribution does not provide.
It passes verbatim under `raku`.
