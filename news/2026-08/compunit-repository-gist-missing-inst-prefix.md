# `CompUnit::Repository` objects render as `<short-id>#<prefix>`, not `TypeName.new`

`.say for $*REPO.repo-chain` printed the generic instance fallback —
`CompUnit::Repository::FileSystem.new`, `CompUnit::Repository::Installation.new`
— instead of the documented short-kind-and-path form rakudo prints
(`inst#/home/user/.raku`, `file#/path/to/lib`).

## Root cause

Neither repository type defined `.Str` or `.gist`, so both fell through to the
default `TypeName.new` rendering for an `Instance` with no stringification of
its own. `CompUnit::Repository::Installation` already computed exactly the right
string for its `.path-spec` method (`inst#{prefix}`); nothing exposed it as the
gist.

Measured against rakudo v2026.06, the two renderings differ and both matter:

- `.Str` is the **bare prefix path** (`/home/user/.raku`).
- `.gist` is `<short-id>#<prefix>` (`inst#/home/user/.raku`), and `.say` uses
  the gist.

## The fix

Added `Str`/`gist` to the Installation dispatch
(`src/runtime/methods_distribution.rs`) and to the FileSystem dispatch
(`src/runtime/methods_instance_ops.rs`), each returning the bare prefix and the
`<short-id>#<prefix>` form respectively, derived from the repository's own
`prefix` attribute rather than a literal.

The *contents* of mutsu's repo chain legitimately differ from rakudo's (mutsu's
default chain starts with a FileSystem repo, so it leads with `file#…` where a
stock rakudo leads with `inst#…`) — that was never the bug, and the ticket said
so. The format is what was wrong, and it now matches.

Pin: `t/eval-compunit-introspection.t` asserts `.Str` equals the prefix, `.gist`
equals `file#` plus the prefix, and that every `repo-chain` entry gists with a
`#` separator. It passes verbatim under `raku`.
