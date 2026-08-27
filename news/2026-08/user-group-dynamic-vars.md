# `$*USER` and `$*GROUP` now report the real effective uid/gid

`$*USER` and `$*GROUP` were unimplemented and silently read as `Nil`, which made
`+$*USER` return `0` -- "root" -- instead of erroring. A permission check in
user code (`die "not root" unless +$*USER == 0`) could therefore silently take
the wrong branch. Found while measuring `Archive::Libarchive::Raw`'s test
suite (`t/05-archive-read-disk.rakutest` compares libarchive's reported owner
against `+$*USER` / `+$*GROUP`).

## Allomorph behaviour, verified against `raku`

Both are `IntStr` allomorphs, matched exactly against `raku` for every access
pattern the ticket asked to check:

| Expression | Result |
|---|---|
| `$*USER.^name` | `IntStr` |
| `$*USER.Int` / `+$*USER` | the numeric uid (e.g. `1000`) |
| `$*USER.Str` / `~$*USER` | the login name (e.g. `tokuhirom`) |
| `$*USER.gist` | the login name |
| `$*USER.raku` | `IntStr.new(1000, "tokuhirom")` |
| `$*USER == 0` | `False` (unless actually root) |
| `$*USER eq "root"` | `False` (unless actually root) |
| `$*USER.WHAT` | `(IntStr)` |

`$*GROUP` behaves identically for the effective gid/group name.

**Read-only, like the other lazily-built magic vars (`$*KERNEL`/`$*DISTRO`):**
`temp $*USER = "x"` and `$*USER = "x"` both die under `raku` ("Can only use
'temp' on a container" / "Cannot modify an immutable IntStr") and under mutsu
(a dynamic-var-not-found die from the same lazy-materialization path
`$*KERNEL` already used) -- the error text differs, but no existing lazy magic
var round-trips through `temp`/assignment either, so this is consistent with
established behavior, not a new gap.

## Implementation

Following Rakudo's model: `getpwuid(geteuid())` / `getgrgid(getegid())` on
POSIX, **falling back to the bare numeric id (no `Str` facet) when the name
lookup fails** -- a uid/gid with no passwd/group entry is normal in
containers. The fallback is exercised directly by a Rust unit test
(`missing_passwd_entry_falls_back_to_bare_int` in
`src/runtime/io_sysinfo_user.rs`) that drives the allomorph-assembly helper
with a synthetic "no name" lookup, since a real absent-passwd-entry uid can't
be relied on to exist on any given box or CI runner.

`$*USER`/`$*GROUP` join the existing lazy-magic-var pattern
(`Interpreter::lazy_magic_dynamic_var` in `src/runtime/io_env.rs`, alongside
`$*DISTRO`/`$*PERL`/`$*RAKU`/`$*VM`/`$*KERNEL`): they materialize on first
read via `getpwuid_r`/`getgrgid_r` (the reentrant, loop-on-`ERANGE` forms) and
are cached process-wide in a `OnceLock` thereafter, in the new
`src/runtime/io_sysinfo_user.rs`.

**Dependency decision:** no new dependency was needed. `libc` is already an
optional dependency gated behind the existing `native` Cargo feature (used
throughout `src/runtime/` and `src/crash_report/` for `getrusage`, `uname`,
`localtime_r`, signal handling, etc.), so this reuses that existing FFI
surface rather than adding anything new.

**Non-POSIX targets:** the real `getpwuid_r`/`getgrgid_r` path is gated
`#[cfg(all(unix, not(miri), not(target_arch = "wasm32"), feature = "native"))]`,
matching the existing gating convention (`local_timezone_offset_secs`,
`io_sysinfo_host`). wasm32 (the `wasm-e2e` CI job builds with
`--no-default-features --features wasm`, so `native`/`libc` are compiled out
entirely) and Miri (which cannot call a foreign function) both take a "uid 0,
no name" fallback arm that only needs to compile, not resolve a real
identity -- verified with a `cargo check --target wasm32-unknown-unknown
--no-default-features --features wasm`. mutsu ships no Windows target, so no
Windows-specific (`GetUserNameW`-based, Str-only, no numeric part) path was
implemented; the existing non-Unix fallback arm covers that compile case too
were it ever needed.

## Testing

`t/user-group-dynamic-vars.t` pins the allomorph shape and the numeric/string
facet agreement without hardcoding a username or uid (CI runs as a different
user than any dev box): it asserts `.^name`, that `+$*USER`/`+$*GROUP` are
`Int`, that `~$*USER`/`~$*GROUP` are non-empty `Str`, that the two facets of
each allomorph agree with each other, and cross-checks the numeric facet
against the OS's own idea of the effective uid/gid via a `run('id', '-u')` /
`run('id', '-g')` shell-out -- a completely independent source from the
`geteuid()`/`getegid()` calls under test. Verified to produce identical output
under both `raku` and `mutsu`.
