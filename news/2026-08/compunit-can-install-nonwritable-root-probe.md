# `t/compunit-can-install.t` no longer assumes `/` is non-writable

Test 4 of `t/compunit-can-install.t` ("prefix under a non-writable root cannot
install") built a `CompUnit::Repository::Installation` with
`prefix => "/mutsu-nonexistent-root-{$*PID}/repo"` and asserted `.can-install`
returns `False`, on the assumption that the running user cannot write directly
under the filesystem root `/`. That assumption does not hold everywhere: in at
least one mutsu LXC dev container, the running user (`tokuhirom`, uid 1000)
owns `/` itself (`drwxr-xr-x tokuhirom tokuhirom /`), so `/` genuinely is
writable there, and `can-install` correctly reported `True` per its own
documented semantics ("the nearest already-existing ancestor directory is
writable") — the test then failed, even though the *implementation* was
behaving correctly for the filesystem it was actually running on.

## Fix

Rather than hardcoding `/` as "the" non-writable root, the subtest now probes
a short list of directories that are conventionally non-writable regardless of
who owns the box — `/proc`, `/sys`, `/root` — using the exact same
writability primitive `can-install` itself relies on internally
(`IO::Path.w`, which both call through `path_is_writable()` /
`access(2, W_OK)` in `src/runtime/native_io/helpers.rs`). It picks the first
existing candidate that reports non-writable and builds the unrootable prefix
under that instead of `/`. `/proc`'s top level is a synthetic filesystem that
refuses new entries regardless of permission bits, so it holds even for a
root-owned container; in the dev container that motivated this fix, `/proc` is
`dr-xr-xr-x nobody nogroup` and correctly reports non-writable even though `/`
itself does not.

If none of the candidates turn out to be non-writable in a given environment
(a theoretical case — e.g. every candidate directory missing or the process
running with a uid that bypasses all permission checks everywhere), the
subtest calls `skip` with a clear reason instead of asserting on a false
premise, rather than failing spuriously. This still exercises the same branch
of `can-install`'s logic as before in the common case (CI's non-root Actions
runner, most real dev machines), while being robust to the one dev environment
where it was observed to misfire.

Verified with both `raku` and `mutsu`: all 4 subtests pass under both.
