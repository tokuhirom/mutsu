# `t/compunit-can-install.t` test 4 assumes the filesystem root `/` is non-writable

`t/compunit-can-install.t` test 4 ("prefix under a non-writable root cannot install") creates a
`CompUnit::Repository::Installation` with `prefix => "/mutsu-nonexistent-root-{$*PID}/repo"` and
asserts `.can-install` is `False`, on the assumption that the running user cannot write directly
under `/`. That assumption does not hold in every dev environment: in at least one mutsu LXC
container, the running user (`tokuhirom`, uid 1000) owns `/` itself (`drwxr-xr-x tokuhirom
tokuhirom /`), so `/` genuinely is writable, and `can-install` correctly reports `True` per its own
documented semantics ("the nearest already-existing ancestor directory is writable") — the test
then fails, but the *implementation* is behaving correctly for the filesystem it's actually running
on.

Reproduced and isolated 2026-08-21 while working on
`todo/deep/use-lib-dynamic-path-defers-declaration-visibility-to-parser.md` (an unrelated parser
fix touching `src/parser/stmt/simple/lib_paths.rs`): confirmed via `git stash` that the failure
reproduces identically with that change fully reverted, so it is not a regression from that PR —
purely a pre-existing environmental assumption in the test.

This is very unlikely to affect CI (GitHub Actions runners run as a non-root user that does not own
`/`), so it has probably never been observed there. It is filed here rather than fixed inline
because there wasn't a clear best fix at investigation time — options include: probing for an
actually-non-writable path at runtime instead of hardcoding `/` (e.g. skip the subtest if `/` turns
out to be writable by the current user), or picking a prefix under a path more reliably
non-writable across environments (still fragile: any environment where the invoking user owns the
relevant root defeats it by construction). Whoever picks this up should decide the right tradeoff
between "skip on environments where the assumption doesn't hold" vs. "find a more portable way to
assert this branch of `can-install`'s logic".
