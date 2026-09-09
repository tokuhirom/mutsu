# Investigated #7803: plain `sub is export` not reachable via `Module::name()` is correct, not a bug

Issue #7803 claimed that a plain (non-`our`) `sub NAME() is export { ... }`
declared inside a `unit module` should be callable package-qualified as
`Module::NAME()` from an importer, and that mutsu's
`Could not find symbol '&NAME' in 'Module'` error for that call was a bug.

Verification against the live Rakudo oracle (v2026.07) shows the premise was
wrong: `raku` throws the exact same error, with the exact same wording, for
the same repro. A plain `sub` declaration is `my`-scoped (lexically scoped)
by default; `is export` only affects what `use` imports into the caller's
lexical scope, it does not place the routine in the package's stash. Only an
`our sub` is a package symbol, so only `our sub ... is export` is reachable
via package-qualified `Module::name()` syntax. `raku-doc/doc/Language/packages.rakudoc`
documents this directly ("ensure you use the 'our' declarator" before a
package-qualified-access example).

mutsu already implements this correctly, and already carries a dedicated
regression test pinning it: `t/module-sub-package-visibility.t` (added for an
earlier, real version of this same bug) asserts that a lexical `sub`/`multi
sub` is not reachable via `Package::name()` — including from inside the
package's own code — while an `our sub`/`our proto` is. That test passes on
`main`.

No code change was needed; issue #7803 was closed as not planned with this
finding recorded in the issue thread.
