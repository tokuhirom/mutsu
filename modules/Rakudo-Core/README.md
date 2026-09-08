# Rakudo-Core (vendored)

Modules that **Rakudo ships inside its own core library** (`rakudo/lib/`) rather
than through the Zef ecosystem. They are ordinary Raku source, so mutsu runs the
genuine upstream implementation instead of reimplementing it natively.

- Upstream project: <https://github.com/rakudo/rakudo>
- Imported from release **2026.06**, source tarball
  <https://rakudo.org/dl/rakudo/rakudo-2026.06.tar.gz> (`rakudo-2026.06/`)
- License: **Artistic-2.0** — `LICENSE` in this directory is the upstream
  license file copied verbatim from the same release, and it governs every file
  under `lib/` here. Copyright remains with The Perl Foundation / the Rakudo
  contributors.
- Vendored **verbatim** — do not edit these files. If a module does not run on
  mutsu, fix the interpreter (BATTERIES.md §1 rung 2), never the vendored source.

## Contents

| Module          | Upstream path             | md5 of the imported file           |
| --------------- | ------------------------- | ---------------------------------- |
| `Pod::To::Text` | `lib/Pod/To/Text.rakumod` | `3903bd3642ee99500a4ca67782fc5055`  |
| `Test`          | `lib/Test.rakumod`        | `f34dec45d52ad099c37f42fdbd93e277`  |

(`LICENSE` is `rakudo-2026.06/LICENSE`, md5 `18740546821e33d23e8809da70d4a79a`.)

### `Test` runs verbatim, behind `MUTSU_REAL_TEST=1`

This file is the unmodified upstream `Test.rakumod`, and mutsu runs it verbatim
-- it was never renamed or shimmed. It is **not** yet what a bare `use Test`
resolves to: mutsu's native TAP provider (`src/runtime/test_functions.rs`) is
still the default, and this file is selected with **`MUTSU_REAL_TEST=1`**, which
is also how the dual-provider sweeps (`scripts/test-module-sweep.sh`,
`scripts/roast-test-module-sweep.sh`) compare the two.

```
mutsu t/some-test.t                        # the native provider (default)
MUTSU_REAL_TEST=1 mutsu t/some-test.t      # this file
```

Every `t/` file and every roast file stands on `Test`, so the switch is held
until nothing regresses under it. The roast and `t/` suites already pass; the
`Bundled-library test suites` gate does not, on four unrelated interpreter gaps.
The flip was attempted on 2026-09-07/08 and withdrawn -- see
`todo/deep/vendor-real-test-module-flip.md` for the measurements and
`todo/deep/vendored-test-battery-gate-regressions.md` for what is left. Pinned by
`t/vendored-real-test-module.t`.

## Why this directory exists

`Pod::To::Text` was previously provided natively: `use Pod::To::Text` was
recognized as a built-in no-op and `pod2text` was a Rust builtin rendering the
Pod object tree (`src/runtime/io_pod.rs`). That is BATTERIES.md rung 3 applied to
a module that never needed it — unlike `JSON::Fast`, the real `Pod::To::Text` is
168 lines of plain Raku with no `use` statements and no nqp dependency at all.
Bundling the real file replaces the private dialect with the upstream behaviour
and turns any remaining gap into an interpreter bug we can fix.

Other Rakudo core modules mutsu still provides natively (`NativeCall`,
`experimental`, `newline`, ...) belong here too, once the interpreter runs them —
except `NativeCall`, measured on 2026-08-01 as genuinely out of reach
(`todo/deep/nativecall-cannot-be-vendored.md`).

## Updating

Download the Rakudo release tarball, copy the files listed above verbatim, and
update the version in this README and in `META6.json`.
