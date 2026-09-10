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

### `Test` runs verbatim, and is what `use Test` resolves to

This file is the unmodified upstream `Test.rakumod`, and mutsu runs it verbatim
-- it was never renamed or shimmed. Since 2026-09-10 it is also **the only
provider**: a bare `use Test` loads this file, and there is nothing else it
could load. Mutsu's native TAP provider was retired on 2026-09-10
([#7566](https://github.com/tokuhirom/mutsu/issues/7566)); the `MUTSU_REAL_TEST`
switch that used to select between the two is gone with it.

Every `t/` file and every roast file stands on `Test`, so the switch was held
until nothing regressed under it. It was attempted on 2026-09-07/08 and
withdrawn, because the `Bundled-library test suites` gate regressed on four
upstream distributions -- four unrelated interpreter gaps, none of them a `Test`
compatibility problem. Those were fixed one at a time
([#7555](https://github.com/tokuhirom/mutsu/issues/7555) records the chase) and
the gate now passes under this module with more files green than under the
native provider. Pinned by `t/vendored-real-test-module.t`.

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
