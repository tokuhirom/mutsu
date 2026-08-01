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

(`LICENSE` is `rakudo-2026.06/LICENSE`, md5 `18740546821e33d23e8809da70d4a79a`.)

## Why this directory exists

`Pod::To::Text` was previously provided natively: `use Pod::To::Text` was
recognized as a built-in no-op and `pod2text` was a Rust builtin rendering the
Pod object tree (`src/runtime/io_pod.rs`). That is BATTERIES.md rung 3 applied to
a module that never needed it — unlike `JSON::Fast`, the real `Pod::To::Text` is
168 lines of plain Raku with no `use` statements and no nqp dependency at all.
Bundling the real file replaces the private dialect with the upstream behaviour
and turns any remaining gap into an interpreter bug we can fix.

Other Rakudo core modules mutsu still provides natively (`Test`, `NativeCall`,
`experimental`, `newline`, ...) belong here too, once the interpreter runs them.

## Updating

Download the Rakudo release tarball, copy the files listed above verbatim, and
update the version in this README and in `META6.json`.
