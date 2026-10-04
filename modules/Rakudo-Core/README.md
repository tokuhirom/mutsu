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
| `Telemetry`     | `lib/Telemetry.rakumod`   | `9ccb1decfc2e45e504b924589743a24b`  |
| `Test`          | `lib/Test.rakumod`        | `f34dec45d52ad099c37f42fdbd93e277`  |
| `NativeCall`    | `lib/NativeCall.rakumod`  | `4bd77651da44cd061a50a582457b278f`  |
| `NativeCall::Types` | `lib/NativeCall/Types.rakumod` | `c9f3a7912199f91a89a27bbcd81e2f78` |
| `NativeCall::Dispatcher` | `lib/NativeCall/Dispatcher.rakumod` | `1591768e4f6679935f4cc4640cdb0dc8` |
| `NativeCall::Compiler::GNU` | `lib/NativeCall/Compiler/GNU.rakumod` | `e4137971eea209f610fa1415449c2a6c` |
| `NativeCall::Compiler::MSVC` | `lib/NativeCall/Compiler/MSVC.rakumod` | `0eef1fd91fb8080009fb70c14a6e9776` |

(`LICENSE` is `rakudo-2026.06/LICENSE`, md5 `18740546821e33d23e8809da70d4a79a`.)

### `NativeCall` is vendored but not yet loaded

The five `NativeCall` files are here, verbatim, ahead of the switch
([ADR-11203](../../docs/adr/11203-nativecall-runs-upstream-via-the-backend-neutral-path.md),
[#11203](https://github.com/tokuhirom/mutsu/issues/11203)). `use NativeCall` and
`use NativeCall::Types` are still intercepted by name and served by the native
provider (`src/runtime/nativecall*.rs`) until the interpreter runs these files
and every bundled-library suite passes under them; they are deliberately absent
from `META6.json`'s `provides` until then. `NativeCall::Dispatcher` is never
loaded on mutsu even after the switch: upstream only `require`s it when
`$*RAKU.compiler.?supports-op('dispatch_v')` is true, and mutsu takes the
backend-neutral `nqp::nativecall` path instead.

To see how far the real module gets today, run `scripts/nativecall-upstream-trial.sh`:
it copies these files under a renamed namespace (`UNC`, so the name interception
does not apply) and runs a probe script against them.

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
`experimental`, `newline`, ...) belong here too, once the interpreter runs them.
`NativeCall` was measured on 2026-08-01 as out of reach; that verdict was
re-decided on 2026-10-03 (ADR-11203: its QAST import is dead and its dispatcher
is optional upstream), and its files are now vendored ahead of the switch.

## Updating

Download the Rakudo release tarball, copy the files listed above verbatim, and
update the version in this README and in `META6.json`.
