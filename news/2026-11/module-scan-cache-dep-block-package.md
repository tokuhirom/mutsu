# A warm module-scan cache no longer splits Selkie's handle types

The ledger sweep at `435679266` showed Selkie dying in seven test files with
"expected NcplaneHandle but got Notcurses::Native::Types::NcplaneHandle"
(#18848). The trigger was the sweep's load probe. `use Notcurses::Native::Types`
was scanned first, on its own. Its `use NativeCall` scanned NativeCall's
block-form `our module NativeCall { }`, and that left the parser's
package-declared flag set. `unit module Notcurses::Native::Types;` was then
dropped as too late. The on-disk scan cache recorded the module's exported
types under GLOBAL, and every later importer read that poisoned entry.

#18785 (restoring the flag after a nested scan) had already fixed the root
cause. This entry adds the regression test for the cache path:
`t/modules/module-scan-cache-dep-block-package.t` warms a fresh cache with a
probe, then checks the importer that used to fail. Selkie is green again,
96/96 files.
