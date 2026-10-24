# An `is export` sub nested in a method body is exported and reads the latest `self`

Rakudo applies `is export` while it compiles the declaration, so
`submethod BUILD { multi sub log('x', *%o) is export { $!bar } }` exports `log` before any object
exists, and the sub's `self` / attributes / lexicals are those of the most recent `BUILD` call
(`capturelex`). mutsu hoisted such declarations only out of `sub` bodies, so Pakku's
`use Pakku::Log; log '...'` died with `Unknown function: log`.

Method bodies now get the same load-time registration, the sub reads `self` (and so `$!attr`)
from the latest activation of the declaring method, and a `multi` candidate without free
lexicals no longer drops the cells its sibling candidates bound (mutsu#14870). The four Pakku
suites that died with `Unknown function: log` now load and run their assertions.
