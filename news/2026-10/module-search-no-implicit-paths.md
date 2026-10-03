# Module resolution no longer searches the script's directory or ancestor `packages/` trees

Module resolution used to consult two implicit sources that Rakudo does not,
and both ranked above the bundled batteries: the script's own directory (when
no plain lib path was given), and `<ancestor>/packages/<Top>/lib` /
`<ancestor>/roast/packages/<Top>[-Helpers]/lib` for every ancestor of the
script path — a convenience for roast's `Test::Util`. The parser's
parse-time export scan had a matching fallback, plus the current directory.

That made results depend on where a script lives: a same-named file next to a
script, or a stray `packages/` directory anywhere above it, silently replaced
a bundled module (#11213). Both are gone from the runtime resolver and the
parser scan, so the search order is exactly `use lib` → `-I` → `MUTSULIB` →
installed → bundled batteries. Roast files already reach `Test::Util` through
their own `use lib $?FILE.IO.parent(2).add('packages/Test-Helpers')`.

The ancestor walk had also been hiding a real gap: roast's
`S11-modules/require.t` installs its own repository with
`PROCESS::<$REPO> := CompUnit::Repository::FileSystem.new(:next-repo($*REPO), :prefix(...))`
and then `require`s from it, and that only worked because the walk happened to
find `roast/packages/Fancy/lib`. The resolver now reads repositories the
program binds into `$*REPO` itself off the chain and searches them first, as
the head of the chain. The default `$*REPO` head (prefix `.`) is marked so it
is never mistaken for one — the current directory is not a search location.

`t/modules/module-search-no-implicit-paths.t` pins all three behaviours.
