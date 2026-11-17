# `dir(:test)` binds the right `$*CWD`, once per listing

`dir`'s `:test` ran every entry's test with `$*CWD` bound to a fresh `IO::Path` of the
listed directory. That was wrong in two ways:

- **The binding.** Rakudo's `IO::Path.dir` runs the test in the listed path's own `CWD`.
  `IO::Path.new($p, :CWD($d))` gives `$d` (roast `S32-io/dir.t`); a path without one gives the
  caller's `$*CWD`, never the listed directory itself. That is the documented behaviour, and
  why `dir('mydir', test => { .IO.d })` does not work. mutsu now binds `$*CWD` only when the
  path carries its own `CWD`.
- **The cost.** The `IO::Path` was built once per entry. Readline's `is native(LIBREADLINE)`
  traits list `/lib/x86_64-linux-gnu` with a regex test 280 times per `use`, 382k entries,
  and building that path was 11% of the load. It is now built at most once per listing.

`use Readline` went from 19.5G to 15.2G instructions (-22%). The rest of the load is the
traits re-running on every warm load, and the regex match per entry (#18003).
