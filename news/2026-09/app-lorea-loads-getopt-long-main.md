# App::Lorea loads, and Getopt::Long drives `MAIN`

`App::Lorea` 0.2.6 did not even `use` under mutsu: its main module died with
"Missing block". It now loads, and its `bin/lorea` parses its command line
through Getopt::Long exactly as under rakudo. Getting there took eight
general interpreter fixes:

- **A subscript's `}` is not a block end.** `die "…" if %hash{$_}` followed
  by `for < config help >;` on the next line is one statement. The modifier
  parser treated any operand ending in `}` before a newline as a finished
  statement, so the `for` became a new statement with no block. It now checks
  whether the operand's rightmost term is a subscript.
- **`-I` repositories are on `$*REPO`'s chain, and `file#PATH` specs work.**
  `$*REPO.repo-chain.map(*.path-spec)` now starts with the `-I`/`MUTSULIB`
  directories, as in rakudo. `-I file#/path` names the same repository as
  `-I /path`, so a test can forward its include path to a child process.
- **`Proc::Async.new` flattens its slurpy command.** `'-I' «~« @dirs` used to
  reach the child as one argument.
- **Alias-chain parameters introspect like rakudo.** For `:r(:@regex)`,
  `.named_names` is `(regex r)` and `.sigil` is `@`; `.usage-name` and `.type`
  also come from the innermost variable.
- **`.of` on an unparameterized role or type object** (`Positional`,
  `Associative`, `Callable`, `Code`, `List`, `Map`, …) is `Mu`.
- **A `my token` in a package body is visible from its methods.** This also
  covers methods of a class nested in that body. Method calls do not switch
  the current package, so token lookup now also tries the package the running
  routine was written in, then its enclosing packages.
- **Implicit `MAIN` dispatch honours an `ARGS-TO-CAPTURE` in scope.** It now
  takes the same path as `RUN-MAIN`, so Getopt::Long's exported
  `ARGS-TO-CAPTURE` parses `@*ARGS`.
- **`.flatmap` forces a mapper's `gather` result**, instead of dropping it.
- **A code object never inherits the caller's return type.** `$x.&f` built
  inside `sub g(--> Supply)` enforced `--> Supply` on `f`'s own `return`.

The distribution's only test file, `t/file-change.t`, is `no_baseline`:
rakudo also fails its third assertion. Under mutsu the file still cannot see
the file change it provokes, because `IO::Notification.watch-path` is missing
(#9586). Two other findings were filed along the way: a `gather` returned from
a `.map` block renders and flattens as empty (#9584), and a `CATCH default`
inside a lazily pulled gather catches mutsu's internal suspension signal
(#9585).
