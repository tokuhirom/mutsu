# Bundled-battery repository links are built lazily

`bench-startup` (`say "hello"`) jumped from about 8.96M to 9.75M instructions (+8.8%), and from
13,917 to 15,232 allocations, on 2026-09-23. The step lines up with #9083, which exposed the bundled
batteries on `$*REPO`'s repository chain (`news/2026-09/repo-chain-sees-bundled-batteries.md`).
That change built one `CompUnit::Repository::FileSystem` per bundled distribution when each
interpreter started. There are 42 of them, and each one cost a `canonicalize` syscall, an `IO::Path`
instance (with its `$*SPEC` / `$*CWD` lookups) and an attribute map. That is about 31 allocations
and 19K instructions per distribution.

None of those links are needed to load a module. `use` finds a battery through
`resolve_module_path`'s fallback and never walks the chain. Only code that introspects the
repository API reads them: `.repo-chain`, `.next-repo`, and `.resolve`, which delegates along
`next-repo`. So almost every program paid for 42 objects it threw away unread.

## What changed

- At startup the interpreter only marks the tail of the default chain as the place where the
  bundled batteries go. It does not build them.
- Every reader of a repository's `next-repo` now goes through one helper,
  `Interpreter::repo_next_link`. These readers are the generic `repo-chain` and `next-repo`
  methods, the Installation `next-repo` arm, and `resolve`'s delegation to the next repository.
  The first read of the marked tail builds the bundled sub-chain, links it in, and clears the mark
  in one delta write. That write is queued rather than taken if the caller still holds a read guard
  on the tail's attributes.
- The mark lives on the repository instance itself, not in interpreter state. A thread or scratch
  interpreter that shares the chain therefore sees the same pending state, and the links are
  appended once.

Chain order and what each link resolves are unchanged. `t/modules/compunit/repo-resolve-bundled.t`
still passes as written. The new `t/modules/compunit/repo-chain-bundled-lazy.t` pins a program
whose first touch of the chain is a hand walk over `.next-repo`, and checks that a second
`repo-chain` does not grow.
