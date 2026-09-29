# A CI ratchet on building an `Interpreter`

`Buf.subbuf(*-2)` used to build a whole new `Interpreter` on every call just
to evaluate the WhateverCode (#10118). That meant a `%*ENV` sweep, IO handle
seeding and the builtin registry, all for one closure call, and the code then
ran outside the caller's env and pragmas. That site is gone. The new
`make check-interp-construction` (`scripts/check-interp-construction.py`) keeps
another one from appearing.

The check counts every spelling that yields a fresh interpreter in non-test
code: `Interpreter::new()` / `::default()`, `new_regex_scratch*`, and
`clone_for_thread`. It compares the counts per file against
`scripts/interp-construction-allowlist.txt`. Test code is masked the same way
`check-panic-surface.py` masks it, including `#[cfg(all(test, ...))]` modules.
The allowed sites are:

- the process entry points: the binary, the library run API, the REPL and
  `--doc`;
- thread spawns;
- the parse-time slang/export probes, which run a module on a fresh thread
  once per `use`;
- the thread-local regex validation interpreter;
- the eleven regex/grammar scratch interpreters, marked as debt and tracked by
  #10151.

A new file or a higher count fails. A lower count also fails until the list is
re-cut with `--update`, so the allowlist only ever shrinks. It runs as a
`make test` prerequisite and as its own CI lint step, and AGENTS.md now states
the rule.
