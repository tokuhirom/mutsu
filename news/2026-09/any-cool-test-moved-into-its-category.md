# The last stray top-level test moves into its category

`main` went red on the `test` job at its `t/ layout is valid` step:

```
t/any-cool-method-not-found.t sits at t/ top level; every test belongs in a category directory
    scripts/migrate-t-layout.py names the category for a file.
```

Two PRs crossed. `a8fa6878` ("fix: an undefined Any does not answer Cool's methods") added
`t/any-cool-method-not-found.t` at `t/` top level, which was correct when it was written —
`scripts/check-t-layout.sh` still had `MIGRATED=0`, so the top-level rule was off. #7833 then landed
the migration itself, moving 3949 files into categories and flipping `MIGRATED=1`. Neither PR was
wrong about its own diff; the new file simply landed on the other side of the flip, and nothing in
either PR's own CI run could see the combination.

`scripts/migrate-t-layout.py` names `t/oo/method/` for it, which is right by the rule
`docs/t-directory-layout.md` states — place by what the test would catch if it broke. The file is
about method *dispatch* on an undefined invocant (`Any` not answering `Cool`'s methods, while
declared accessors and real built-ins still resolve), not about the `Any`/`Cool` types themselves.

`make check-t-layout` passes again — `3952 tests, all in category directories` — and both its
checkers agree (the shell rules and `migrate-t-layout.py --check`). The test itself is unchanged and
still passes, 26/26.

This is a pure `git mv`; no test content, and no interpreter code, changes.
