# A subtest says why its body died

A `subtest` whose body throws is reported as a failing test, but the reason was
dropped on the floor. All the reader saw was

```
# Subtest: Interaction of middleware written as Cro::Transform with HTTP router
    1..0
not ok 6 - Interaction of middleware written as Cro::Transform with HTTP router
```

`finish_subtest` took the body's `Result` only to decide the ok/not-ok verdict
and never rendered the error, so a dying subtest was one of the least
diagnosable failures in the suite: no message, no line, no exception type — and
nothing to distinguish "the body threw on its first statement" from "the body
ran and planned zero tests".

The reason now goes to `$*ERR`, indented to the subtest's level, next to the
`# You failed N tests of M` line that already lives there:

```
    # subtest died: Expected IO::Handle
```

stdout — where the TAP stream lives — is untouched, so no plan or test count
changes. When the error carries a typed exception its `.message` is rendered
(via `exception_message_text`, so a class that computes its message gets its own
text); otherwise the raw error message is used.

This immediately paid for itself: the vendored Cro suite's
`http-middleware.rakutest` subtest 6 had been a silent `1..0`, and the one new
line named the culprit — a bareword `get` inside a multi-statement `route` block
reaching mutsu's builtin `get` (read a line from a handle) instead of
`Cro::HTTP::Router`'s exported one. That bug is now written up with its
debugger trace in
`todo/tickets/imported-sub-loses-to-a-builtin-inside-a-subtest.md`.

Pinned by `t/subtest-reports-why-its-body-died.t` (both checks also pass under
`raku`, which reports the same reason on stderr).
