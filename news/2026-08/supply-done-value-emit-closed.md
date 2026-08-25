# `done VALUE` inside a supply block emits the value before completing

`done` with a payload is sugar for `emit VALUE; done`, so

```raku
my $supply = supply {
    for 1 .. 3 { emit($_); }
    done 42;
}
$supply.tap: -> $v { say "Val: $v" }, done => { say "No more" }
```

must print `Val: 1` / `Val: 2` / `Val: 3` / `Val: 42` / `No more`. mutsu used to
drop the `42`: the parser turned a bareword `done` straight into
`Stmt::ReactDone` on the assumption that `done` never takes a payload, which is
only true of the parenthesized `done()` call form.

The ticket filed for this turned out to be stale — `df871c8cc` ("fix: emit value
before supply done") had already fixed it, pinned by `t/supply-done-value-emits.t`.
Re-running the ticket's own repro verbatim against current `main` produces the
full five-line output identically under `raku` and `mutsu`. The ticket was never
removed when the fix landed; this closes the bookkeeping.
