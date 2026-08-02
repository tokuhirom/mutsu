# `note("hi")` printed "Noted" and warned about a string in sink context

`note` with an attached parenthesis was not a call at all:

```raku
note("hi");     # raku: writes "hi" to stderr
                # mutsu: wrote "Noted", then
                #   Useless use of constant string "hi" in sink context
```

`note_stmt` matched the keyword and then, finding no whitespace to start an
argument list, fell through to its no-argument form (bare `note` is legal and
prints `Noted`). It returned `Stmt::Note(vec![])` with `("hi")` still unconsumed,
so statement dispatch parsed the leftover as its own statement.

`say`, `print` and `put` do not have the problem because they have no
no-argument form: their `ws1` fails on `say(...)`, the statement parser bails,
and dispatch reaches the general call parser — `note` is in
`BUILTIN_FUNCTION_NAMES` too, so all it needed was the same bail-out.
`note_stmt` now returns an error when the keyword is immediately followed by
`(`.

A space still means the listop form, so `note ("a", "b")` keeps passing one
`List` argument (stderr `(a b)`) while `note("a", "b")` passes two — matching
raku in both cases.

Found while instrumenting Cro::HTTP's router: a debug `note($msg)` added to a
vendored module printed `Noted` instead of the message, which is a fine way to
lose an afternoon.

Pinned by `t/note-with-parens.t` (checked against raku).
