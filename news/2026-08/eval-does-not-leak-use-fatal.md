# `use fatal` no longer leaks out of an `EVAL`

A pragma the EVAL'd unit turns on is scoped to that unit. mutsu keeps `use
fatal` as an interpreter-wide flag (`Interpreter::fatal_mode`) and never
restored it around `EVAL`, so the *caller* went on throwing for every later soft
`Failure`:

```
$ raku  -e 'use MONKEY-SEE-NO-EVAL; try { EVAL q{use fatal; "foo"[2]} }; my $f = "bar"[5]; say "soft: " ~ $f.^name'
soft: Failure
$ mutsu -e '...same...'
Index out of range. Is: 5, should be in 0..0
```

`eval_eval_string` now saves and restores it, next to the `=pod` and
`__mutsu_in_eval` restores it already did.

## Why it mattered

`throws-like 'use fatal; …', X::Whatever` is a common assertion shape, and under
rakudo's real `Test.rakumod` every one of them EVALs its string — so a single
such assertion poisoned the rest of the file. That is what made the two
`lives-ok`s of `t/statement-call-sinks-its-value.t` report *died* under the
aliased upstream module while passing under mutsu's native provider and under
`raku`. The file is green under all three now.

## A correction

The first diagnosis of that symptom was wrong and is worth recording as such. It
looked like a stale `$!` — the failing assertion is `lives-ok`, whose body in
`Test.rakumod` is `try { $code(); }` followed by `proclaim((not defined $!),
…)`, and the obvious story is that the preceding `throws-like` left `$!` set
where the `try` could not reset it. A ticket was filed saying so.

Instrumenting the sequence killed that story immediately: between the two
assertions `$!` was **undefined**, and the throw happened one line *earlier*
than the `lives-ok`, at the `EVAL` that was supposed to produce a soft Failure.
The lesson is the usual one — the symptom was reported by `lives-ok`, so the
first guess blamed `lives-ok`; printing the state between the two assertions
cost one run and pointed at the pragma instead.

## What is still open

The same flag leaks out of a `sub` body and a `do {}` block, which is a wider
change (the import-scope mechanism that gets a bare *statement* block right also
un-registers functions and classes, so extending it to routine bodies wants its
own pass): `todo/tickets/use-fatal-leaks-out-of-a-sub-or-do-block.md`.

Pinned by `t/eval-does-not-leak-use-fatal.t`, whose 6 assertions are green under
`raku` too.
