# `Thread.new(code => {...}).run` — `.run` method is unimplemented

Found by the doc-diff harness re-run (`docs/doc-diff-backlog.md`, `Language/concurrency.rakudoc:703`).

## Repro

```raku
my $thread = Thread.new(code => { for  1 .. 10  -> $v { say $v }});
# ...
$thread.run;
```

- raku: prints `1` through `10`
- mutsu (`target/debug/mutsu`): crashes —
  ```
  No such method 'run' for invocant of type 'Thread'
    in block <unit> at ... line 3
  ```

## Analysis

`Thread.new(code => {...})` constructs a `Thread` object without starting it; `.run` is the method
that actually starts execution of the thread's code block. mutsu's `Thread` type doesn't implement
`.run` at all (compare with `Thread.start({...})`, the more common one-step constructor-and-start
form, which does exist but has its own bug — see
[thread-start-block-not-awaited-before-process-exit.md](thread-start-block-not-awaited-before-process-exit.md)).

## Affected files (starting point)

- Wherever the `Thread` type's methods are implemented (concurrency runtime module) — needs a
  `.run` method that starts the underlying OS thread for a `Thread.new`-constructed (not-yet-
  started) `Thread` object.
