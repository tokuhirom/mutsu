# An exception raised on a worker thread gets an empty backtrace

Found while implementing the `X::Promise::Broken` gist wrapper
(`news/2026-08/promise-broken-exception-not-wrapped-in-x-promise-broken.md`).

## Repro

```raku
my $p1 = Promise.new;
my $p2 = $p1.then(-> $antecedent { $antecedent.result });
$p1.break("First Result");
try $p2.result;
say $p2.cause.backtrace.Str.raku;
```

- raku: `"  in block  at f.raku line 2\n"`
- mutsu: `""`

The exception's *type* now survives the `.then` boundary correctly
(`X::AdHoc+{X::Promise::Broken}` in both), and the gist wrapper renders. Only
the backtrace lines are missing, so `say $p2.cause` prints

```
Tried to get the result of a broken Promise

Original exception:
    First Result
```

where raku puts a frame line under each half.

## Root cause

`clone_for_thread_excluding` (`src/runtime/runtime_thread.rs`) gives the child
interpreter `routine_stack: Vec::new()` and `cur_source_line: 1`, and nothing
seeds `current_source_file()`. So when a `.then`/`start` callback raises,
`build_backtrace_value()` has neither a callframe nor a file/line to attribute
the frame to, and `format_location` yields an empty string — the resulting
`Backtrace` has empty `text`, which `exception_backtrace_text` correctly reports
as "no backtrace" rather than emitting a bare `in block <unit>` with no
location.

Note this is *not* the same thing as the duplicated-frame bug fixed in
`news/2026-08/promise-cause-duplicate-in-block-backtrace-frame.md`: a `die`
directly inside a `Promise.start` block *does* get a located frame, because the
block's own callframe carries a call-site line. The gap is specifically that a
worker interpreter has no notion of "the source location the spawning code was
at", so anything raised outside a located callframe on that thread is
unattributable.

## Why it is not a small fix

The spawning site would have to hand the child its origin (file plus the line
of the `start`/`.then`/`whenever` that created it), and the child's backtrace
builders would need to treat that as the synthetic bottom frame — which is
exactly the frame the duplicate-frame fix teaches them *not* to synthesize for
thread clones. The two interact, so the origin frame has to be a real recorded
value rather than a guess, and it needs threading through every spawn site
(`spawn_callable_promise`, `promise_chain_method`, the supply/react drive
loops), not just the Promise ones.

## Affected files

- `src/runtime/runtime_thread.rs` (`clone_for_thread_excluding`)
- `src/vm/vm_helpers.rs` (`build_backtrace_string`,
  `build_backtrace_value_with_leading`, `stack_bottom_is_mainline_unit`)
- `src/runtime/builtins_system.rs` (`spawn_callable_promise`)
- `src/runtime/methods_promise.rs` (`promise_chain_method`)
