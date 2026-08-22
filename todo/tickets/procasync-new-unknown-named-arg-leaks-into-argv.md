# `Proc::Async.new` shoves an unrecognized named argument into the spawned command's argv

Found by the doc-diff harness batch-3 re-run (`docs/doc-diff-backlog.md`,
`Type/Proc/Async.rakudoc:172` and `:203`).

## Root cause

`Proc::Async.new`'s constructor (`src/runtime/methods_object_native_ctors_io.rs`,
`native_proc_async_new` or equivalent around line 230) only recognizes three named pairs
specially — `:w`, `:out`, `:enc` — and falls through to `positional.push(arg.clone())` for
**any other named pair**, including `:r`:

```rust
for arg in args {
    match arg.view() {
        ValueView::Pair(key, value) if key == "w" => { w_flag = value.truthy(); }
        ValueView::Pair(key, _value) if key == "out" => {}
        ValueView::Pair(key, value) if key == "enc" => { enc = Value::str(value.to_string_value()); }
        _ => positional.push(arg.clone()),
    }
}
```

So `Proc::Async.new(:r, 'echo', 'Raku')` pushes the `:r` `Pair` value itself into
`positional`, which becomes the `cmd` attribute array consumed by `.start()`
(`src/runtime/native_proc_async.rs`). At spawn time the `Pair` stringifies to something
like `"r\tTrue"` and is inserted as if it were a real argv element ahead of the actual
`echo`/`Raku` program name, so `Command::new(&program)` tries to spawn a program literally
named `r\tTrue`, failing with `No such file or directory`.

Real Rakudo's `Proc::Async.new` signature has a slurpy `*%_` for named args it doesn't
otherwise recognize, silently absorbing them without corrupting the positional command.

## Minimal repro

```raku
my $proc = Proc::Async.new(:r, 'echo', 'Raku');
$proc.stdout.tap( -> $str { say "got: $str"; });
my $promise = $proc.start;
await $promise;
```

- `raku`: `got: Raku` (`:r`/etc. named args Proc::Async doesn't specifically use are simply
  ignored)
- `mutsu`: `Failed to spawn 'r\tTrue': No such file or directory (os error 2)`

## Affected files

- `src/runtime/methods_object_native_ctors_io.rs` — the `for arg in args` loop building
  `positional`/`attrs` for `Proc::Async.new` (around line 230-245). The catch-all `_ =>`
  arm needs to distinguish "an unrecognized *named* `Pair`" (absorb and drop, or store for
  future use) from "a genuine *positional* command/argv element" (push as today) — a
  `Pair` reaching this constructor should never be pushed into `positional` unless Raku's
  own `Proc::Async.new` signature treats bare positional pairs as literal argv strings
  (it does not).
