# `my $tap = whenever <Supply> -> $x {...}` inside a react block binds `$tap` to the literal string `"whenever"`

Found while writing a regression test for
`todo/tickets/supply-lines-drops-channel-backed-supplies.md` (now resolved; see
`news/2026-08/supply-lines-channel-backed-tap-fix.md`). Unrelated to that fix —
this is a separate, general bug in `whenever`'s target-var binding.

## Repro

```raku
my $s = Supplier.new;
react {
    my $tap = whenever $s.Supply -> $x {
        say "got $x";
    }
    say "tap type: {$tap.WHAT.raku}";
    say "tap raku: {$tap.raku}";
    $s.emit(1);
    whenever Promise.in(0.2) { done }
}
```

Expected: `$tap` is a `Tap` instance (so e.g. `$tap.close` works later).

Actual (current `main`, verified against a fresh `cargo build` debug binary):

```
tap type: Str
tap raku: "whenever"
```

`$tap` is bound to the literal 9-character string `"whenever"` — not even a
plausible malformed Tap, but what looks like a stray string constant leaking
through. Reproduces for the simplest possible live source (a plain
`Supplier.new.Supply`), so this is not specific to `IO::Socket::Async::Listener`
or any other special-cased class — every `my $x = whenever <live Supply> {...}`
inside a `react { }` block is affected. (A `whenever` *without* the `my $x =`
binding runs its body normally — only the bound handle is wrong.)

## Where to look

- `src/vm/vm_scope_ops.rs`'s `exec_whenever_scope_op` — computes `target_var`
  from `target_var_idx` and, after `run_whenever_with_value` returns, reads the
  bound name back out of `self.env()` into the caller's local slot. Verify
  what `self.env().get(name)` actually holds at that point.
- `src/runtime/subtest.rs`'s `run_whenever_with_value` — in the "in react mode"
  branch (`self.supply_emit_buffer` non-empty or `self.react_active > 0`), the
  generic (non-Listener) case does `self.env.insert(name.to_string(),
  supply_val)` — i.e. it binds `$tap` to the **source Supply**, not a Tap at
  all (only the special-cased `IO::Socket::Async::Listener` branch calls
  `.act()` and binds the real Tap it returns). That still wouldn't explain a
  *string* `"whenever"`, so something upstream (parser/compiler emitting
  `target_var_idx`, or a stray constant-pool entry) is also worth checking —
  use `--dump-bytecode` on the repro above and `rust-gdb -batch` breakpoints
  on `exec_whenever_scope_op` per `CLAUDE.md`'s debugging guidelines, rather
  than guessing.

## Why this is filed separately

Not blocking the `.lines`/channel fix (that fix does not rely on this
mechanism at all — the regression test added for it gets its port via a plain
top-level `.tap()`, not `my $tap = whenever`). But it is a real, general,
likely high-impact bug: any code that needs to hold onto a live `whenever`'s
`Tap` handle from inside a `react`/`supply` block (e.g. to `.close()` it
early) is currently getting garbage instead.
