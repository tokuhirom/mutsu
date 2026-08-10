# A bare `when` inside a `.tap: { ... }` callback leaks `Control::Succeed` and kills the process

## Repro

```raku
class C {
    has Supplier $.s = Supplier.new;
    submethod TWEAK() {
        $!s.Supply.tap: {
            when Int { say "int: $_" }
            when Str { say "str: $_" }
        };
    }
}
my $c = C.new;
$c.s.emit(42);
say "after emit";
```

- mutsu: prints `int: 42` then `Runtime error: ` (empty message) and exits 1.
- raku: prints `int: 42` then `after emit`.

Confirmed on `main` (66738c7ca), independent of any in-flight ticket work —
not a regression from a specific PR.

## Diagnosis so far

gdb backtrace at `main.rs`'s `print_error` shows the escaping `RuntimeError`
carries `control: Some(Control::Succeed)` and no `exception`/message — i.e. an
unconsumed `succeed` signal from the `when`'s implicit end-of-block succeed,
reaching all the way to the top of `run_main` uncaught. A `.tap: { when ... }`
callback block must be its own topicalizer scope for a bare `when` (the same
way a `given`/method/sub body is), consuming the `succeed` signal at the
callback's own frame boundary — it currently does not when the callback is
invoked via the emit->tap dispatch path (as opposed to, e.g., a directly
`.tap`-ed callback with no surrounding TWEAK/registry interaction — not yet
narrowed further).

## Next step

Narrow whether ordinary (non-`submethod TWEAK`) `.tap: { when ... }` also
leaks, or whether TWEAK/instance-attribute context is required to trigger it;
then find the frame boundary that already does this for plain
subs/methods/given (compare with `vm_control_ops.rs` / `vm_closure_dispatch.rs`
"is_succeed()" handling) and figure out why a tap callback frame doesn't hit
that same catch.
