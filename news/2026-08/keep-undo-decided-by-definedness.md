# KEEP/UNDO are decided by the trailing value's definedness, not truthiness

`KEEP`/`UNDO` phasers (and `LEAVE`'s success/failure queue split) used to
decide which queue to run based on the TRUTHINESS of the block's trailing
value:

```raku
my $s = "";
{ KEEP { $s ~= "K" }; UNDO { $s ~= "U" }; 0 }
say $s;   # raku: K   mutsu (before): U
```

Real Raku's actual rule, confirmed by probing several shapes against `raku`
directly, is **definedness**, not truthiness or even "did the block complete
normally":

- `0`, `False`, `""`, `()` — falsy but DEFINED — run **KEEP**.
- `Any`, `Nil`, an undefined `Failure`, or a phaser-only block with no value
  statement at all (implicit `Nil`) — run **UNDO**.
- `return 0` from a named routine also runs KEEP (a defined return value);
  `last`/`next` (whose implicit `return_value` is `None`, read as undefined
  `Nil`) run UNDO.
- An actual exceptional exit (a thrown exception or `fail`) always runs
  UNDO, regardless of any value.

`should_run_success_queue` (`src/vm/vm_misc_block.rs`) and its tree-walk-era
twins `should_run_success_queue_raw`/`should_run_success_queue_vm`
(`src/runtime/run.rs`) now check `crate::runtime::types::value_is_defined`
(the same structural definedness check used by `//`/`andthen`/`orelse`)
instead of `Value::truthy`.

A pre-existing, narrower, separate bug was found along the way and filed
separately: a loop body's `KEEP`/`UNDO` phasers never run at all (not even
the wrong queue) when the iteration exits via `last`/`next` — see
`todo/tickets/loop-body-keep-undo-not-run-on-last-next.md`.

Pinned by `t/keep-undo.t` and `t/enter-phaser-rvalue.t` (both extended with
the falsy-but-defined and undefined-value cases, verified byte-identical
against real `raku`'s TAP output).
