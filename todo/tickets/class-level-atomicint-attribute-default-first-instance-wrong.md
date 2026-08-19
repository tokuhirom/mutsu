# A class-level `my atomicint` read via `⚛++` inside an attribute default value is wrong for the first instance, and off-by-one for the rest

Found 2026-08-20 while verifying ADR-0035 Slice 3's acceptance gate
(`t/logging.rakutest` from `modules/Log-Timeline/`, tests 10-30). Unrelated to
ADR-0035 (caller-frame observation) — a separate, narrower bug surfaced only
because Slice 1+2 let execution reach further into `Log::Timeline`'s real
code path (previously `PROCESS::<$LOG-TIMELINE-OUTPUT>` read as `Nil`, so the
`else`/no-op branch was taken and this code never ran).

## Repro

```raku
class Foo {
    my atomicint $current-id = 1;
    has $.id = $current-id⚛++;
}
my $a = Foo.new;
my $b = Foo.new;
say $a.id;
say $b.id;
```

- raku: `1` then `2` (correct atomic post-increment starting at 1).
- mutsu: `(atomicint)` then `1` — the first instance's attribute prints as
  the bare `atomicint` type object (not a value at all), and the second
  instance gets the value the *first* instance should have gotten.

## Isolation (each dimension tested independently against raku as oracle)

- Plain `my Int $current-id` (non-atomic) + `has $.id = $current-id++`:
  **works correctly** (`1`, `2`) — rules out "class-level `my`-var read from
  an attribute default" in general.
- `my atomicint $x` + `$x⚛++` used standalone (not inside a class/attribute
  default): **works correctly** (`1`, `2`, `3`) — rules out the `atomicint`
  type or the `⚛++` postfix operator in isolation.
- Native `int $.id` attribute with a non-atomic default expression: **works
  correctly** — rules out native-typed attributes in general.
- The bug requires the exact combination: a class-BODY-level `my atomicint`
  variable, read via `⚛++` inside an attribute's default-value expression,
  evaluated at instance-construction time. Reproduces identically whether the
  attribute itself is native (`has int $.id`) or not (`has $.id`).

## Hypothesis (not yet confirmed against the compiler internals)

The "(atomicint)" printed for the *first* instance is literally the type
object's display form — suggesting the attribute-default-value closure reads
`$current-id` before the class-body's `my atomicint $current-id = 1;`
initializer has actually run against the SAME cell the closure captured, on
the very first evaluation only. Subsequent instantiations then read/increment
correctly relative to each other but are shifted by one (the second instance
gets what should have been the first instance's value), consistent with the
first read having silently no-op'd (or read-then-discarded) rather than truly
failing. Needs a `rust-gdb` investigation of how attribute default-value
closures capture class-body `my` lexicals, and whether there is a class-body
statement-ordering issue specific to `atomicint`-typed `my` declarations (vs.
plain `Int`, which is unaffected).

## Why this matters

Blocks `modules/Log-Timeline`'s `t/logging.rakutest` tests 10+ (task
start/end logging), because `Log::Timeline::Ongoing::Logged`
(`modules/Log-Timeline/lib/Log/Timeline/Model.rakumod:13-30`) uses exactly
this pattern for its unique task-ID counter:

```raku
class Log::Timeline::Ongoing::Logged does Log::Timeline::Ongoing {
    my atomicint $current-id = 1;
    has int $.id = $current-id⚛++;
    ...
}
```

The wrong first-instance ID is a `Package` (not the `atomicint` type object
directly — worth re-checking the exact printed type in the real module
context vs. this minimal repro) that then fails a downstream `Int $id`
parameter type check in `FakeOutput.log-start`, aborting the test file with
`X::TypeCheck::Binding::Parameter`.

## Affected files (starting points, not confirmed)

- Compiler: wherever attribute default-value expressions are compiled as
  per-instance-construction closures/thunks (likely `src/compiler/` class
  registration path).
- VM: `my atomicint` class-body variable registration and the `⚛++` postfix
  atomic-increment opcode handling.
