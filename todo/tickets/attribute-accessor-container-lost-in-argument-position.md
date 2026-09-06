# An `is rw` attribute accessor names a container for `:=` and for an lvalue invocant, but not in argument position

Measured 2026-09-06 against raku v2026.07 and a debug `mutsu` built from `main`
at `30d6754f5` (plus ADR-0067's E6 producer, which does not touch argument
position).

`class C { has $.v is rw }` gives `$c.v` a real Scalar in raku. mutsu now agrees
for two of the three consumers:

| Consumer | raku | mutsu |
|---|---|---|
| `my $x := $c.v; $x = 9` | `9` | `9` |
| `$c.v.snitch = 9` (ADR-0067 E6) | `9` | `9` |
| `sub g($y is rw) { $y = 9 }; g($c.v)` | `9` | **dies**: `Parameter '$y' expects a writable container (variable) as an argument, but got '42' (Int) as a value without a container.` |
| `sub f(\x) is raw { x }; f($c.v) = 9` | `9` | **`42` — silent, exit 0** |

The last row is the bad one: it is not a refusal, it reports success and drops
the write.

## Why this is a separate producer

ADR-0067's E6 producer emits `MarkAccessorRefContext` before the *invocant* of
an `__mutsu_assign_method_lvalue` call, so the container is produced only for
that one call shape. An ordinary argument goes through
`compile_call_arg_with_escape` (`src/compiler/helpers_call_args.rs`), whose
`is_bind_target` arm already marks a `:=` bind RHS the same way — but a plain
positional argument is not a bind target, so no marker is emitted and the
accessor read returns a value copy.

The narrow fix is presumably the argument twin of the E6 producer: mark an
argument-position accessor read when the callee's parameter is `is rw` / raw.
But unlike the invocant case, the callee's signature is not knowable at compile
time for an indirect call, and unlike the invocant case there is no
decontainerize chokepoint downstream to absorb a `ContainerRef` that nobody
consumes — so this needs its own measurement of where such a container would
flow before any code is written.

## ADR correction this row carries

ADR-0067's slice-3a subsection cites this exact program as evidence, saying raku
"dies with 'expects a writable container'". That is **mutsu's** diagnostic, not
raku's; raku answers `9`. The conclusion the row supported (E6 is a producer
question, and `.VAR` is not the discriminator) is unaffected, but the row itself
was misattributed. The ADR's non-goals section likewise describes the
`f($c.v) = 9` twin as "still copies", which understates it: it is a silent wrong
answer, not a conservative copy.

## Repro

```raku
class C { has $.v is rw }
sub f(\x) is raw { x }
my $c = C.new(v => 42);
f($c.v) = 9;
say $c.v;          # raku: 9    mutsu: 42
```
