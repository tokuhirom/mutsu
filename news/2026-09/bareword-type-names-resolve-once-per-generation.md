# A bareword type name resolves once per registry generation

A slice of [ADR-0121](../../docs/adr/0121-instance-attributes-live-in-per-class-slots.md) D3
(#9291). `P` in `P.new(...)`, `Int` in `$x ~~ Int` and every other bareword
term compiles to `OpCode::GetBareWord`. That opcode ran the whole
term-resolution chain on every execution: env probes, the enum namespaces,
the imported-routine check, the type registry. That cost ~2,150 instructions
for a plain class name. rakudo resolves the name at compile time.

## What changed

- **A per-site memo.** A chunk now keeps one memo per string constant
  (`BarewordSiteCaches`). When a `GetBareWord` resolves to the type object of
  its own spelling, the site remembers it for one registry write generation.
  A later execution under the same generation pushes the type object
  directly.
- **The `env` guard.** A memo is written, and used, only while `env` binds
  nothing under the name, or binds that same type object (a class
  declaration binds its own name that way). A type capture
  (`sub f(::T $x) { T }`) and a role's type parameter rebind the name in
  `env` without writing the registry, so the generation alone cannot see
  them.
- **Spellings rewritten from bindings are never remembered.** These are
  parameterised names (`Box[T]`) and smileys on a bound parameter (`T:D`).
- **Per-interpreter generation ranges.** Each interpreter's registry write
  generation now starts in a range of its own
  (`Interpreter::fresh_registry_write_gen`). A thread's interpreter shares
  compiled chunks with its parent but works on a snapshot of the registry,
  so a memo written by one interpreter must never look current to another.

## A bug the guard also fixed

TRIR's `ClassOperandSite`, used by `nqp::create(Bareword)`, remembered its
term without that `env` guard. So after
`class T { }; sub f(::T $x) { nqp::create(T) }`, the first call `f(T.new)`
fixed the answer, and `f(42)` kept creating a `T` instead of an `Int`. The
site now applies the same guard.

## Measured

Callgrind instructions per loop iteration (`--profile profiling`, a 10,000
iteration difference, second run after each build):

| loop body | before | after |
|---|---:|---:|
| `$s = $i` (baseline) | 5,849 | 5,849 |
| `$s = P` | 6,739 | 4,792 |
| `$s = P.new(x => $i, y => 2)` | 17,447 | 15,500 |

A memo hit costs ~190 instructions, most of it the `env` probe.
