# `nqp::create` remembers how each class is allocated

`nqp::create` and `.CREATE` now remember, per class, how an instance of that
class is allocated (ADR-0121 D3, #9291). On the untyped VM, an
`nqp::create(Bareword)` site also resolves its bareword once per registry
write generation instead of on every call.

## What changed

- **One implementation.** The body of `nqp::create` moved out of the generic
  op table into `src/runtime/nqp_create.rs`. The generic `NqpOp`, TRIR's
  `NqpOpGen` and the new opcode all call it. `.CREATE` on a registered class
  (`dispatch_create`) builds from the same code.
- **The allocation kind.** Which allocation a type gets is a pure function of
  its name and the registry:
  - an empty codepoint store for `Uni` / `NFC` / ...;
  - mutsu's own hash or array for a bare `VMHash` / `VMArray` class;
  - `.new` for native arrays, Buf/Blob and the built-in containers;
  - otherwise the slot template.

  Deciding this used to cost a short-name derivation and two string-keyed
  registry probes on every call. It is now memoized per type (`CreateMemo`).
- **The slot template.** The seeded slot template and whether the class
  needs an associative backing store (`is Hash` / `is Map`) are memoized
  together. Before, every call did a constructor-plan lookup plus an MRO walk
  that resolved each ancestor's name.
- **The class operand.** On the untyped VM, `nqp::create(IB)` compiles to
  `OpCode::NqpCreateC`. It carries the same `ClassOperandSite` TRIR's
  bareword terms use: the site remembers a type object named by the
  bareword's own spelling for one registry write generation. Any other
  operand keeps the generic op.
- **An instance operand.** `nqp::create($instance)` now creates the
  instance's class, as MoarVM does. Before, it died with "No such method
  'CREATE'".

Both memos are keyed on the registry write generation. Every registry
mutation bumps it, so a declaration made at run time (an `EVAL`'d subclass,
a MOP `^add_attribute`) is never answered from a stale entry.

## Measured

Callgrind instructions per `nqp::create(IB)`, taken from the difference
between 10 and 5,010 iterations, on profiling builds of `main` and this
branch:

| where | before | after |
|---|---:|---:|
| untyped VM, `$s = nqp::create(IB)` above `$s = IB` | 5,134 | 739 |
| TRIR routine, above the empty `nqp::while` loop | 3,882 | 2,278 |

What remains in TRIR is mostly the allocation itself: the instance, its slot
vector, and freeing both. #9291's close condition (every row within 2x of
rakudo) is not met yet.
