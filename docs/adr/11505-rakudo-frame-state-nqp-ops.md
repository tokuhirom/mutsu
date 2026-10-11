# ADR-11505: Rakudo's frame-state `p6*` nqp ops

- **Status**: Accepted (2026-10-11). §2.1–§2.4 are implemented. §2.5 and §2.6 are decided
  but not yet implemented.
- **Date**: 2026-10-11
- **Issue**: [#11505](https://github.com/tokuhirom/mutsu/issues/11505) (campaign
  [#11488](https://github.com/tokuhirom/mutsu/issues/11488))
- **Related**: [#11496](https://github.com/tokuhirom/mutsu/issues/11496) (the call capture
  `nqp::usecapture` reads), [#12839](https://github.com/tokuhirom/mutsu/issues/12839) (that
  capture loses caller containers)

## 1. Question

Rakudo registers 26 `p6*` ops for the Raku HLL in `src/vm/moar/Perl6/Ops.nqp`. Seventeen
landed in #11558 and #11505's first slice. The other nine read or write state that a MoarVM
frame or code object carries and mutsu does not keep:

| Op | Rakudo state |
| --- | --- |
| `p6argvmarray` | the frame's incoming positional arguments |
| `p6bindsig`, `p6trybindsig` | "bind the frame's arguments to its signature" |
| `p6stateinit` | "this closure clone's `state` variables are being initialised" |
| `p6setfirstflag`, `p6takefirstflag` | a per-code-object `FIRST` flag |
| `p6setpre`, `p6clearpre` | a per-frame "inside `PRE`" flag |
| `p6staticouter` | the code object's static (compile-time) outer |

The issue asked which of these to implement and which are not applicable to mutsu. A cheap
answer would be wrong both ways. "Rakudo's compiler emits them, so user code never does"
would leave a working Raku program dying on `Unsupported nqp:: op`. And a frame record
filled on every call would charge every routine for ops almost no program uses.

## 2. Decision

All nine are implemented. None is not-applicable, because Rakudo accepts each one from
user code. Each op's behaviour is taken from what `raku -e 'use nqp; ...'` (2026.09) shows,
not from its name. Where Rakudo's answer depends on its own code generation or optimizer, we
match the observable rule and record the residue here. No op adds a per-call cost to code
that does not use it.

### 2.1 `p6argvmarray`: the recorded call capture

It answers the running call's positional arguments as a raw `BOOTArray`. A method's
invocant comes first, named arguments are left out, and slips arrive flattened. This is the
positional half of the capture `nqp::usecapture` already answers (#11496), so the op reads
that record:

- The compiler sets `CompiledCode::uses_capture` on a code object that mentions the op.
- The frameless call paths decline such code objects.
- The named-sub, closure and method binders store the call's `Capture` on the VM call frame.

A code object that does not mention the op pays nothing.

Implementing it found a bug: the TRIR entry (ADR-0110) binds no capture, yet it admitted
`uses_capture` routines. So `sub f($a, $b) { nqp::usecapture() }` answered an empty capture.
TRIR now declines such routines.

Known gap: the record holds argument *values*, while MoarVM holds what the caller passed. A
variable argument is therefore `Int` where Rakudo answers `Scalar`. This is #12839, and it
belongs to the shared record, not to this op.

### 2.2 `p6bindsig` / `p6trybindsig`: the bound-frame branch

Rakudo's code generation emits these to bind a frame's arguments on entry. Called again
from the body, they do not rebind:

```
$ raku -e 'use nqp; sub f($a is copy) { $a = 5; nqp::p6bindsig(); say $a; say nqp::p6trybindsig(); say $a }; f(1)'
5
1
5
```

mutsu binds every frame before its first statement runs. A failed bind raises before the
body. So wherever Raku code can evaluate these ops, the frame is already bound, and both take
Rakudo's bound-frame branch: `p6bindsig` answers null and `p6trybindsig` answers 1. This is
the whole behaviour reachable from Raku, not a stub. An unbound frame never runs user code
in mutsu.

### 2.3 `p6setpre` / `p6clearpre`: no Raku-reachable reader

They set and clear a frame flag whose only reader is NQP's `p6inpre`, which the Raku HLL does
not register. Both answer null. That null is everything a Raku program can observe, so
answering it is the implementation. (Rakudo 2026.09 segfaults on `say nqp::p6clearpre()`;
mutsu does not copy that.)

### 2.4 `p6stateinit`: lowered onto `state` variables

Rakudo answers 1 exactly when the frame's code declares `state` variables and this is the
first run of this closure clone. It answers 0 otherwise. The "frame" is the one Rakudo has
*after* its optimizer has inlined blocks:

- A code object is a frame of its own: a routine, a method, a block or pointy block used as
  a value (a `map` argument), `gather`, a phaser, or `CATCH`/`CONTROL`.
- A statement-level body is inlined into the enclosing frame: `if`, a statement-level
  `for`/`while`/`loop`, a bare block, `do`, `try`. The exception is a body that declares
  `state` variables itself, because Rakudo never inlines such a block.

mutsu keeps no frame flag. Instead, a parse-time pass (`parser/p6_stateinit.rs`, run only
when the source mentions the op) finds each op's frame by these rules. For a frame with
`state` variables, the pass prepends a hidden pair of variables to the frame's body:

```
state $__P6STATEINIT_SEEN__;
my $__P6STATEINIT__ = $__P6STATEINIT_SEEN__ ?? 0 !! ($__P6STATEINIT_SEEN__ = 1);
```

Each of the frame's ops then reads `$__P6STATEINIT__`. In a frame without `state` variables,
the op is the literal 0. The hidden `state` variable is cloned and initialised by the same
machinery as the frame's own `state` variables. So the op agrees with them by construction,
whatever mutsu's per-clone rule is.

Rejected: a flag set on every frame entry, which charges every call, and a check on whether
some user `state` variable is already initialised, which is wrong when a `return` skips the
declaration on the first run.

Residue: Rakudo's optimizer also inlines a statement-level `for 1 { }` over one constant
element. These rules treat that as an inlined body, which agrees with Rakudo's answer.

### 2.5 `p6setfirstflag` / `p6takefirstflag` (decided, not yet implemented)

`p6setfirstflag($do)` sets a flag on a code object. Like Rakudo, it requires the bytecode
handle (`nqp::getattr(&f, Code, '$!do')`, not `&f`). `p6takefirstflag()` reads and clears
the flag of the running frame's code object, with the frame found as in §2.4. The flag
belongs to the closure clone, so it is kept per clone identity, in the subsystem that owns
code objects (never a new `Interpreter` field).

Not mirrored: Rakudo's `for` loop sets the flag on a loop body that has a `FIRST` phaser, so
a `p6takefirstflag()` inside that body answers 1 on the first iteration. A `while` loop does
not. mutsu compiles `FIRST` its own way, and this code-generation artifact shows only when a
program reads the flag inside such a loop.

### 2.6 `p6staticouter` (decided, not yet implemented)

`p6staticouter($do)` requires a `$!do` handle. It answers the `$!do` of the code object the
given code was declared in: its static outer, which is `<unit>` at the top level of a
program. Implementing it needs a code object to record its static outer when it is compiled.

## 3. Consequences

- `docs/nqp-op-coverage.md`: Rakudo p6* goes from 17 / 26 to 23 / 26 with §2.1–§2.4.
- `nqp::usecapture` in a routine TRIR used to admit now gets its capture.
- No per-call cost for code that does not use these ops: the capture is recorded only for
  flagged code objects, and the `p6stateinit` pass runs only when the source mentions the op.
