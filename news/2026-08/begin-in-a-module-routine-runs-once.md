# A `BEGIN` inside a module routine runs once, not once per execution

`BEGIN <expr>` promises exactly one evaluation. mutsu delivered that only for
the mainline: `reorder_phasers` lifts a `BEGIN` rvalue out of the enclosing
closure and hoists it to the top of the compilation unit, but that pass is run
from `run_program` alone — a module's body goes straight to `run_block`. So
every `BEGIN` inside a module's routines was compiled inline and re-evaluated
on **every execution of the surrounding code**.

The cost is not subtle when the BEGIN sits in a loop body. `Digest::SHA2`'s
compression round reads its round constant from

    (BEGIN map { frac $_ **(1/3), 32 }, @primes[^64])[$j]

so mutsu rebuilt the whole 64-word table — 64 cube roots, each a `FatRat`
`**(1/3)` — once per round, i.e. 4096 times per 64-byte block. A single
`sha256("abc")` executed 65069 opcodes, 4096 of them the `1/3` division alone.

## The fix

A `BEGIN` that reaches the compiler is one the lifter left in place, so the
compiler now emits `OpCode::BeginOnceExpr` for it: the body runs at most once
and its value is memoized in the same `once` store that `once { … }` uses. The
value is computed at first use rather than hoisted, which is *safer* than
hoisting here — a hoisted BEGIN would run before the module-level `constant`
declarations it reads (`@primes` above), which is exactly why lifting the
module case was not the fix.

The memo cell's key is the interesting part. `once` keys on the op's bytecode
position plus the enclosing routine's clone id, but a BEGIN needs to survive
something stronger: a block handed to a builtin that runs it through the AST
carrier — `reduce`, `classify`, `categorize`, … — is **recompiled on every
call** (`call_sub_value` → `eval_block_value` → `Compiler::compile`), so any id
minted during compilation is fresh each time and the memo would never hit. That
is precisely the SHA2 shape: the round function is a `reduce` callback. So the
site id is hashed from source identity instead — declaring package, source
line, and a fingerprint of the BEGIN body — which is stable across
recompilations. Two textually identical BEGINs on one line are told apart by an
occurrence counter, so they keep separate cells.

## Result

The `Digest` distribution's own test suite, on a release build:

| file | before | after | raku |
| --- | --- | --- | --- |
| `t/md5.t` | 42.5s | 2.2s | 1.0s |
| `t/sha.t` | 97.9s | 1.5s | 1.9s |
| `t/rfc4231.t` | >200s (timeout) | 5.9s | 2.4s |
| `t/ripemd.t` | >200s (timeout) | 512.9s | 46.0s |

`sha256("abc")` drops from 65069 opcodes to 23248. Three of the four files now
run comfortably faster than the batteries gate's per-file budget; `t/ripemd.t`
completes but is still 11x raku, which is a separate (unrelated) gap.

Pin: `t/begin-once-in-module-routine.t`, which checks all three properties
against a module fixture — one array shared by repeated calls, one array shared
by every iteration of a `reduce` callback, and two same-line BEGINs staying
distinct. Every assertion passes under rakudo too.
