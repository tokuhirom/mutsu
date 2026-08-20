# The fast binder for simple positional-only signatures skips the "Too many positionals" check

Found while investigating `todo/deep/blind-slip-flattening-in-fixed-arity-calls.md`
(now [ADR-0054](../../docs/adr/0054-argument-list-interpolation-is-a-call-site-property.md)).
Independent of that ADR, and small enough to fix directly.

## Symptom

Passing more positional arguments than a plain sub's signature accepts is
silently ignored — the extra arguments are dropped instead of raising
`X::AdHoc: Too many positionals passed`. Verified on `main` @ `b1a9bb8a5`
(debug build).

```raku
sub g($a) { say "g:", $a }
my @z = (1, 2);
g(|@z);      # raku: dies "Too many positionals passed; expected 1 argument but got 2"
             # mutsu: prints "g:1"

sub g2($a, $b) { say "g2:", $a, $b }
g2(1, 2, 3); # raku: dies;  mutsu: prints "g2:12"

my $b = -> $a { say "blk:", $a };
$b(1, 2);    # raku: dies;  mutsu: prints "blk:1"
```

(Use the `|@z` form to probe it: with literal extra arguments, `raku` catches it
at *compile* time — "Calling g(Int, Int) will never work with declared
signature ($a)" — which is a separate check mutsu also lacks. The runtime check
is the one this ticket is about.)

## What works, and what that tells us

The check is not missing from the codebase — it is in the general binder, at
`src/runtime/types/binding_signature.rs:2318-2342` ("Check for extra positional
arguments when no array/capture slurpy is present"). It fires correctly whenever
something forces a call onto that binder:

| Callee shape | Extra positionals |
|---|---|
| `sub g($a)` | **silently dropped** |
| `-> $a { }` block | **silently dropped** |
| `sub g($a, :$k)` (any named param) | correctly dies |
| `sub g($a where * > 0)` (any constraint) | correctly dies |
| `multi g($a)` | correctly dies (`X::Multi::NoMatch`) |
| `method m($a)` | correctly dies |

So the gap is confined to the **fast positional-only binding path** — the one a
simple `sub`/block signature takes (the light-call / OTF / `call_compiled_function_fast`
family in `src/vm/vm_call_func_ops.rs` and `src/vm/vm_closure_dispatch.rs`),
which binds `min(params, args)` slots and returns without comparing the counts.

## Fix sketch

Add the count comparison to the fast binder before it binds: if the signature
has no array/capture slurpy and no positional sub-signature, and the argument
count exceeds the positional parameter count, raise the same
`Too many positionals passed; expected {n} arguments but got {m}` error the
general binder produces (keep the message byte-identical — `main_args.rs:466`,
`calls.rs:423`, `builtins_lvalue.rs:197,228` and
`methods_instance_ops.rs:306` all match on it as a string). The cheapest form is
a compile-time flag on `CompiledCode` ("signature is fixed-arity positional-only"),
so the fast path pays one integer compare rather than re-deriving slurpiness per
call.

## Why it matters beyond tidiness

It masks over-supply bugs elsewhere. ADR-0054 §2.2 hit exactly this: mutsu's
blind Slip flattening turns `g((1,2).Slip)` into a two-argument call to a
one-parameter sub, and instead of the loud arity error raku gives, mutsu prints
`g:1` — the corruption is invisible. Any campaign that changes argument-list
construction is flying blind in one direction until this check exists.

## Test

`t/too-many-positionals-fixed-arity.t`: the table above, one case per callee
shape, each wrapped in `try`/`CATCH` asserting the message. Expect fallout in
`t/` and roast from calls that currently over-supply silently — that is the
point of the ticket.
