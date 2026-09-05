# The invocant arrives as a container

`my $a = 42; $a.snitch = 5` now prints `42` and leaves `$a` holding `5`, as it
does in raku. So does the user-written twin,
`augment class Any { method mysn(\SELF:) is raw { SELF } }; $a.mysn = 5`, and so
do the element spellings `@a[0].mysn = 9` and `%h<a>.mysn = 9`. Before this,
every one of them died with `X::Assignment::RO: cannot assign through .snitch on
non-instance`.

This is slice 3a of
[ADR-0067](../../docs/adr/0067-a-routine-hands-back-the-container-it-was-given.md),
whose one rule is that a routine hands back the container it was *given* — and
the invocant is parameter zero. Slice 1 taught a sigil-less name to denote its
container on the way *out* of a routine; slice 2 gave methods the same
rw-capability oracle a `sub` already had. What was still missing was the inbound
direction for parameter zero: the invocant arrived as a value, so there was no
location for the assignment to write through.

## The contract, and why both halves matter

raku requires two independent declarations before `$a.m = 5` writes through `$a`:
the invocant parameter must be raw (`\S:`, `$s is raw:`, `$s is rw:`) *and* the
routine must be rw-capable (`is rw`, `is raw`, or spelling `return-rw`).
Dropping either keeps the assignment a refusal — `method m(\S:) { S }` is
`Cannot modify an immutable Int` and `method m(Any:D $s:) is raw { $s }` is
`Cannot assign to a readonly variable or a value`. Both refusals are now pinned
as regression controls, so a future change cannot buy one half by giving up the
other.

That contract lives in exactly one place, `Interpreter::method_returns_raw_invocant`
(`src/runtime/raw_invocant.rs`), and both consumers read it: the VM gate that
decides whether to box the invocant, and the runtime branch that consumes the
box. Keeping them on one function is not tidiness — a VM that boxed an invocant
the runtime then refused to consume would hand a `ContainerRef` to the ~40
`Instance`/`Array`/`Hash` branches of `assign_method_lvalue_with_values` and
silently skip all of them.

## Where the box happens, and why it could not go anywhere else

The invocant of `$a.snitch = 5` was already tagged with its source name
(`GetLocal(0); ContainerizePair; WrapVarRef{name_idx, slot}`); the missing step
was the shared-cell box that a `return-rw` tail emits right after such a tag.
It could not simply be emitted by the compiler, because rawness is not
statically known — the callee depends on the invocant's runtime type and, for
the dynamic spellings, on a runtime method-name string. It could not be done
inside the runtime entry either, because the slot resolution needs the frame's
`&CompiledCode`, which that entry does not have. So the box is a runtime-gated
step in the VM (`src/vm/vm_raw_invocant_lvalue.rs`), at the one site where the
frame's code and the resolved callee are both in hand.

The element spellings needed no per-shape code, but for a subtler reason than
"container mode handles them": `@a[0].m = 9` already compiles to a copy-in /
copy-out protocol through a compiler temp, so boxing *the temp* is enough — the
existing tail reads the name back through `GetGlobal` (which dereferences a
container) and writes it into the element. That temp is a global name rather
than a frame local, which is the one open question slice 3a inherited. The
answer chosen was a global-name container route rather than promoting the temps
to locals: the temp is read back by two separate opcodes, so promoting it would
touch the whole temp protocol for every lvalue method call, while an env cell is
transparent to both.

## The ordering rule the first attempt got wrong

The lookup that finds the invocant's container tries four routes, and the rule
that orders them was learned the hard way: **reusing an existing location must
always come before minting one.** The first ordering minted an env cell whenever
the name had no frame slot to box — which silently broke
`for @a -> $e is rw { $e.mRaw = 3 }`, a shape that had *refused loudly* before
the slice. An `is rw` loop parameter binds the source element's own promoted
cell, and the loop then suppresses its end-of-iteration writeback precisely
because that alias carries the write; minting a second cell over the top of it
dropped the write on the floor. The fix is the env-side twin of a check the
local-slot path already had, and both loop spellings (`-> $e is rw` and
`<-> $e`) are now pinned, so the ordering cannot regress unnoticed.

## Paying for the gate

The gate runs on every `$obj.attr = v`, and measuring it (a same-binary
env-switch A/B on a release build) put the first version at **+13%** on a tight
attribute-assignment loop. The fix is a set-only `any_raw_invocant_method` flag
the registry raises when a method with a raw invocant is declared, checked in
the VM gate *ahead of every allocation* against a borrowed method name. That
placement matters: putting the same flag inside the oracle recovered almost
nothing, which located the real cost — most of the 13% was the argument
extraction the gate did before it could even ask, not the method resolution
everyone would have blamed. The flag and the oracle read the same predicate, and
a `debug_assert` re-derives the slow answer whenever the flag declines, so the
two cannot drift apart unnoticed.

## Two things the measurement changed

The ADR proposed a native raw-invocant table of `.snitch`, `.item` and `.list`.
Measured against raku, only `.snitch` belongs. `$a.list =:= $a` is `False` and
`.list` returns a `List`; `$a.list = 7` does reach `$a`, but by *list assignment*
into a List whose one element is the invocant's container — a different
mechanism, which a table row would have silently replaced. `.item` is genuinely
raw, but the compiler erases `$a.item = 5` to a plain store, so the row would
never be read. (That erasure is sound only because `.item` is pure; `.snitch`
notes its invocant, which is exactly why it cannot take the same route.)

The slice also closed a silent wrong answer nobody had listed:
`class C { method m(\S:) is raw { S } }; my $c = C.new; $c.m = 5` is `5` in raku,
because the raw invocant is the *variable's* container and the write replaces
its whole contents. mutsu reported success and dropped the write. It now
answers `5`.

## What is still refused

`class C { has $.v is rw }; $c.v.snitch = 9` still refuses, and the measurement
says why it is not part of this slice: that spelling compiles with no temp and
no writeback tail at all — the invocant is a bare accessor call and there is no
name to box. The producer it needs already exists (`MarkAccessorRefContext`, the
op that makes `my $x := $c.v; $x = 9` write through today), but wiring it into an
lvalue invocant is an unconditional compile-side change and earns its own slice.
Aggregate invocants (`@a.snitch = (7,8)`) and chained ones
(`$a.snitch.snitch = 5`) likewise still refuse — loudly, which is the status quo,
not a new regression.

Pinned by `t/raw-invocant-lvalue-container.t` (29 tests) and
`t/snitch-lvalue-raw-invocant.t` (12 tests), both byte-identical under `mutsu`
and `raku`.
