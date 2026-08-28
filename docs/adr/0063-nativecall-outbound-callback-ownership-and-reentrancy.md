# ADR-0063: A NativeCall callback is a process-lifetime closure that re-enters the calling VM

- Status: Accepted (implemented)
- Date: 2026-08-28
- Supersedes: nothing
- Related: [ADR-0015](0015-native-backed-container-storage-and-repr-bodies.md) (native-backed
  container storage — a callback's `CArray`/`Buf` arguments are pointers into that storage),
  [ADR-0012](0012-libffi-macos-arm64-vendored-bump.md) (the vendored libffi this builds on),
  `todo/tickets/nativecall-callback-parameter-marshalling.md` (the finding this ADR closes)

## Context

`Language/nativecall.rakudoc`'s "Function arguments" section documents a native function that
takes a function pointer:

```raku
use NativeCall;
# void SetCallback(int (*callback)(const char *))
my sub SetCallback(&callback (Str --> int32)) is native('mylib') { * }
```

mutsu had the *inbound* half of this — `nativecast(:(...), $ptr)` turns a C function pointer
into a callable Raku routine (`src/runtime/nativecall_fnptr.rs`) — but not the outbound half.
A `&callback (Sig)` parameter had no C type at all, which made
`register_native_call_routine` skip native registration for the whole declaration; the sub then
kept its `{ * }` Raku body and the call fell into ordinary signature binding, where the
callback's own parameter list was (wrongly) matched against the passed `Sub`:

```
Calling qsort(Any, Int, Int, Sub) will never work with declared signature (CArray[int32], size_t, size_t, &cmp)
  X::TypeCheck::Argument: Type check failed for __type_only__: expected Pointer, got Sub
```

Two decisions have to be made before any of this can be implemented, and neither is readable
off the code — which is why they are recorded here rather than being baked in silently.

### Decision 1 — who owns the C closure, and for how long

libffi's `middle::Closure` owns an executable trampoline, its CIF, and a userdata pointer. If
the `Closure` is dropped, the trampoline's memory is freed. But C routinely **retains** a
callback past the call that installed it: `archive_write_open(a, data, open, write, close)`
stores all three for the lifetime of the archive handle, and every `archive_write_data` call
afterwards runs them. A closure whose lifetime is that of the installing call would leave C
calling freed memory on the very next call — a use-after-free with no diagnostic.

Nothing in the Raku program tells mutsu when C is done with a callback. There is no `unregister`
in the NativeCall surface, and Rakudo does not free them either.

### Decision 2 — how the callback body reaches an `Interpreter`

The trampoline is called by C, on whatever thread C chose, from a stack frame that has no Rust
context at all. It receives only its userdata. To run the Raku body it needs a live `&mut
Interpreter`, and mutsu has no ambient one: the VM is an ordinary `&mut self` threaded through
the call stack.

Three candidate mechanisms:

1. **Thread-clone an interpreter per closure** (`clone_for_thread`), stored in the userdata.
   Works from any thread, but the callback then runs against a *snapshot*: a comparator that
   increments an outer counter, or a libarchive write callback that appends to an outer buffer,
   would silently mutate a copy nobody reads. It is also expensive per closure.
2. **A global "the main interpreter" pointer.** Simple, but wrong the moment a native call is
   made from a spawned thread — the callback would run on a different thread's interpreter,
   racing it.
3. **A thread-local stack of the interpreters currently inside a native call.** The callback
   runs on exactly the VM that made the call that reached C, on the same thread, sharing all
   state. It covers the callback invoked *during* a native call — including from a nested one,
   which is the libarchive shape — but not one C fires later, or from a thread of its own.

## Decision

**Ownership: a marshalled callback is a process-lifetime object, interned by (callable identity,
C signature).**

The libffi closure, its CIF and its userdata — which holds a strong reference to the Raku
`Callable`, keeping it alive — are `Box::leak`ed and never freed. This is the only policy that
is correct for a callee that retains the pointer, and it matches what this subsystem already
does for exactly the same reason: `load_library_cached` leaks every `dlopen` handle (a
`dlclose` would invalidate every pointer obtained through it) and `native_object_where` leaks
its `.WHERE` blocks.

The leak is **bounded by interning**: `callback_code_address` keys a process-lifetime table by
the callable's identity (a `Sub`'s GC address, or a `Routine`'s `package::name`) plus the
`CallbackSig`, so passing the same sub to the same native sub in a loop allocates one closure,
not one per call. Holding the `Value` in the leaked userdata is what makes the *address* half of
that key sound: the allocation can never be freed, so its address can never be reused by a
different sub (the failure mode recorded in `trap-cache-key-must-hold-arc-not-bare-pointer`).

An **undefined** argument (a type object / `Nil`) marshals to a genuine NULL function pointer
rather than a closure — that is how a C API's "no handler for this hook" is spelled.

**Re-entrancy: mechanism 3 — a thread-local stack of active interpreters.**

`call_native_with_out_args` takes the calling `&mut Interpreter` and, for exactly the duration
of the libffi call, pushes it onto a thread-local stack (`InterpreterGuard`, RAII). The
trampoline reads the innermost entry and re-borrows it to run the Raku body. Between pushing
the pointer and dropping the guard, `call_native_with_out_args` does not touch `interp` again,
so the callback's re-borrow is the only live access — which is what keeps the raw-pointer
round trip sound rather than an aliasing violation.

Consequences, accepted deliberately:

- A callback fired **during** a native call works, shares all interpreter state, and sees the
  caller's lexicals. This is the case every real binding needs (`qsort`'s comparator, and
  libarchive's read/write/close callbacks, which fire from `archive_write_data` — itself a
  native call, so the stack is non-empty).
- A callback C fires **later**, or from a thread mutsu never entered FFI from, finds an empty
  stack. It is reported on stderr and returns a zero-valued result rather than crashing. This
  is a real limitation, and it is the honest one: there is no interpreter on that thread to
  re-enter, and inventing one would silently run the body against state nobody can observe.
- A Raku exception or a Rust panic in the callback body is **caught** and reported, and a zero
  result is returned. Unwinding through the C frame is undefined behaviour, and the language
  documentation already says so: "It is not allowed to throw an exception out of a native
  callback, and doing so will lead to process termination."

### Signature recording

Separately from the two decisions above, the parser had to record the parameter correctly. A
`&`-sigilled parameter followed by `(...)` is a constraint on the *Callable's own signature*
(`Language/signatures.rakudoc`, "Constraining signatures of Callables"; NativeCall's spelling
just drops the `:` of `&cb:(...)`). mutsu was recording it as a **destructuring**
`sub_signature`, i.e. as an instruction to unpack the argument — which is why the callback's
`Pointer` parameter became a type constraint checked against the passed `Sub`. It is now
recorded as the parameter's `code_signature`, parsed with `parse_param_list_with_return` so the
`--> T` survives: that return type is the callback's C return type and nothing else carries it.

Rakudo *parses* the same spelling as a sub-signature and lets its NativeCall trait handler
reinterpret it, which is why a plain (non-native) Raku sub with this parameter dies with
"Cannot unpack or Capture &f". mutsu is therefore slightly more permissive on the non-native
path — it accepts the constraint rather than erroring — which is a benign divergence and not
worth a second recording mechanism.

## Rejected alternatives

- **Reference-count the closure against the Raku `Callable`'s liveness.** Attractive (no leak),
  but unimplementable: C's retention is invisible to Raku, so the Callable going out of scope
  says nothing about whether libarchive still holds the pointer. This trades a bounded leak for
  a use-after-free.
- **A per-closure thread-cloned interpreter** (mechanism 1). Rejected because a callback that
  cannot see or mutate the caller's state is worse than one that fails loudly: the qsort
  comparator's counter, and any binding that accumulates into an outer buffer, would silently
  do nothing.
- **A single global interpreter pointer** (mechanism 2). Rejected as unsound under threads.
- **Registering the callback lazily at first C invocation** (build the closure inside the
  trampoline). Circular — there is no trampoline until a closure exists.

## Implementation

- `src/runtime/nativecall_callback.rs` (new) — the intern table, the `InterpreterGuard`
  thread-local, argument/return marshalling, and three trampolines (word-sized, `f32`, `f64`;
  libffi widens a sub-word integer closure result to one machine word, which the word
  trampoline writes).
- `src/runtime/nativecall.rs` — `CType::Callback`, `ParamSpec::callback: Option<Box<CallbackSig>>`
  (so `ParamSpec` loses `Copy` but stays word-sized for ordinary parameters), the marshalling
  arm, and the `interp` parameter plus guard around the libffi call.
- `src/vm/vm_register_sub_ops.rs` — `callback_signature` / `callback_ctype` map a
  `code_signature` onto a `CallbackSig`; every aggregate spelling in a callback signature
  (`CArray[T]`, `Buf`, a CStruct handle) is one machine word, since C hands the value over and
  there is nothing to reify or copy back.
- `src/parser/stmt/sub_param/param_inner.rs` — `&`-sigilled sub-signature becomes a
  `code_signature` with its return type preserved.
- Pin: `t/nativecall-callback-param.t` (libc `qsort` / `bsearch`, so no distribution is needed;
  verified to produce identical output under real `raku`).

## Outcome

The named spelling (`&cmp (Sig)`) and the anonymous one (`& (Sig)`) both work, closing
`todo/tickets/nativecall-callback-parameter-marshalling.md` and the earlier LibZip parse
ticket's other half. `&cmp:(Sig)` also works in mutsu; real Rakudo's NativeCall does *not*
accept that spelling (its `param_list_for` dies on a `Signature` type object), so the pin test
deliberately does not assert it.
