# A NativeCall `&callback (Sig)` parameter is marshalled to a real C function pointer

`Language/nativecall.rakudoc`'s "Function arguments" section documents native functions that
take functions:

```raku
use NativeCall;
# void SetCallback(int (*callback)(const char *))
my sub SetCallback(&callback (Str --> int32)) is native('mylib') { * }
```

mutsu had only the *inbound* direction (`nativecast(:(...), $ptr)` makes a C function pointer
callable from Raku). The outbound half did not exist, and it was the last blocker for
`Archive::Libarchive::Raw`'s sixth test file — `archive_write_open` installs three of these —
as well as for LibZip's `zip_source_function`, which spells the same thing anonymously.

## What was actually wrong

The ticket's diagnosis ("the `&name (Sig)` form is being read as an ordinary typed parameter")
was close but off by one step, and the error message said so:

```
Calling qsort(Any, Int, Int, Sub) will never work with declared signature (CArray[int32], size_t, size_t, &cmp)
  X::TypeCheck::Argument: Type check failed for __type_only__: expected Pointer, got Sub
```

Both spellings already **parsed**. The parser recorded a `&`-sigilled parameter followed by
`(...)` as a **destructuring `sub_signature`** — an instruction to *unpack* the argument — so
the callback's own `Pointer` parameters became type constraints matched against the passed
`Sub`, under the anonymous-type-constraint name `__type_only__`. The knock-on effect was the
real blocker: with no mappable C type for that parameter, `register_native_call_routine`
bailed out and skipped native registration for the *whole* declaration, leaving the sub with
its `{ * }` Raku body — which is why ordinary signature binding ran at all.

`&`-sigilled is the distinguishing fact: a Callable cannot be unpacked, and what follows it is
a constraint on the callable's own signature (`Language/signatures.rakudoc`, "Constraining
signatures of Callables"; NativeCall's spelling merely drops the `:` of `&cb:(...)`). It is
now recorded as the parameter's `code_signature`, parsed with `parse_param_list_with_return`
so the `--> T` survives — that return type is the callback's C return type and nothing else
carries it.

## What was built

[ADR-0063](../../docs/adr/0063-nativecall-outbound-callback-ownership-and-reentrancy.md)
records the two design decisions, which are the substance of the change:

- **Ownership.** A marshalled callback is a **process-lifetime** libffi closure, interned by
  (callable identity, C signature). C routinely retains a callback past the call that installed
  it — libarchive keeps all three of `archive_write_open`'s for the lifetime of the archive
  handle — so a closure freed when that call returns is a use-after-free with no diagnostic.
  The leak is deliberate, mirrors `load_library_cached`'s and `native_object_where`'s, and is
  bounded by the intern table; holding the `Value` in the leaked userdata is also what makes
  the GC-address half of the key sound, since the allocation can never be freed and reused.
- **Re-entrancy.** `call_native_with_out_args` now takes the calling `&mut Interpreter` and
  pushes it onto a thread-local stack for exactly the duration of the libffi call. The
  trampoline runs the Raku body on that very interpreter, on that very thread, sharing all
  state — which covers a callback fired from the call itself *and* from a nested native call
  (libarchive's `archive_write_data` shape). A callback C fires later, or from a thread of its
  own, finds no interpreter, is reported on stderr, and returns zero rather than crashing. A
  Raku exception or Rust panic in the body is caught for the same reason: unwinding through a C
  frame is undefined behaviour, and the language docs already forbid throwing out of a callback.

New module `src/runtime/nativecall_callback.rs` holds the intern table, the guard, the
argument/return marshalling and three trampolines (word-sized, `f32`, `f64`).
`src/runtime/nativecall.rs` gains `CType::Callback` and `ParamSpec::callback`, and
`src/vm/vm_register_sub_ops.rs` maps a `code_signature` onto the callback's C signature.

## Result

The repro from the ticket, reduced to a dist-free libc `qsort`, now matches `raku`:

```
$ mutsu -e 'use NativeCall; sub qsort(CArray[int32], size_t, size_t, &cmp (Pointer, Pointer --> int32)) is native { * } ...'
1,3,5,9
```

Pinned by `t/nativecall-callback-param.t` (11 assertions over libc's `qsort` and `bsearch`,
byte-identical output under real `raku`): the named spelling, the anonymous `& (Sig)` spelling,
an anonymous lambda, two different comparators reaching two different closures, a callback that
mutates an outer lexical (proving the VM really is shared rather than cloned), and an `Order`
return unboxing into the declared `int32`.

`&cmp:(Sig)` works in mutsu too, but real Rakudo's NativeCall rejects that spelling
(`param_list_for` dies on a `Signature` type object), so the pin deliberately does not assert
it.
