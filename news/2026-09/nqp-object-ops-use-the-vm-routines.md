# `nqp::istrue` / `istype` / `isconcrete` / `eqaddr` / `clone` ask the VM's own routines

Five object-level `nqp::` ops each kept a private copy of a question the VM already answers.
Every copy diverged from rakudo (ADR-0118 §2.3):

- **`nqp::istrue`** used the pure `Value::truthy()`. It answered 1 for an object whose `Bool`
  method returns False, and for an empty `.grep` result that had not run yet. It now uses
  `eval_truthy`, which is the same boolification `?`, `if` and TRIR use.
- **`nqp::istype`** called the bare `type_matches_value`, which skips the checks `~~` adds on top:
  the type smiley, enums, and the MRO correction that stops `class M is Mu` from matching `Any`.
  So `nqp::istype(M, Any)` answered 1. `~~`'s type-object branch is now the function
  `type_object_accepts`, and both use it.
- **`nqp::isconcrete`** and **`nqp::defined`** asked Raku's `.defined`, which reports a `Failure`
  and `Empty` as undefined. They are concrete objects, and rakudo's `nqp::isconcrete` answers 1
  for both. The ops now ask the new `value_is_concrete`.
- **`nqp::eqaddr`** used `===`'s helper. `===` consults a user `WHICH`, and the memo holding that
  answer is only filled after a `===` has run on the pair. So the result of `eqaddr` depended on
  whether a `===` had run first. It now uses `values_same_object`, which checks object identity
  and never `.WHICH`.
- **`nqp::clone`** handed back the same instance, so the original and the "copy" shared all their
  attributes, and it turned an `Array` into a `List`. It now uses the native `Mu.clone` and
  `Value::array_shallow_clone`. The latter is also what `.clone` on an array uses now.

`t/vm/nqp-object-ops-parity.t` pins 21 rakudo-measured rows.
