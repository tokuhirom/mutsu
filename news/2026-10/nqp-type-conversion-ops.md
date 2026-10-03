# `nqp::` coercion, big-integer conversion and value-test ops

Part of the `nqp::` coverage campaign (#11488). This lands 23 of the 40 ops
tracked by #11492, all of which used to die with `Unsupported nqp:: op`:

- native coercions: `coerce_in`, `coerce_ni`, `coerce_ns`, `coerce_iu`,
  `coerce_ui`, `coerce_us`
- `intify`, `numify`
- big integers: `tostr_I`, `tonum_I`, `fromstr_I`, `fromnum_I`, `fromI_I`,
  `bool_I`, `isbig_I`, `isprime_I`
- boxes: `box_n`, `box_u`, `decont_i`, `decont_n`, `decont_s`
- `isinvokable`, `isttyfh`

The answers match MoarVM's, quirks included:

- `coerce_ni` gives `i64::MIN` for NaN, ±Inf and anything else out of range.
- `coerce_us` renders the 64 bits as signed, so `coerce_us(2**64 - 1)` is
  `"-1"`.
- `isbig_I` is true outside `-2**31 < n < 2**31`.
- Each `decont_*` op unboxes only its own boxed type: `decont_i("42")` dies.
- An object with a `CALL-ME` method is not `isinvokable`.

The conversions reuse existing Raku routines:

- `coerce_ns` uses the `Num.Str` form.
- `isprime_I` uses `Int.is-prime`'s `builtins::primality`.
- `isttyfh` uses `IO::Handle.t`.
- The unsigned reads use the `unbox_u` body.

The other 17 ops ask about representations mutsu does not have yet. They are
`isstr`, `isint`, `isnum`, `ishash`, `iscoderef`, the eight `boot*` type
ops, `iscont_i`, `iscont_n`, `iscont_s` and `isrwcont`. Answering them needs
MoarVM's `BOOT*` REPRs, native lexical references (`IntLexRef`) and container
descriptors. They stay loudly unsupported and are tracked as a design
question in #11553.
