# Every routine return allocated a `String` to say "CX::Return"

`return` is implemented as a control signal: the VM raises
`RuntimeError::return_signal(value)`, whose `message` field said
`"CX::Return".to_string()`. `message` was a `String`, so **every routine return
in every program mallocked and freed a 10-byte string**.

A call-graph profile of `bench-fib` charged essentially all of the benchmark's
`malloc` time to exactly that constructor:

```
mutsu_jit_1
  mutsu::vm::vm_jit_helpers::ret
    mutsu::value::error::RuntimeError::return_signal
      __GI___libc_malloc
```

with `malloc` at 2.2%, `_int_free` at 2.0% and `return_signal` itself at 0.8%
of self time.

## The fix

`RuntimeError::message` is now `Cow<'static, str>`, and `RuntimeError::new`
(plus `with_location`, `warn_signal`, `warn_signal_with_resume`) takes
`impl Into<Cow<'static, str>>`. A `&'static str` becomes `Cow::Borrowed` with
no allocation; a `format!` result becomes `Cow::Owned` exactly as before.

The win is not limited to the return path — it also covers the many
`RuntimeError::new("some literal")` sites throughout the runtime, which used
to allocate a `String` for a string constant.

Most call sites needed no change at all: `Into<Cow>` accepts both `String` and
`&'static str`, and `Cow<str>` derefs to `str`, so reads like
`&err.message` and `format!("{}", err.message)` still compile untouched. The
diff is confined to sites that *moved* the message into a `String` position
(`Value::str(err.message)` → `.into_owned()`) or built one by mutation
(`err.message = format!(...)` → `.into()`).

`RuntimeError` grows from 80 to 88 bytes as a result; the measurements below
are net of that.

## Measurement

Interleaved A/B of two release builds, median over nine alternating runs on a
pinned P-core:

| benchmark | cycles | instructions |
| --- | ---: | ---: |
| `fib` | −14.8% | −7.2% |
| `bench-fib` | −11.2% | −7.2% |
| `bench-class` | −1.1% | |
| `bench-tak` | −0.6% | |
| `method-call` | −0.4% | |

Both orderings were measured on `bench-fib`, `fib` and `bench-class`; every
sign flipped with the swap. Retired instructions drop 7.2%, which is the
layout-insensitive confirmation that real work is gone. `bench-tak` barely
moves because its body has no explicit `return`, so it never raises the signal
— which is itself a good check that the win is attributed correctly.

`value::error::tests::control_signal_messages_do_not_allocate` pins that the
return signal's message is `Cow::Borrowed`, so a future edit cannot quietly
put the malloc back.
