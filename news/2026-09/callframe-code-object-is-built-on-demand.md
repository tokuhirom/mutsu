# A named routine's `callframe().code` object is built on demand

Ninth perf slice for `todo/deep/vendor-real-test-module.md`. Every named
routine call pushed its own `Sub` value on entry so that `callframe().code`,
`&?ROUTINE` and the backtrace could hand it out:

```rust
Value::make_sub(pkg, name, cf.params.clone(), cf.param_defs.clone(), vec![],
                false, self.clone_env())
```

That is a `Gc` allocation, two `Vec` deep clones, and -- because `clone_env`
flattens -- a whole-lexical-scope map clone whenever the caller's env was a
scoped overlay, which it is for every call made from inside another routine.
Then the object is dropped on return. Almost no call ever reads it. On the
vendored `Test.rakumod`'s assertion loop the two frames (`ok`, `proclaim`)
cost ~16k instructions per assertion in `Env::flattened` and
`drop_in_place<Gc<SubData>>` alone, and
`todo/perf/method-dispatch-flattens-the-env-on-every-call.md` had already named
"a lazy `callframe().code` env" as one of the three per-call full-view
consumers standing between the interpreter and a cheaper scoped-env story.

`block_stack` and `CallFrameEntry::code` now hold a `CodeFrame`:
`Ready(Value)` for a closure/block body or an interpreter-carrier routine
(unchanged), or `Lazy(Arc<LazyRoutineCode>)` for a named compiled routine.
The lazy frame records the ingredients -- package, name, params, param_defs,
and the caller's env as an `Arc` bump (later caller writes copy-on-write away
from it, so it is the same snapshot the eager flatten took) -- and
`Interpreter::code_frame_value` builds the `Sub` on first read exactly as the
entry path used to (full lexical view via `Env::flattened`, routine mixins
re-applied), caching it in the frame so repeated reads within one frame see one
object. The readers (`callframe(N).code`, `&?ROUTINE`, `&?BLOCK`, the
backtrace's routine search, `CallFrame.code` attributes) go through that
accessor; the backtrace search compares the frame's `(package, name)` without
materializing. The cycle collector roots a lazy frame's captured env values
and its built object.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 276,814 Ir | 261,339 Ir |
| `Env::flattened` | 8.2k | 0 |
| `drop_in_place<Gc<SubData>>` | 7.9k | 0 |
| `proclaim`'s frame (`call_compiled_function_named_inner'2`) | 166.4k | 144.1k |

**-5.6% per assertion**; -22.2% since the session opened at 335,929
(`news/2026-09/nqp-ops-and-str-gist-skip-the-call-machinery.md`,
`news/2026-09/env-pure-method-dispatch-skips-the-scoped-env-flatten.md`,
`news/2026-09/free-variable-reads-drop-the-substring-searchers.md`).
