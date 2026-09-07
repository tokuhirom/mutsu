# Two env-pure method dispatch shapes stop flattening the caller's scoped env

Seventh perf slice for `todo/deep/vendor-real-test-module.md`. The
`CallMethodMut` opcode collapses a scoped overlay env to a flat one before any
dispatch past the pure-read accessor fast path, because full dispatch may
capture the env into a closure or run an interpreter fallback that iterates it.
A vendored-`Test` assertion makes exactly two method calls, `$desc.Str` and
`$output.say: $tap`, and both paid that guard: the flatten itself is a
whole-scope map clone, and once `proclaim`'s env is flat its return merge and
env drop become scope-sized instead of overlay-sized.
`todo/perf/method-dispatch-flattens-the-env-on-every-call.md` records why the
wholesale answer -- deleting the guard and moving the flatten to the consumers
that iterate -- measured neutral-to-negative; this slice does not touch the
guard. It answers two dispatch shapes *before* it, the way the accessor read
already is:

- **A pure native method on an immutable scalar receiver** (`Str`, `Int`,
  `Num`, `Bool`). `try_native_method` is Rust over the value, and an immutable
  receiver has no writeback, so none of the by-name or by-identity env scans
  the container mutators perform can be reached. Every decision the general
  path made for such a receiver is made identically: the same `quoted` /
  modifier exclusions, the same junction-argument autothreading deferral, the
  same `native_lever_a_user_override` gate (an augmented `Str.uc` still
  dispatches), and the methods that own a dedicated branch on the general path
  (`subst-mutate`, the `hyper`/`race` configuration form, the xxx-KEY /
  `BIND-POS` / `add`/`remove` mutator arms, the `push`-family
  autovivification, the undeclared-type `.new` check) are left to it.
- **Text output to a native `IO::Handle`** (`print`/`put`/`say`/`printf`/
  `print-nl` on an exact `IO::Handle` instance with no augmented override).
  `try_native_io_handle_output` writes handle-table state and renders its
  arguments; it neither captures nor iterates the caller's env. A user
  subclass has a different class name and keeps the full path, so its `WRITE`
  override still routes through `try_user_io_handle_method`.

The hoisted shapes also skip the receiver-typed branches between the guard and
the general native probe (junction threading, `WHO`, `Lock.protect`, the
shared-array mutators, the `skip_native` computation, `Match.make`, the
HyperSeq arms, the xxx-KEY match), which is why the win exceeds what the
flatten alone cost. `src/vm/vm_call_method_mut_prenative.rs` holds the entry;
the general path is unchanged for everything it declines.

## Measured

Callgrind, 300 `ok 1, "x"` in a loop under `MUTSU_REAL_TEST=1`, one-assertion
baseline subtracted, release build:

| | before | after |
| --- | --- | --- |
| per assertion | 313,662 Ir | 285,180 Ir |
| `exec_call_method_mut_op_impl` (both calls) | 29.6k | 14.6k |
| `flatten_scoped_env` | 8.8k | 0 |
| `proclaim`'s frame (`call_compiled_function_named_inner'2`) | 194.8k | 166.4k |

**-9.1% per assertion**, against a -7.5% upper bound measured by disabling the
guard outright (an unsound kill-switch run, purely to size the prize): the
guard's flatten, the scope-sized return merge, and the scope-sized env drop
all go, plus the intermediate branches.

Since the session opened at 335,929 Ir per assertion this and the previous
slice (`news/2026-09/nqp-ops-and-str-gist-skip-the-call-machinery.md`)
total **-15.1%**.
