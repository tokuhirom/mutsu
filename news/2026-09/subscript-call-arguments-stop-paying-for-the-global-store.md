# Subscript call arguments stop paying for the global variable store

Every non-slice subscript argument (`f(@a[i])`, `f(%h<k>)`) gets `is rw`
snapshot/writeback temps at its call site, whatever the callee: before the
call the element is stored under `__mutsu_index_rw_arg_N` and
`__mutsu_index_rw_orig_N`, and afterwards the result under
`__mutsu_call_result_N`, then the temps are read back to decide whether an
`is rw` parameter changed the element
([#9505](https://github.com/tokuhirom/mutsu/issues/9505)).

Those stores and reads used `SetGlobalRaw` / `GetGlobal`, the general by-name
variable path: a `Symbol::intern` of the name on every store, readonly, type
and strict checks, `our`/shared-store mirroring, `::` scans and the whole
package/unit-lexical read chain. That is roughly 19k instructions per subscript
argument per call (callgrind: 729M vs 341M Ir for 20k calls of `g(@x[3], 5)`
vs `my $v = @x[3]; g($v, 5)`). The temps are compiler-internal names no user
code can see, so none of it applied.

Two dedicated opcodes now handle them. `SetCallTemp` replaces the env entry
under the pre-interned name, and `GetCallTemp` reads it back decontainerized,
the same way `GetGlobal` does. The temps still live in `env`, because an `is rw`
callee writes its parameter back by that name (`apply_rw_bindings_to_env`), so
the writeback behaviour is unchanged.

Release build, 4-core container, `tmp/idxarg.raku` from the issue (200k calls):

| | before | after |
| --- | ---: | ---: |
| `g(@x[3], 5)` | 1.05 s | 0.43 s |
| `my $v = @x[3]; g($v, 5)` | 0.45 s | 0.45 s |

`t/collections/subscript/index-arg-rw-writeback-temps.t` checks that the
writeback semantics are unchanged: array and hash elements, loops, an unchanged
`is rw` parameter, a callee that writes the slot directly, recursion, and an
lvalue `is rw` return.

Retiring the temps altogether (container-mode arguments) is still ADR-0059
Slice 3.
