# A plain `$s = $i` store takes the fast path again

A plain store from one scalar variable into another (`$s = $i`) cost about
2,400 instructions. Most of that went to machinery it never needed (#10955).

**The array-share mark.** `$s = $src` compiles to `MarkArrayShareSource`
before its `SetLocal`, so that `$s = @a` (or a chained `$r = $q` whose `$q`
holds an array) shares the container by reference. The mark was set for
every value. A pending mark is a store flag, so `SetLocal` declined its
scalar fast path and ran the full cascade. That cascade copied the source
name, and its tied-store probe formatted and interned a
`__mutsu_sigilless_readonly::` key on every store. The mark is now left
unset when the value already on the stack cannot be shared: an `Int`, `Num`,
`Str`, numeric, `Bool`, type object, pair or range. That is a tag-only probe
(`Value::is_never_array_share_source`), written as an allowlist, so a wrapper
that could resolve to an aggregate (`ContainerRef`, `Proxy`, `HashEntryRef`,
...) keeps the share check.

**JIT shims.** `CheckReadOnly`, which every whole-variable assignment runs,
and `MarkArrayShareSource` were generic `step` ops in a JIT-compiled loop,
each paying a full `exec_one` dispatch. Both now have dedicated shims. Their
bodies moved into one method each (`vm_check_read_only.rs`,
`vm_array_share_mark.rs`), shared with the interpreter's dispatch arm.
`CheckReadOnly`'s error path goes through `finish_op_result`, the
post-processing `exec_one` applies, so a refusal is still located and
backtraced. The scalar fast store also skips its itemization and
identity-restore `view()` matches for those inert scalar kinds.

Measured with callgrind: the method-loop iteration count was varied from
10,010 to 100,010, which leaves out the JIT's one-off compile. Before and
after are paired release builds:

| per iteration | `main` | after |
|---|---:|---:|
| empty method loop | 1,167 | 995 |
| `$s = $i` above the empty loop | 2,405 | 518 |
| `$s = $!x` | 4,415 | 2,355 |
| `$!y = $i` | 7,193 | 6,538 |

In wall clock, the method loop with `$s = $i` went from about 346 to about
106 ns per iteration (rakudo: about 56).
