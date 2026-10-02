# A JIT-compiled loop keeps calling a user `infix:<*>` / `infix:<->`

The JIT's Tier B inline arithmetic runs `Int * Int` and `Int - Int` (and the
`Num` pairs) without calling the interpreter. Only the `Add` path checked
`USER_INFIX_DECLS`, the process-wide count of user infix declarations, before
taking the inline path. So once a loop got hot enough to be compiled (about 100
iterations), a user `multi infix:<*>(UInt $a, UInt $b)` or `infix:<->`
stopped being called on Int operands, and the native operator answered instead:

```raku
my $m = 0;
multi infix:<*>(UInt $a, UInt $b) { $m++; callsame() }
my $x = 1;
for 1..300 { $x = $x * 1 }
say $m;   # rakudo: 300, mutsu before: 99
```

The interpreter arms (`exec_mul_op`, `exec_sub_op`) already checked for a user
override, so the bug was only in the JIT. `Sub` and `Mul` now carry the same
guard as `Add`. Found while profiling the operator section of
`bench-multi-dispatch` (#10111). Until this fix, that section ran its `*`
candidate 99 times instead of 15000.
