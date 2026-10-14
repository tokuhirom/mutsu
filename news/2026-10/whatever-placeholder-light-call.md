# WhateverCode and placeholder blocks take the light closure call

The light closure call (#8335, `src/vm/vm_closure_light_call.rs`) ran only
pointy blocks. A WhateverCode (`* + 1`) and a placeholder block
(`{ $^a + $^b }`) have the same kind of body, a few pure value ops, but every
call of one still went through the full closure entry. Both shapes are the hot
callees of the `Digest` distribution's `ripemd.t` (#8471). They now take the
light call too (#13286).

- A WhateverCode is marked on its compiled code (`CompiledCode::is_whatever_code`).
  Its single `*` is the topic parameter `_`, which binds the argument as it is.
  A Junction argument declines, because the full entry autothreads the call
  over it. The call frame keeps the code object's own name, as the full entry
  names it.
- A placeholder block binds each `$^name` argument as a plain `$` parameter, in
  sorted-name order.
- A call-site variable argument no longer declines a plain parameter. It only
  hands the parameter the variable's type constraint, and a light body never
  assigns. An `is raw` parameter still declines, because it aliases the
  variable.
- Every light call now declines when the caller's env overlay holds a container
  under a parameter's name, such as a `for @a` loop's `$_`. `GetLocal` reads the
  slot only while no such container exists (ADR-0097 §15).

Release callgrind, `MUTSU_JIT=off`, Ir per loop iteration:

| call | before | after |
| --- | ---: | ---: |
| `my &f = * + 1; f($s)` | 22,143 | 10,538 |
| `my $f = * + 1; $f($s)` | 18,358 | 4,363 |
| `my &f = { $^a + $^b }; f($s, 1)` | 28,982 | 10,865 |
| `my $f = { $^a + $^b }; $f($s, 1)` | 26,000 | 4,892 |
| `my $f = -> $a, $b { $a + $b }; $f($s, 1)` | 23,579 | 4,921 |
