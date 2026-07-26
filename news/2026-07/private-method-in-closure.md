# A private method called from inside a closure in the same class keeps its permission

`self!priv(...)` worked directly in a method, but the same call inside a `sub`,
pointy block, or bare block defined in that method was rejected:

```
Calling private method 'secret' must be fully qualified with the package
containing that private method.
```

A closure created inside a method of `C` is still lexically inside `C`, so
`self!secret` must remain legal however late the closure is invoked. Rakudo
decides this from the lexical scope of the call site, not from the frame that
happens to be running.

## Root cause

The permission check reads the *currently executing* method's class
(`method_class_stack`), falling back to `current_package`. Once the enclosing
method has returned, neither names `C`:

- `method_class_stack` only holds a class while a method body is running, and
  the closure is invoked from a foreign frame later;
- `current_package` is switched to the class **only for some method shapes** —
  when the class body declares class-scoped subs, when it declares `my` statics,
  or when the class name is `::`-qualified (`vm_method_dispatch.rs`). A plain
  `class C { method m() { ... } }` runs with `current_package` still `GLOBAL`.

A closure records its declaring package in `SubData.package`, and invoking it
restores that package (`vm_closure_dispatch.rs`) — but for a plain class the
recorded package was `GLOBAL`, so the class was lost at closure-creation time,
not at call time.

## The fix

Closure creation now records the package it is *lexically* inside:
`lexical_closure_package()` (`runtime/accessors_state.rs`) returns
`current_package`, except that a `GLOBAL`/empty package inside a running method
resolves to that method's class. The four `SubData` construction sites that
build closures (`vm_register_ops.rs`, `vm_register_sub_ops.rs` — bare blocks,
pointy blocks, anonymous subs, `WhateverCode`) use it instead of
`current_package` directly. The existing per-invocation package restore then
carries the class into the closure's frame, so the permission check sees `C`.

The permission itself is unchanged: an out-of-class caller still cannot make an
unqualified private call, and wrapping the call in a closure does not launder it.

Pin: `t/private-method-in-closure.t` — 9 assertions verified against raku first,
covering an anonymous sub, a pointy block, a block stored in a lexical, a block
passed to `.map`, a sub nested in a sub, a closure calling another closure, and
the two out-of-class rejections.

## A correction to the original report

The ticket listed `method via-block() { { self!secret(9) } }` as returning a
`Block` in raku. It does not — raku *runs* the trailing bare block and returns
its value, so `$c.via-block()()` dies with `No such method 'CALL-ME' for string
'S:9'` there too. mutsu already matched raku on that shape; only the closure
permission was wrong.
