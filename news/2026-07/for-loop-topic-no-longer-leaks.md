# A `for` block no longer leaks its topic into the enclosing `$_`

In Raku a `for` block owns its topic: `$_` is the block's own (implicit)
parameter, so once the loop ends the enclosing `$_` is exactly what it was.
mutsu writes the topic into env instead, and restored it afterwards only when the
loop took no explicit parameter *and* its body held exactly one non-`SetLine`
statement. Every other shape leaked.

The most damaging shape was a loop with several parameters. `for LIST -> $a, $b`
iterates the list in chunks, and mutsu binds `$_` to the current chunk — a
`List` — so after

```raku
for %spec.value<> {
    for 'main', $_<template>, |$_<partials>.kv -> $name, $text { ... }
    $_<expected> .= subst(:g, "\r\n", "\n");   # <-- $_ is now the last chunk
}
```

the outer `$_` was no longer the hash the outer loop had bound, and the next
subscript died with `Type Array does not support associative indexing`. A
zero-parameter inner loop leaked just as readily, leaving `$_` set to the last
item; a mainline `for` left `$_` set after the file's loops had finished.

The topic is now restored unconditionally. The narrow condition was a heuristic
that happened to cover the shape someone had a failing test for, not a semantic
distinction — the VM already had the save/restore machinery and the compiler was
simply declining to ask for it.

This was one of two blockers behind `Template::Mustache`'s
`92-specs-file.rakutest`, where every subtest died in its setup before reaching a
single assertion.

Pin: `t/for-topic-restore.t`.
