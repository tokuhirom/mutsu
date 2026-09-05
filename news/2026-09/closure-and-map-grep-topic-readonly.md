# A bare block's implicit topic is read-only when it aliases an immutable item

`(1, 2).map({ $_ = 5 })`, `(1, 2).grep({ $_ = 5 })` and `my $s = { $_ = 5 };
$s(7)` — the three closure-topic rows of
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` — all
succeeded silently in mutsu where rakudo throws `X::AdHoc`, "Cannot assign to an
immutable value". They now answer exactly what rakudo answers, and so do six
neighbours found on the way (`(1..3).map`, `%h.keys.map`, the listop
`map { $_ = 5 }, 1, 2` and its `grep` twin, `{ $_ = 5 }(3 + 4)`, and the
`&`-sigiled `my &s = { $_ = 5 }; s(7)`).

## The blocker the ticket recorded did not exist, and neither did its successor

The ticket said these rows were blocked on "separating the direct value call and
native map rw-writeback callers of `call_compiled_closure_with_topic`, which
currently share it". A gdb-instrumented sweep disproved that: `capture_rw_topic
== true` is produced by exactly one call site in the tree (`vm_native_map.rs`,
gated on `writeback_name.is_some()`), so it already means precisely "this topic
is an element of a named real `Array` that will be written back" — the
separation was already there. The two `.map`/`.grep`-over-a-literal rows never
reach that function at all; they land in `eval_map_over_items` /
`eval_grep_over_items_with_mutated` and bind the topic at `bind_loop_topic`.

The obvious replacement — decide per item at runtime, `!item.is_container_ref()`,
the test `vm_for_loop_lazy.rs` already applies on the lazy `for` path — was
measured and rejected. A real `Array`'s elements are stored **bare** in mutsu, so
that test additionally rejects five shapes rakudo accepts: `@a.list.map({ $_ = 7
})`, `@a[0..1].map(...)`, `@a.Seq.map(...)`, `my $x = [1,2,3]; $x.map(...)` and a
`.sort` key extractor. Turning a silent no-op into a spurious throw is a worse
answer, not a better one, so the runtime rule was dropped.

## What shipped instead: the receiver's syntax, decided at compile time

The oracle is `Compiler::for_iterable_yields_bare_items` — the same conservative
predicate that fixed the `for`-loop topic rows in August (`for 1, 2 { $_ = 5 }`
and `(1, 2).map({ $_ = 5 })` are the same rejection in rakudo, for the same
reason). When a `.map`/`.grep` (method or listop form) is written directly
against a source it proves bare, the bare-block argument is compiled with a new
`CompiledCode::immutable_topic` flag, and the three loops mark `$_` read-only per
iteration. Because the predicate answers `false` for every variable and every
derived receiver, no shape that works today starts throwing.

The direct value call (`$s(7)`) is the same idea at the argument end: a new
`Compiler::expr_yields_container_less_value` recognises an argument expression
that provably mints a fresh value (a literal, or an operator result built only
out of such), `OpCode::CallOnValue`/`CallOnCodeVar` carry that verdict as a
`bare_args` flag, and `call_compiled_closure_with_topic` marks the implicit topic
when it holds. `{ $_ = 9 }($v)` and `{ $_ = 9 }(@a[0])` — which rakudo lets write
through — are excluded by construction, since a variable and a subscript are both
outside the predicate.

Both marks are made at the **call/loop site**, never as an injected body prologue:
a prologue statement runs wherever the bytecode does, including the
`push_call_frame`-bypassing native loops, where a mark with no readonly frame open
skips the undo journal and leaks permanently (the trap the `-> $v { $v = 1 }` fix
already hit). The grep loop, which had no per-iteration `ReadonlyFrameGuard` while
both map loops did, gained one.

## Five pre-existing divergences fell out of making the marking symmetric

`readonly_vars` is keyed by the bare name `_`, so a construct that marked its own
topic immutable left that mark in force inside *any* nested topic binding. That
was already wrong before this change, and it is why `for 1, 2 { ... }` bodies were
oddly restricted:

```
for 1, 2 { my @b = 7,8; for @b { $_ = 1 } }     # threw; rakudo: fine
sub f() { $_ = 5 }; for 1, 2 { f() }            # threw; rakudo: fine
my @a = 1,2,3; for 1, 2 { @a.map({ $_ = 5 }) }  # threw; rakudo: [5 5 5]
my @a = 1,2,3; for 1, 2 { @a.grep({ $_ = 5 }) } # threw; rakudo: [5 5 5]
```

Every construct that binds a topic now sets its writability in **both**
directions rather than only marking: the eager `for` loop clears the mark when
its own topic is writable (and restores the caller's marking on all three exit
paths), the map/grep loops do the same per iteration inside their readonly
frames, `call_compiled_closure_with_topic` clears it for a writable implicit
topic and for a routine's fresh `$_`, and `call_compiled_function_fast` — which
deliberately opens no readonly frame — saves the topic's kind and puts it back on
the way out, matching the `unmark_readonly_topic` every other call path already
did.

## Pins

`t/closure-topic-readonly.t` (31 assertions) pins both halves in one file, so a
future change cannot fix one by breaking the other: the rejected shapes, the
write-through shapes that must keep working (`@a.map({ $_ *= 10 })`,
`@a.map({ $_ = 5 })`, `@a.grep({ $_ = 5 })`, `%h.values.map({ $_ = 9 })`,
`@a.values.map({ $_ = 7 })`, the listop `map { $_ = 5 }, @a`, and a block called
with a variable or an element), the read-only uses of the topic, and the five
nesting cases above. Every expectation was measured against rakudo first; the
whole file passes under `raku` as well as under mutsu.

What is still open — the `@`-bound-List and `Seq`-variable receivers, `.first`
over a literal, `for %h`, the element shapes where mutsu drops the write instead
of performing it, and the error *class* rakudo uses for a compound assignment —
is recorded in
`todo/deep/immutable-lvalues-that-mutsu-still-lets-you-assign-to.md` with the
measurements that separate them from what landed here.
