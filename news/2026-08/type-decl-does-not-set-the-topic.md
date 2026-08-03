# A type declaration no longer sets the topic

Filed as `todo/tickets/anon-role-mixin-clobbers-the-enclosing-topic.md` on
2026-08-03 with this repro:

```
$ mutsu -e 'for ^3 { my $o = 42 but role :: { method z { 1 } }; say "n=$_"; }'
Use of uninitialized value element of type __ANON_ROLE_0__ in string context.
n=
... (x3)
$ raku -e 'for ^3 { my $o = 42 but role :: { method z { 1 } }; say "n=$_"; }'
n=0
n=1
n=2
```

The `but` mixin turned out to be incidental. Bisecting the repro at the Raku
level dropped it to `for ^3 { role R { }; say $_ }` — and then to
`for ^3 { class C { }; say $_ }`, which fails the same way. **Any** `class`,
`role` or `grammar` declaration overwrote the enclosing `$_` with its own type
object, anonymous or not, inside a loop or at the top level.

## The write

`exec_register_class_op` and `exec_register_role_op` (`src/vm/vm_typedecl_ops.rs`)
each ended with

```rust
env.insert("_".to_string(), Value::package(Symbol::intern(&storage_name)));
```

with no comment justifying it; the surrounding comment describes only the
*name* registration below it. It predates the `vm_register_ops.rs` split
(#3756) and had been carried through every refactor since.

Finding it took one `rust-gdb -batch` run. The ticket's suggested starting
point — breaking on the `SetTopic` / `RestoreTopic` opcode arms, which is where
the 2026-07-29 role-parameterisation topic leak lived — produced nothing: the
compiled loop body contains no `SetTopic` at all. A conditional breakpoint on
`Env::insert` (`break src/env.rs:612 if key.vec.len == 1`) named the writer on
its first hit, one frame below the opcode dispatch.

## What the write was for

Deleting it broke five `t/` files, all of the shape "an `EVAL`'d compilation
unit returns the declared type":

```raku
my $t = EVAL 'unit class UC0 is export; has $.x = 42;';
is $t.new.x, 42;
```

`$_` doubles as the **block-value channel**: `eval_block_value_inner` notes that
"a carrier block publishes its value through the topic". So the registration was
publishing the declaration's value — correctly — down a channel that is also the
caller's topic.

The value now travels the same way a trailing `sub` declaration's already did.
`eval_block_value_inner` had a `trailing_sub_value` special case for exactly
this ("when the block's last statement is a sub declaration, its value is the
declared sub"); it is extended to `ClassDecl` / `RoleDecl`, reading the type
object back out of the env under the declared name — registration has already
installed the possibly-mangled storage name there, so nothing re-derives the
mangling.

## The second consumer: a postfix after the closing brace

One test failure was a different shape:

```raku
method iterator {
    class It does Iterator { ... }.new(cur => $.from);
}
```

Inside a routine body the statement parser took `class It { ... }` as a
declaration statement and left `.new(...)` to start a *new* statement — which
the generic "leading `.` means `$_.method`" rule turned into `$_.new(...)`. That
only ever worked because registration had just put the class into `$_`.

Raku parses `class It { ... }.new` as one expression, and it does so even across
a space: `for ^2 { class C { } .say }` prints `(C)` twice, not `0` and `1`. Only
a newline before the `.` starts a fresh statement (`0`, `1` — which mutsu
already got right). So the statement-level `class_decl` / `role_decl` /
`grammar_decl` parsers now decline when a `.` follows on the same line
(`reject_trailing_postfix`), and the expression-statement path — whose
`anon_class_expr` accepts a named class too — parses declaration and postfix as
the single expression they are.

Pin: `t/type-decl-does-not-set-topic.t` (12 assertions covering class/role/
grammar in a loop, at the top level and under `given`; the anonymous-role `but`
mixin; the type object still being the value of an anonymous declaration
expression, of a same-line postfix in a routine body and of an `EVAL`'d unit;
and a newline before the next statement keeping the topic). It passes under real
`raku` too.
