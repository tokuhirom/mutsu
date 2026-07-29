# Parameterising a role no longer retopicalizes the caller

Every compiled body ends with a `SetTopic`: that is how a block publishes its
value, and `run_compiled_block` reads `$_` back to produce the block's result.
Two of the bodies run during role parameterisation are not lexically the
caller's, though — the type-argument expression (`R[Elem]` compiles and runs
`Elem`) and the role body's deferred statements — and both were leaving their
value in whatever `$_` happened to be current.

So the *first* `R[T].method` call inside a topic block silently changed the
topic:

```raku
role P[::T] { method make() { 'x' } }
class Elem { }
class Holder { method tag() { 'TAG' } }

with Holder.new {
    my $x = P[Elem].make;
    say .^name;    # raku: Holder    mutsu (before): Elem
    say .tag;      # raku: TAG       mutsu (before): No such method 'tag' for Str/Elem
}
```

It only ever bit the first call, because the second one finds the pun already
registered and never re-composes — which is exactly the shape that makes it look
like a heisenbug.

Found in `DBIish`. `DBDish::mysql::StatementHandle`'s `BUILD` is

```raku
with $!stmt {
    if $!param-count = .mysql_stmt_param_count -> $pc {
        $!par-binds = LinearArray[MYSQL_BIND].new($pc);   # <-- retopicalized here
        ...
    }
    if ($!field-count = .mysql_stmt_field_count) && ... { ... }
}
```

and the second `if` dispatched `.mysql_stmt_field_count` on the type argument
instead of the statement handle.

The fix is at the carrier level rather than at the two call sites:
`eval_block_value` now restores the caller's topic after `run_compiled_block`
has read the block's value, and the deferred-role-body loop does the same. An
EVAL'd compilation unit is deliberately exempt — it runs *in* the caller's
scope, so `EVAL '$_ = 3'` really does set the caller's topic, which is what
rakudo does. The role body's other lexical effects still persist on purpose: a
composed method closes over `my int $sol = nativesizeof(T)`.

Pinned by `t/role-parameterisation-keeps-the-topic.t`.
