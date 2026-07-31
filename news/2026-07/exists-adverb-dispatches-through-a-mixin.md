# `:exists` dispatches EXISTS-KEY / EXISTS-POS through a mixin

`$obj<k>:exists` answered `False` for everything when `$obj` was a mixin, even
where a direct `$obj.EXISTS-KEY('k')` answered `True`:

```raku
role R { }
my %h = a => 1;
my $m = %h but R;
say $m<a>:exists;      # raku: True,  mutsu: False
my @a = 1, 2, 3;
say (@a but R)[1]:exists;  # raku: True,  mutsu: False
```

## Root cause

The exists opcode (`exec_exists_index_adv_op`) walks a chain of container arms —
Hash, Pair, Stash, Set, Bag, Mix, Instance — and dispatches the subscript
protocol (`EXISTS-KEY` / `EXISTS-POS`) only from the `Instance` arm. A `Mixin`
matches none of them, so the subscript fell all the way through to the generic
tail whose final arm is `_ => false`.

Two quite different things land on that arm. A role mixed into a container
(`%h but R`) is the obvious one. The less obvious one is a `does Associative`
role that delegates to a private hash and is then *punned* — punning a role
builds a mixin, not an `Instance`, so an object like DBIish's

```raku
role TC does Associative {
    has %!store handles <AT-KEY EXISTS-KEY>;
    method STORE(\v) { %!store{.key} = .value for @(v) }
}
class C { has %.conv is TC }
```

read fine through `AT-KEY` (the read path has an explicit `(Mixin, Str)` arm) but
answered `False` to `$c.conv<a>:exists`. That is the shape the bug was found in.

## Fix

The exists opcode now routes a `Mixin` target through the same
`instance_exists_pos_result` the `Instance` arm uses. Method dispatch does the
rest: where the mixed-in role supplies `EXISTS-KEY`/`EXISTS-POS` (declared or
delegated with `handles`) those run, and where it supplies nothing dispatch
reaches the inner container's own methods, so a plain `%h but R` answers for its
contents. Slices and the `:kv`/`:p`/`:k`/`:!exists` adverbs come along for free,
because `instance_exists_pos_result` already implements them.

Pinned by `t/exists-adverb-on-mixin.t` (14 assertions, verified to pass under
`raku` as well as mutsu). `t/bless-is-type-container-attr.t` used a direct
`.EXISTS-KEY` call as a stand-in and is switched back to `:exists`.

## Not covered

`:delete` on a mixin has the same shape and is not fixed here: mutsu implements
`%h<k>:delete` directly in the opcode rather than through a `DELETE-KEY` method,
so there is no builtin for the mixin path to reach. Recorded in
`todo/tickets/delete-adverb-on-mixin.md`.
