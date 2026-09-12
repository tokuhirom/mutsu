# An attribute initializer copies its container instead of aliasing it

An attribute initializer written with `=` is an **assignment**, so the attribute
gets a copy of whatever container the right-hand side evaluated to. mutsu stored
the evaluated value directly, so the attribute shared the very container on the
right — exactly as if `:=` had been written:

```
$ raku  -e 'my @o = 1,2; class C { our @.x = @o; }; @o.push(3); say C.x'
[1 2]
$ mutsu -e 'my @o = 1,2; class C { our @.x = @o; }; @o.push(3); say C.x'
[1 2 3]
```

On a class-level attribute that made `=` and `:=` indistinguishable, which is
the actual defect: the two spellings mean different things and only one of them
was implemented ([#8150](https://github.com/tokuhirom/mutsu/issues/8150)).

## The per-instance half the ticket did not name

The same measurement on `has` shows the ticket was describing one side of a
wider gap — the per-instance initializer aliased too:

```
$ mutsu -e 'my @o = 1,2; class C { has @.x = @o }; my $c = C.new; @o.push(3); say $c.x'
[1 2 3]     # raku: [1 2]
```

That one is worse than it first looks, because it also makes two instances built
from the same default share one container: `C.new.x.push(99)` was visible
through the next instance's `.x`, and wrote back into `@o` as well.

Both are fixed, and each at the single place its path already funnels through:

- **Per-instance** — `Interpreter::eval_attr_default_expr` is "the single
  env-setup shape for evaluating an attribute default", shared by `dispatch_new`'s
  pre-BUILD fill, the post-BUILD deferred pass, the native default-constructor
  fast path and `dispatch_bless`. Detaching its result covers all four at once.
- **Class-level** — `Interpreter::class_body_has_decl` stores into
  `class_def.class_level_attrs`, and had nothing to tell `=` from `:=`. Both
  role-header spellings of the declaration parsed into the same
  `Stmt::HasDecl { default: Some(expr) }`, so the declarator is now carried on
  it (`default_is_bind`, `#[serde(default)]`, mirrored onto
  `CompiledAttrDecl`) — set only by the `my`/`our` class-level parser, since
  `has @.x := ...` is "Cannot use := to initialize an attribute" in rakudo and
  is refused before it gets here.

Both sides reuse `Value::detach_shared_container`, the existing "Raku `=` copy
semantics" primitive (`my @b = @a` yields `@b !=:= @a`, also used by `is copy`
parameter binding). It returns a singly-owned `Gc` unchanged, so a default that
built its own fresh container pays nothing.

## Found on the way, filed not bundled

[#8175](https://github.com/tokuhirom/mutsu/issues/8175): the class-level
initializer is not a list assignment at all — `our @.x = 1, 2, 3` keeps only the
first element (the parser stops at the first comma), and `our @.x = (1, 2, 3)`
stays a `List` because that store never goes through
`coerce_attr_value_by_sigil`. The ticket had assumed this spelling "looks right
by accident"; it does not. Fixing either half alone leaves the other spelling
wrong, so it is its own ticket rather than a rider here.

Pinned by `t/oo/attribute/attribute-initializer-copies-its-container.t` (10
assertions, green under `raku` as written), alongside the existing
`class-level-attribute-bind.t`, which pins the `:=` aliasing this must not take
away — one bind assertion is repeated in the new file for exactly that reason.
