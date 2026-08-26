# `lazy { BLOCK }` produces a lazy list, not an opaque `lazy(...)` thunk

From the doc-diff harness (`Language/statement-prefixes.rakudoc:32`):

```raku
my @array = lazy { (^3).map( *² )  };
say @array;         # raku: [...]      mutsu: [lazy(...)]
say @array.eager;   # raku: [0 1 4]    mutsu: [lazy(...)]
```

## Root cause

`lazy EXPR` parses as `EXPR.lazy`, so `lazy { ... }` reached the compiler as a
`.lazy` **method call on a block**. The block compiled to a closure `Sub` value,
and the native `.lazy` implementation has a `ValueView::Sub(..)` arm that wraps
the callable in a `LazyThunk`. That thunk is opaque: it renders as the literal
string `lazy(...)`, it is a *single* element when assigned to an `@` array, and
`.eager` has no arm that unwraps it — so both the placeholder print and the
forced print showed `[lazy(...)]`.

The measured Rakudo semantics are simpler than the thunk suggests. The `lazy`
statement prefix **runs its block eagerly** and only marks the *result* lazy:

```
$ raku -e 'my $s = lazy { say "run"; 1,2,3 }; say "after"'
run
after
```

and the result is a `Seq` with `.is-lazy` `True`.

## Fix

The compiler now lowers a `lazy BLOCK` operand (both the `AnonSub` and
`AnonSubParams` spellings, after the existing ADR-0048 placeholder check) to
`(do BLOCK).lazy`, which reuses the ordinary list `.lazy` marking that
`(1,2,3).lazy` already went through. `lazy { ... }` is therefore a genuine lazy
`Seq`: `.is-lazy` is `True`, an unforced `@`-assignment gists as `[...]`,
`@array[1]` reifies just enough, and `.eager` yields `[0 1 4]`.

`.eager` on a lazy list also learned to preserve array context: a lazy list
assigned into an `@` variable *is* that array's element store, so forcing it
must answer an `Array` (`[0 1 4]`), not the `List` gist `(0 1 4)` it used to
return. The `Sub` arm of `.lazy` is left in place for a genuine
`&callable.lazy`; nothing reaches it from the statement prefix any more.

Pinned by `t/lazy-gather-and-junction.t`.

## Known remaining divergence

`my @a = lazy {...}; @a.eager; say @a` still prints `[...]` in mutsu where raku
prints `[0 1 4]` — rakudo's `.eager` on an `Array` reifies the array *in place*,
while mutsu's returns a forced copy and leaves the variable lazy. That is a
property of `.eager`-as-a-mutator, not of the `lazy` prefix.
