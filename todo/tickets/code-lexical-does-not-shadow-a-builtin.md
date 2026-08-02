# A `&`-sigil lexical does not shadow a builtin of the same name

```raku
my &emit = { "e" };
sub f() { my @out = 1; @out.push(emit()); @out }
say f().raku;
```

```
$ raku            $ mutsu
[1, "e"]          Nil
```

The bare call `emit()` reaches mutsu's **builtin** `emit` (the `gather`/`supply`
one) instead of the lexical `&emit` that is plainly in scope. Because that
builtin is a control-flow construct, the call does not merely return the wrong
value — it hijacks the enclosing routine, which returns `Nil`.

Every way of introducing the lexical is affected, so this is about name
resolution for a bare call, not about any one declaration form:

```raku
my &emit = { "e" };            # a `my` binding
sub f(&emit)      { emit() }   # a positional Callable parameter
sub f(:&emit)     { emit() }   # a named Callable parameter
```

`&emit()` — the explicitly sigiled call — is wrong in the same way.

## Why it matters

`emit` and `done` are exactly the two names roast's
`Test::Tap::tap-ok` takes as `:&emit` / `:&done` and invokes as `emit() if
&emit`. It escapes there only because those calls sit inside the tap callback
block, whose value is discarded. Any consumer that needs the *return* of such a
call gets `Nil` and, worse, loses the rest of its routine body.

The general rule is the one already established for qualified calls in
`news/2026-07/qualified-call-no-longer-aliases-a-builtin.md`: resolve on
**whether a declaration exists**, not on whether the name happens to be a
builtin. A lexical `&name` in scope is a declaration and must win.

Found while fixing the adjacent named-Callable binding bug
(`news/2026-08/named-callable-parameter-binds.md`); the pin
`t/named-callable-param.t` deliberately avoids `emit`/`done` for this reason.
