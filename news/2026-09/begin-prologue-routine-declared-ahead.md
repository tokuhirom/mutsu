# A nested BEGIN behind a routine declared ahead of it now runs at BEGIN time

A `BEGIN` nested in a scope that had already declared a `sub` was not lifted into the unit's BEGIN
prologue (ADR-0134 slice 2, #10329). It kept its pre-ADR handling, so it ran only when the
enclosing scope ran, and every later nested BEGIN in the unit stayed on that path with it:

```raku
sub f { sub helper { 1 }; BEGIN say "after-helper" }; say "m"
# raku:  after-helper, then m
# mutsu: m   (now: after-helper, then m)
```

The prologue runs before the scope exists, so the routine did not exist there either. A lifted
BEGIN now gets a copy of each routine it calls. The lifted body runs in a block that declares the
routine again, over the same static cells as the variables the routine closes over, so
`{ sub my-uc($x) { $x.uc }; BEGIN { $r = my-uc 'Ab' } }` sees `AB` at true BEGIN time (it used to
pass only through the in-block reorder). The routine's own declaration stays in place, so every
frame of the scope still gets its own.

The routines a BEGIN needs are found from the compiled body: the names it calls, `&name` reads, and
then, transitively, whatever those routines read and call. A routine sees only the bindings that
preceded its own declaration, and the block for each scope nests as the scopes do, so shadowing
comes out as it does in place. A body that can reach a name dynamically (`EVAL`, `CALLER::`,
symbolic lookup) in a scope that declares routines still keeps the old handling, since it cannot say
which ones it needs.

Still not lifted, because the prologue cannot reproduce them yet: a type, package, import or
`my &code` declared ahead of the BEGIN (#10394), and a `multi`, `our sub`, operator or exported
routine (#10395).
