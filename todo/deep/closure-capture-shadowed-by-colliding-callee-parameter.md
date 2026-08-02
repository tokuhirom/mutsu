# A caller's closure loses its capture to a same-named callee parameter

A closure passed into a routine keeps its own captured lexicals — until the
routine invokes it from a *nested block* that itself reads a parameter with the
same name. Then the closure's free variable resolves to the callee's parameter
instead of the value it closed over.

```raku
class Src { method kind() { "SRC" } }
class Dst { method kind() { "DST" } }

sub runner(&body, $d) { body() }

sub takes($s, :&after) {
    runner({ say "  callee \$s is ", $s.kind; after() }, "t")
}

{
    my $s = Src.new;
    takes(Dst.new, :after({ say "closure sees: ", $s.kind }));
}
```

```
$ raku                       $ mutsu
  callee $s is DST             callee $s is DST
closure sees: SRC            closure sees: DST
```

## What the repro isolates

All three ingredients are required; drop any one and the closure resolves
correctly:

1. **A name collision** — the caller's captured lexical and the callee's
   parameter are both `$s`. Rename the parameter to `$q` and the closure sees
   `SRC` again.
2. **A nested block** — invoking the closure directly from the callee's own body
   (`takes` calling `after()` itself) is correct. Only a call from inside
   `runner`'s body block leaks.
3. **The nested block reading the colliding name** — remove the
   `say "  callee \$s is ", $s.kind` and the closure sees `SRC`.

So the closure's capture survives (ingredient 2 proves it); it is *shadowed* at
invocation time by the frame the call happens in.

The **named** parameter form (`:&after`) leaks; the **positional** form
(`&after`) does not, which is what makes this hard to spot — the two differ only
in how the closure value reaches the callee, so the two paths must be attaching
or consulting the capture differently.

## Why it is large

The mechanism is the block-frame env layering (`captures_env_by_name`, the
closure upvalue capture in `capture_closure_env`, and the nested-block env
overlay) rather than any one call site — the same cluster ADR-0001 and the
`needs_env_sync` analysis sit on. Two narrow attempts made it *worse* rather
than better, both by widening what the callee frame publishes by name:

- Adding `GetCodeVar` to `compute_needs_env_sync`'s by-name-reader scan
  (`src/opcode.rs`) made the *positional* form leak too.
- Reading a `&`-sigil lexical from its local slot at **compile** time
  (`Expr::CodeVar` in `src/compiler/expr.rs`) also broke the positional form:
  a nested block shares the parent compiler's `local_map`, so the slot index is
  resolved against the wrong frame.

The shipped fix for the adjacent `&`-named-parameter bug
(`news/2026-08/named-callable-parameter-binds.md`) therefore reads the slot at
**runtime**, against the executing frame's own `code.locals`
(`exec_get_code_var_op`), and does not touch the env at all — which leaves this
bug untouched in either direction.

## What it blocks

Nothing outright today, but it is a live trap for any test using roast's
`Test::Tap::tap-ok`, whose first parameter is `$s`: a `:after-tap` closure over
a caller lexical also named `$s` silently operates on the *Supply* under test
instead of the Supplier that feeds it. `t/supply-unique-tap-ok-expires.t` hit
exactly that and its Supplier is named `$src` to avoid it; roast's own
`S17-supply` files escape only because they name their suppliers `$s1`/`$s2` or
never read the collided name from a nested block.
