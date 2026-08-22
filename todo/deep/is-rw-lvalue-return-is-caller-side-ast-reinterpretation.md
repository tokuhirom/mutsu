# `is rw` routines do not return an lvalue — the caller re-interprets the callee's AST instead

## Symptom

A routine declared `is rw` cannot hand its caller a writable container element
whenever that element is reached through one of its own parameters:

```raku
sub g(\c) is rw { return-rw c<a> }
my %h;
g(%h) = 1;          # raku: %h is {:a(1)}
                    # mutsu: X::Assignment::RO: sub 'g' is not rw
say %h.raku;
```

Every variation fails the same way — `sub g(%c) is rw { return-rw %c<a> }`,
`sub g() is rw { return-rw %h<a> }` (captured outer), and the recursive form
Crane uses. Only the degenerate `sub g() is rw { $scalar }` shape, where the
tail expression names a variable the *caller* can also see, works.

The method form fails differently and even more confusingly:

```raku
class I { method in(\c, *@s) is rw { return-rw c{@s[0]} } }
my %h;
I.in(%h, "a") = 1;  # mutsu: "I cannot be parameterized"
```

## Root cause

There is no lvalue return. `assign_named_sub_lvalue_with_values`
(`src/runtime/builtins_lvalue.rs`) does not call the routine and assign to what
it returns; it takes the callee's **AST tail expression** —
`rw_sub_target_expr(&def.body)`, literally the last `Stmt::Expr`/`Stmt::Return`
of the body — and re-evaluates that expression *in the caller's frame* via
`assign_rw_target_expr`.

That works only when the tail expression happens to resolve identically in the
caller's scope. It cannot work for the general case:

- The tail names a **parameter** (`c`, `%c`, `@steps`) — the caller's frame has
  no such binding, so the re-evaluation resolves nothing.
- The tail is `return-rw <non-Var>`: `rw_sub_target_expr` hands back the whole
  `Call { name: "return-rw", args: [...] }` node, `is_explicit_return_rw_target`
  recognizes only `return-rw $var` / `$var.return-rw`, and
  `assign_rw_target_expr`'s `Expr::Call` arm then tries to dispatch `return-rw`
  as an *lvalue sub*, which fails and falls through to "sub is not rw".
- The tail is computed (a conditional, a loop, a dispatch to another `is rw`
  multi) — there is no single static expression to re-interpret at all.

So the mechanism is a lookalike, not an implementation: it reproduces the
*syntax* of the common one-liner cases and has no way to express the semantics.

## Why this is `deep/`, not a ticket

The real fix is for an `is rw` routine to **return a container** — a
`ContainerRef` (or `Proxy`) pointing at the element — and for lvalue assignment
to write through it. That touches:

- the return path (a `return-rw`/`is rw` return must not decontainerize),
- `ContainerRef` deref coverage (CLAUDE.md's Track B / ADR-0013 territory —
  every reader of the returned value must see through the cell),
- autovivification (Crane's recursion writes through `c{@s[0]}` on a hash that
  does not have that key yet, and each recursive step must vivify),
- multi-dispatch (`is rw` multis: `Crane::In`'s `in` has 8 candidates
  distinguished by `where` clauses on the step list),
- and the `Expr::Call`/`MethodCall` lvalue arms, which should stop
  re-interpreting ASTs once the container path exists.

It is very likely worth an ADR, and it should supersede
`rw_sub_target_expr`/`assign_rw_target_expr` rather than sit alongside them —
leaving both means two mechanisms disagreeing about which one owns a given
shape.

## Why it matters

`Crane` — the sole dependency of `Config::TOML`, the selected TOML battery
(`docs/batteries/toml.md`) — is built entirely on this. `Crane::In.in` returns
`return-rw container{@steps[0]}` recursively and `Crane::Set.set` writes
`Crane::In.in(container, @path) = $value`. With no lvalue return, every
`Crane.set` silently does nothing:

```raku
use Crane;
my %h;
Crane.set(%h, :path["a","b"], :value(1));
say %h.raku;   # raku: {:a(${:b(1)})}   mutsu: {}
```

which is why `Config::TOML`'s `from-toml` currently returns an empty hash even
though its grammar parses the document correctly. This is now the **largest
remaining blocker** for the TOML battery slot.

`Crane` is not an unusual consumer: returning a writable element is the
idiomatic Raku way to write a path-addressing container library, and `is rw`
accessors are common in ordinary classes too.

## Minimal repros

```raku
# 1. parameter-reached element
sub g(\c) is rw { return-rw c<a> }
my %h; g(%h) = 1; say %h.raku;              # want {:a(1)}

# 2. recursive autoviv (the Crane shape)
sub g(\c, @s) is rw {
    @s.elems > 1 ?? return-rw g(c{@s[0]}, @s[1..*]) !! return-rw c{@s[0]}
}
my %h; g(%h, ["a","b"]) = 1; say %h.raku;   # want {:a(${:b(1)})}

# 3. method form
class I { method in(\c, *@s) is rw { return-rw c{@s[0]} } }
my %h; I.in(%h, "a") = 1; say %h.raku;      # want {:a(1)}
```
