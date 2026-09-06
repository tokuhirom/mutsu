# `$.attr = v` now consults the accessor, instead of writing past it

Inside a method, `$.attr = v` is not a write to the attribute slot. Raku
evaluates `self.attr` and assigns to whatever that hands back, so for a non-`rw`
attribute it hands back a bare value and the assignment dies:

```raku
class F { has $.x = 5; method m { $.x = 9 } }
F.new.m;   # raku: X::Assignment::RO: Cannot modify an immutable Int (5)
```

mutsu wrote `9` into the attribute and carried on. `is rw` was never consulted at
all, in either statement or expression position — a public accessor's read-only
contract was simply not enforced from inside the class.

## Why it slipped through

`Compiler::compile_expr_assign`'s `$.attr` arm (`src/compiler/expr_data.rs`) does
call the accessor — but only to check that it *exists*. It emits `CallMethod`,
immediately `Pop`s the result, and then performs an ordinary **named** assignment
to the variable `.attr`, which lands on the attribute. The accessor's return
value, and with it its mutability, was thrown away one instruction after it was
computed.

The check now lives at the store instead of in the compiler
(`Interpreter::check_dot_twigil_accessor_writable`, `src/vm/vm_misc_assign.rs`),
because rw-ness is a property of the *invocant's class*, which only the runtime
knows. It reports raku's own wording — `Cannot modify an immutable Int (5)`, the
accessor's return type and value — rather than mutsu's older
`method 'x' is not rw`.

## Scoped to the `$` sigil, deliberately

Measured against raku v2026.07: for a non-`rw` `has @.a` and `has %.h`,

```raku
@.a = 7, 8;      # succeeds
@.a[0] = 99;     # succeeds
@.a.push(3);     # succeeds
%.h<k> = 99;     # succeeds
```

all work without `is rw`, because those accessors hand back the container itself
and assigning *into* a container is a `STORE`, not a modification of an immutable
value. Only the scalar accessor refuses. `t/dot-twigil-accessor-readonly.t` pins
both halves, plus the `is rw` and `$!` forms that must stay unaffected.

Blast radius turned out to be zero: of the 138 `t/` files whose grep matched
`$.name =`, every one was either a `has $.x = default` declaration, an `is rw`
attribute, or a write from a `sub` with no invocant.

## What is left

The **compound** form is still divergent, now in the safe direction. raku treats
`$.x *= 2` on a non-`rw` accessor as a silent no-op — the `$` sigil itemizes the
accessor's bare return into a throwaway `Scalar`, the multiplication lands there,
and the attribute is untouched — while mutsu refuses. Before this change mutsu was
*inconsistent* about it: the bare-statement form threw, and the
expression/parenthesized forms silently over-mutated the attribute, because the
two spellings take entirely different lowerings (`MethodCall` on `__ANON_STATE__`
versus `AssignExpr { name: ".x" }`). Now all four spellings refuse, which is
coherent and never loses a write.

Closing the remaining gap needs the `$.`-versus-`self.` origin to survive both
lowerings, which is a parser and compiler change rather than a runtime one. The
map — including why the obvious `do { my $t = self.x; $t OP= v; $t }` desugaring
would break every `is rw` compound assign — is in
`todo/deep/dollar-dot-attr-compound-assign-spurious-ro-error.md`, retitled to
match what is actually left.
