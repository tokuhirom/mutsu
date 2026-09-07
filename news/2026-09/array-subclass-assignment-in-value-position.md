# An array assignment decomposes an `is Array` subclass in every position

```raku
class SA is Array { }
my @a := SA.new(3, 2, 1, 4);

my @c = @a;              # statement position
say @c.raku;             # raku: [3, 2, 1, 4]   mutsu: [3, 2, 1, 4]   -- correct

say (my @b = @a).raku;   # value position
                         # raku: [3, 2, 1, 4]   mutsu: [[3, 2, 1, 4]]  (before)
```

The statement-position rule landed with
`news/2026-09/array-subclass-iterator-override.md`; the same assignment written
where its result is consumed compiles to a different store and had none of it.

## The rule now lives in one place, read by four stores

`Interpreter::array_assign_decomposed_instance` is the single answer to "does an
`@`-assignment of this instance distribute its elements?" — a `does Iterable`
class with its own `iterator` populates the array through it, and an
`is Array`/`is List` subclass distributes its backing storage, with the
override winning over the storage exactly as a user method wins anywhere else.

Three stores now call it, where only the first had the rule inline:

- `SetLocal` — statement position (`my @c = @a`),
- `SetGlobal` — a declaration whose result is consumed (`say (my @b = @a)`),
- `AssignExprLocal` — an assignment to an already-declared local in the same
  position (`say (@d = @a)`).

## The mirror half: a scalar-held instance must NOT decompose

The ticket named `my $c = SA.new(...); my @h = $c` as a control that must stay
one element. It did not: the statement-position rule decomposed it too
(`4` where rakudo says `1`), and the value-position form was right only by
accident of having no rule at all. Both halves needed the itemization signal.

An instance is Positional but has **no itemized container kind of its own** —
precisely the situation `OpCode::ItemizeVar` already handles for `Set`/`Bag`/
`Mix` and `Range` by wrapping them in a `Scalar` on the `@a = $var` path. A
decomposable instance now takes the same arm, decided by the new structural
predicate `instance_decomposes_on_array_assign` (which, unlike
`try_iterable_instance_items`, does not drive the iterator, so it is safe to ask
before deciding whether to itemize).

That opcode was also missing from the value-position compiles entirely, so
`emit_array_target_itemize` was split out of
`compile_assignment_rhs_for_target` and is now emitted by the expression-position
`@` declaration, the expression-position `Stmt::Assign`, and the `AssignExpr`
path as well.

## Why the itemization did not break the receiver

An earlier attempt (recorded in the ticket's own re-scoping, 2026-09-07)
concluded this direction was blocked: it itemized in
`Interpreter::itemize_scalar_store_value`, so the value the SCALAR ITSELF holds
became a `Scalar` wrapper, and every `ValueView::Instance` test downstream then
missed it — `$c.elems` answered 1, `$c.join("-")` answered `SA()`, and `for $c`
gisted the wrapper. Three separate `delegates_to_array_storage` call sites plus
the `for`-loop source and the stringifier each test the view directly, so a
wrapped receiver is invisible to all of them.

`OpCode::ItemizeVar` is a different place, and that is the whole difference: it
wraps the value **being assigned into the array**, on the `@a = $var` path only,
which is exactly where `Set`/`Bag`/`Mix` and `Range` are already wrapped. The
scalar keeps holding the bare instance, so no receiver, loop source or
stringifier ever sees a wrapper. Measured against the attempt's own
counter-examples: `$c.elems` is 2, `$c.join("-")` is `1-2`, `for $c` gists
`[1 2]`, and `$c.push(3)` works — all matching rakudo.

The one row that representation would also have fixed is `$c.raku`, which is
`$[1, 2]` in rakudo and `[1, 2]` here: a scalar-held container's `$` marker. It
is a rendering property of the holder, not of the assignment, and is left to
`todo/tickets/one-element-array-raku-omits-comma-for-subclass-element.md`
alongside the other `.raku` row.

## Measured against `raku`, all matching

Statement, `(my @b = ...)` and `(@d = ...)` forms for a plain subclass and for
one with an `iterator` override; the scalar-held form in all three positions for
both; and the controls — a plain `Array` in a scalar (one element), an `Array`
literal (flattens), and an explicit `@$c` deref (decomposes).

## Split off, deliberately

One row of the matrix is a rendering nuance rather than an assignment one and is
filed as
`todo/tickets/one-element-array-raku-omits-comma-for-subclass-element.md`:
`.raku` of a 1-element array omits rakudo's trailing comma when the element is
an `is Array` subclass instance (`[[3, 2, 1, 4]]` vs `[[3, 2, 1, 4],]`). The
arity, the element and its own `.raku` all agree; a plain `Array` element
renders the comma correctly.

## Testing

New `t/array-subclass-value-position-assign.t` (14 assertions), which passes
unchanged under rakudo. `t/array-subclass-iterator-override.t` is untouched, and
the 84 `t/array*.t` / `t/list*.t` / `t/itemize*.t` files (1019 assertions) pass.
