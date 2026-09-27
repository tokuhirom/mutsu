# Math::Symbolic loads: four general fixes found by its operation table

The `Math::Symbolic` distribution was `blocked_load`: none of its modules
would `use`. Getting it to load and run its suite exposed four general
interpreter bugs, all fixed in the same change:

- **Word lists inside hash composers.** `{ :type<postfix>, :parts< .abs > }`
  parsed as a Block, because the lexical hash-vs-block scan read the `.abs`
  inside the `< … >` word list as an implicit-topic method call. The scan now
  skips a word list wherever one can start (glued to a term, or in term
  position), while `$a < .elems` stays infix less-than. The scan moved out of
  `lambda.rs` into its own `topic_scan.rs`.
- **`for` over an rw accessor aliases the attribute.** `for $obj.attr <-> $v
  { $v = … }`, the `$_` topic form, and the dynamic spelling
  `for $obj."$name"() <-> $v` now write through to the attribute, exactly as
  `my $c := $obj.attr` does. That bind also works for the dynamic spelling now.
  Both ask the accessor for its container (ADR-0067).
- **Loop parameters are never package variables.** Inside a `package`/`module`/
  `unit class` body, a write to a `for` loop parameter was package-qualified
  (`$P::v`), so `for @a <-> $v { $v = 9 }` silently left `@a` unchanged there.
- **Junction eigenstates are values.** After `grep`/`first` aliased `$_` to an
  array's elements, `any(@a)` / `@a.all` held the shared element cells, and a
  method threaded over the junction died with "No such method". `Value::junction`
  now decontainerizes its eigenstates.

The distribution now loads and its test file runs. The remaining failures are
tracked as issues.
