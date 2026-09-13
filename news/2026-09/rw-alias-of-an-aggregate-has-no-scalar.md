# An `is rw` alias of an aggregate has no Scalar to box

Another measurement pass on [#7539](https://github.com/tokuhirom/mutsu/issues/7539)'s
`Config::TOML` + `Crane` battery pair turned up eight general interpreter bugs.
Both suites were re-fetched from upstream and re-run against a fresh build; the
raku oracle passes 34/34 on the same checkouts.

| Suite | raku | before | after |
| --- | --- | --- | --- |
| `Config::TOML` v0.1.3 | 19/19 files | 14/19 | 14/19 |
| `Crane` v0.1.2 | 15/15 files | 4/15 | 4/15 |

The file counts did not move, but the failing-assertion counts did, sharply:
`Crane`'s `set.rakutest` went from 5 failing subtests to 1, `remove.rakutest`
from 2 to 1, and `add`/`copy`/`move`/`replace`/`transform` each shed failures.
Every fix below is a general divergence from rakudo, each pinned by its own
regression test, each verified to fail without its fix and to pass under
rakudo v2026.07.

## 1. A sigilless alias of an aggregate was boxed into a Scalar that does not exist

Raku gives an `@`/`%` variable no `Scalar` container of its own. A sigilless
alias of one — `\c` bound to `%a` — *is* that aggregate: `c = {...}` is
`%a.STORE(...)`, a write every holder sees, and `return-rw c` hands the caller
the aggregate itself.

mutsu's `CaptureVarCell` boxed such an alias into a fresh scalar cell, modelling
a container Raku does not have. The caller then assigned into a cell owned by a
frame that had already returned, and the write reached nobody:

```raku
sub leaf(\c) is rw { return-rw c }
sub hop(\c)  is rw { return-rw leaf(c) }
my %a = :k(1);
hop(%a) = {:z(9)};   # rakudo: {:z(9)}   mutsu: {:k(1)}
```

One hop happened to work — a `%`-named argument never reached the boxing
branch — and two did not, because the second hop's argument is the sigilless
`c`. `exec_capture_var_cell_op` now declines for a real `Array`/`Hash`, the same
aggregate exclusion `exec_attr_container_ref_op` already makes, and
`assign_through_rw_result`'s existing "replace the aggregate's contents in
place" arm takes over. That arm was also factored out
(`store_into_aggregate_lvalue`) and wired into the **type-object rw-method**
lvalue path, which previously only accepted a `ContainerRef` and let the legacy
setter chain quietly drop the write — the root-path case of every `Crane.set` /
`add` / `replace`.

This is `Crane::In.in(container, @path) = $value`, which every mutating Crane
operation is built on.

## 2. A sigilless argument to an lvalue call carried no source name

`f(c) = 1` lowers to `__mutsu_assign_named_sub_lvalue("f", (c), 1)`. The
argument list is a List literal, whose elements are tagged with `WrapVarRef` so
the callee's raw parameter binds the caller's container — but only for
`Expr::Var`, the `$`-sigiled spelling. A **sigilless** lexical is
`Expr::BareWord`, which is also how a type name is spelled, so it was left
untagged: the callee's `\c` got a bare value with no source name, the binder
marked it a readonly non-lvalue, and the assignment died with
`Cannot modify an immutable Int (0)` — while the identical call written with
`$`-sigils worked.

`sigilless_local_container_name`'s `local_map` probe is what settles bareword
ambiguity, and it is now consulted at the List-element capture site too.

## 3. A sigilless parameter's declared type leaked into assignment

A `\c` parameter's declared type is checked once, when the argument binds; a
later write through the alias goes into the *caller's* container and is checked
against that container's constraint. mutsu registered the parameter's type in
the assignment-time lane as well, inventing a constraint rakudo does not have:

```raku
sub h(Associative \container) { container = Empty }
my $root = {:a(1)}; h($root);
# mutsu: Type check failed in assignment to $container; expected Associative but got Slip
```

`bind_param_type_constraint_sym` now takes the untyped path for a sigilless
name, which also keeps the shadowing behaviour the untyped case documents. This
is Crane's `remove-from-associative(\container, :in-place)`, which empties a
container with exactly that statement.

## 4. `deepmap` did not itemize a nested Hash

`deepmap` itemizes what a descend returns — the `Array` and `Seq` arms honoured
the flag, the `Hash` arm silently dropped it. `%(:x({:a(1)})).deepmap({$_})`
answered `{:x({:a(1)})}` where rakudo answers `{:x(${:a(1)})}`. Without the
itemization nothing can be bound to a mapped copy's nested hash, which is how
`Crane::At.at($root, @path){$step}:delete` on a `container.deepmap({ .clone })`
copy deleted from a temporary instead of from `$root`.

The itemization has to be the per-holder flag on the same `HashData` (`.item`),
not a `Scalar` wrapper: a wrapper hands back a copy nothing can mutate in place.

## 5. `:delete` on a call-result subscript deleted from a temporary

`<expr>{key}:delete` compiled the target onto the stack and ran the generic
`DeleteIndexExpr` over the popped value, so a `return-rw` call's result was
mutated as a temporary and the deletion vanished — while binding the identical
expression to a variable and deleting through *that* worked. The nested-subscript
case already had a temp-bind rewrite for this reason; it is now the generic
fallback, so `Crane::At.at($root, @path){$step}:delete` reaches the real
container.

## 6. `splice` ignored a `Callable` index on a by-value invocant

`splice`'s start/elems positions take `Int`, `Whatever` or `Callable`, and a
from-the-end index (`*-1`) is a `WhateverCode`. The lvalue path resolved it
against the array's length; the by-value invocant path (a function result, an
element read, a literal) did not, so the callable read as index 0 and
`f().splice(*-1, 1)` cut the array's **first** element. The two paths now share
one `resolve_splice_callable_args`.

## 7. `.clone` kept its invocant's itemization

Itemization is a property of the container, not of the object, and `.clone`
copies the object: `my $v = <a b c>; $v.raku` is `$("a", "b", "c")` but
`$v.clone.raku` is `("a", "b", "c")`, which is why `my @a = $v.clone` flattens
where `@a = $v` does not. mutsu carried the `ArrayKind`'s itemization through
verbatim. Crane's `Crane::In.in(container, @path) = $value.clone` depends on the
flattening.

## 8. A nested store into a `Pair` clobbered it — and destroyed the variable

A `Pair` does `Associative`, so rakudo descends into it on a nested store and
refuses at the value the next subscript reaches
(`my %h = :x(:y(1)); %h<x><y> = 2` is `Cannot modify an immutable Int (1)`).
mutsu refused on the *slot's* type with `X::AdHoc` "Type Pair does not support
associative indexing" — the class rakudo raises for a genuinely non-Associative
slot — and the deeper walk did worse: finding neither an Array nor a Hash to
step through, it overwrote the `Pair` with a fresh `Hash` and reported success,
rebuilding a whole colonpair chain as nested hashes.

Both nested-store ops also invalidate the target's local slot to `Nil` before
walking and refresh it from `env` when they finish. Returning early on a refusal
skipped that refresh, so a **failed** assignment left the caller's `%h` reading
`Nil` — the variable was destroyed by a store that was supposed to change
nothing. The restore now runs on the error path too.

Crane's `CATCH { when X::Assignment::RO }` maps the refusal to
`X::Crane::OpSet::RO`, and its fixtures are colonpair chains
(`:a(:pair(:is(:not(:a<hash>))))`), so both halves mattered.

## 9. `{ :when(...) }` parsed as a Block

A statement can never start with a `:`, so a block body whose first token is a
colonpair is unambiguously a hash composer — whatever the pair's key spells.
mutsu excluded a list of statement keywords there (`when`, `if`, `for`, `my`,
`return`, …), so `{:when(1)}` and eleven others parsed as `Block` where rakudo
answers `Hash`. `Crane`'s `t/remove.rakutest` builds a literal
`:me({:when({:im(7)})})`, and the `when` entry alone turned a nested hash into a
closure, aborting the file.

## Pins

- `t/vm/binding/rw-alias-of-an-aggregate-has-no-scalar.t` (1, 2, 3)
- `t/collections/transform/deepmap-nested-hash-itemization.t` (4)
- `t/collections/subscript/delete-through-a-call-result-subscript.t` (5)
- `t/routines/splice-callable-index-by-value-invocant.t` (6)
- `t/oo/construct/clone-decontainerizes-its-result.t` (7)
- `t/collections/range-pair/pair-subscript-store-is-refused.t` (8)
- `t/lang/adverbs/colonpair-keyword-key-hash-composer.t` (9)

## What is left on #7539

`Crane` is still 4/15 files, so the vendoring steps stay parked. The residue:

- **`Crane::In`'s descent through a `Pair` is still writable.** The direct
  subscript spelling now refuses, and so does a hand-written recursive
  `inn(c{@s[0]}, @s[1..*])`, but the real `Crane::In.in` — reached through a
  class method with a `*@steps` slurpy and `Associative:D`-constrained multis —
  still promotes the Pair's element to a writable location, so `set`'s two
  "operation fails when … is immutable" subtests do not throw. That is
  `set.rakutest`'s only remaining failure.
- **`X::Assignment::RO` on a Pair names the wrong Pair.** The refusal fires at
  the first Pair the walk meets rather than descending to the leaf, so a
  four-level chain reports the outer pair's value where rakudo reports the
  innermost. The class is right; only the rendered value differs.
- `flatten` / `list` are still the object-hash blocker
  ([#7539](https://github.com/tokuhirom/mutsu/issues/7539)'s item 5), and
  `add` / `copy` / `in` / `move` / `patch` / `transform` each have their own
  positional/error-handling residue.
- `Config::TOML`'s `grammar-actions/01`, `02`, `04` and the two dumper files are
  untouched.
