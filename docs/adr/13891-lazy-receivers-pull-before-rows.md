# ADR-13891: A lazy receiver's methods run through per-method pulls

- **Status**: Accepted (2026-10-19)
- **Date**: 2026-10-19
- **Deciders**: tokuhirom, Claude
- **Issue**: [#13891](https://github.com/tokuhirom/mutsu/issues/13891)
- **Related**: [ADR-11276](11276-built-in-methods-are-handler-rows.md) (built-in methods are
  handler rows; §9.178 records this slice), ADR-0038 (the `Seq` body and its consumption step)

## 1. Context

A `LazyList` is mutsu's representation of a `Seq` (or a lazy `List` or `Array`) whose iterator
has not run to its end: an infinite sequence, a lazy `map`/`grep` pipeline, a `gather` still
being pulled, a lazy `@`-array. It reports the type it stands for (`value_type_name`: `Seq`,
`List` or `Array`); Rakudo has no class for it.

Until this ADR, `call_method_with_values_inner` answered a `LazyList` through five blocks that
matched method names:

| block | names | what it did |
|---|---|---|
| placeholders | `gist`, `Str`, `raku` | `(...)` / `[...]` / `...`, or `Seq.raku`'s bounded prefix |
| cache | `cache` | a `List` view over the same iterator |
| partial force | `AT-POS`, `EXISTS-POS` | pull up to the index, then the array's method |
| force list | 50 names (`should_force_lazy_list`) | reify the list, re-dispatch on a `Seq` |
| join | `join` | `...` for a lazy list, else reify |

None of them could become a row. A `LazyList` has no `DispatchShape`, and the rows that answer
these names already exist for the *settled* receivers: `Seq`'s rows (reached after the `Seq`
consumption step, `Receiver::of_settled`) and `List`'s and `Array`'s (pure rows, reached by the
pure entries and the call-site lane). A row is `(owner, name, arity)`, and the table lets the
first row for a key win, so a second `Seq.AT-POS` row for the lazy case cannot be registered.

## 2. Decision

**A lazy receiver goes through the *lazy step* before any row.** The step is the twin of the
`Seq` consumption step (`reify_or_consume_seq_target`): it decides what one call does to the
iterator, and leaves the method itself to the rows.

1. **A pull** is the part of a method that runs on a receiver before it is reified. It is
   registered, like a row, by the owner Rakudo declares the method on
   (`method_table::lazy_pull::PULLS`: `Seq`, `List`, `Array`, `Any`), and found by walking the
   MRO of the type the `LazyList` reports, most-derived first. A pull either answers from what
   it pulls (`AT-POS` pulls up to its index and hands the prefix to the array's own `AT-POS`;
   `cache` answers a view over the iterator; `gist` of an infinite sequence is `(...)`) or
   declines.
2. **The reify table** (`methods_lazy_step.rs`, the former force list) names the methods that
   need every element. A call that no pull answered reifies the receiver and is re-dispatched on
   a settled `Seq` of its elements, whose rows answer it. The exceptions the force list had stay
   with it: a `map`/`grep` that appends a lazy stage, and a laziness-preserving coercion.
3. Any other call goes on to the later steps unchanged.

A pull is not a second implementation of its method: it decides how much of the iterator the
method reads, then hands that to the method's row (or answers the placeholder that *is* the
method's answer on an iterator that may not end, as Rakudo's `Seq.gist`/`Seq.join` do). The
check `pulls_are_declared_by_rakudo` holds every pull to an owner Rakudo declares the method on.

## 3. Consequences

- The five by-name blocks are gone from `call_method_with_values_inner`; the step is one call.
- A lazy `@`-array's `AT-POS` is `Array`'s pull, a lazy `List`'s `gist` is `List`'s: the answer
  follows the type the receiver reports, as the method would in Rakudo.
- The reify table is still a list of names. Turning it into "every method the reported type has
  a row for and no pull" needs each name audited on an infinite sequence first: Rakudo answers
  `Bool`, `is-lazy` and `lazy` without reifying and throws `X::Cannot::Lazy` from `elems` and
  `sort`, which a blanket reify would hang on. That audit is the next slice; each name it
  settles either gets a pull or joins the rows' reify rule.
- No behaviour change: every answer of `t/collections/lazy-seq/lazy-receiver-pulls.t` matched
  before and after, and matches Rakudo.

## 4. Not done here

- The streaming adaptors (`skip`, `rotor`, `batch`, `unique`, `repeated`, `squish`, `produce`,
  `flat`: `try_lazy_adaptor_method`) and the index pipes (`pairs`, `antipairs`, `kv`:
  `index_pipe_method`) run ahead of the step and keep their own name dispatch, because they also
  serve an infinite `Range`, which is not a `LazyList`. They become pulls when the unbounded
  `Range` receiver goes through the same step.
- The VM's `CallMethodMut` lanes keep their own `LazyList` handling until they route through
  the step.

## 5. Alternatives rejected

- **An internal owner and shape `LazyList`** with `Handler::Interp` rows reached from
  `answer_shapeless`. Rakudo has no such class; a row's owner is what `.^can` and
  `.^methods` read, and the rows would be a second implementation next to `Seq`'s.
- **The settled rows take a `LazyList` too.** Pulling a `gather` coroutine needs the
  interpreter, so `List`'s and `Array`'s `AT-POS`, `join` and renderings would become
  `Handler::Interp` rows, and every plain list would lose its pure answer (the pure entries,
  constant folding, the call-site lane's pure rows).
- **A second `lazy` handler field on `MethodRow`.** Every row literal would change for the
  dozen rows that need one; a side table keyed by the declared owner says the same.
