# ADR-14477: A code value's methods are found through its class's MRO

- **Status**: Accepted (2026-10-22)
- **Date**: 2026-10-22
- **Deciders**: tokuhirom, Claude
- **Issue**: [#14477](https://github.com/tokuhirom/mutsu/issues/14477)
- **Supersedes**: the outcome of [ADR-12523](12523-code-methods-need-a-dispatch-shape.md) §7
  ("No `DispatchShape::Code`")
- **Related**: [ADR-11276](11276-built-in-methods-are-handler-rows.md) (handler rows),
  #14081 (the per-site owner hooks; `routine_owners_of` is its last row), #14080 (the receiver step)

## 1. Context

In Rakudo a code object is an instance of an ordinary class, and a method call on it walks that
class's MRO like any other call:

| class | MRO (below `Any`, `Mu`) |
|---|---|
| `Sub`, `Method`, `Submethod` | `Routine` → `Block` → `Code` |
| `Regex` | `Method` → `Routine` → `Block` → `Code` |
| `Block` | `Code` |
| `WhateverCode` | `Code` |
| `Sub+{R}` (a mixin) | `Sub` → `Routine` → `Block` → `Code` |

Each method is declared on a specific class in that chain. `Code` declares, for example, `arity`,
`count`, `signature`, `of`, `returns`, `name`, `line`, `file`, `Str`, `Capture`, `clone`,
`assuming`, `cando`, `set_name` and `is-implementation-detail`. `Block` declares `of`, `returns`,
`clone` and `raku`. `Routine` declares, for example, `multi`, `dispatcher`, `is_dispatcher`,
`candidates`, `package`, `yada`, `onlystar`, `soft`, `wrap`, `unwrap`, `gist` and `raku`.
`Method` and `Regex` add `gist` and `raku`. `Block.multi` therefore dies with
`No such method`, and `Regex.multi` answers.

mutsu holds a code object as one of several value kinds: `ValueView::Sub` (a routine, a closure, a
bare or pointy block, a `method` value or a `WhateverCode`, told apart by `__mutsu_callable_type`
and `is_bare_block`), `WeakSub`, a `Routine { package, name, is_regex }` handle, `Regex` /
`RegexWithAdverbs`, an `Instance` of class `Method` (a method object from `.^lookup`), and a mixin
around one of these. `type_name` already decodes every one of them to its Rakudo class in O(1),
and `.^mro` and `.^can` agree with Rakudo.

The method dispatch does not use that class. Instead, owner lists are spelled out at each call site:

- `routine_owners_of` (`method_table/instances/routine.rs`) answers `["Routine", "Code"]` for
  *every* `Sub` value and `Routine` handle, blocks included. Two call sites ask it
  (`methods_sub.rs`, `methods_instance_ops.rs`).
- `code::answer` gates on a hand-kept list of ten method names and then always looks under
  `["Code"]`; `pure_answer` keeps a second list.
- `code_accepts_owners` keeps a third list of owners for `ACCEPTS`, and `METHOD_OBJECT_ROWS`
  (`code_methods.rs`) a fourth, for `Instance` method objects only.
- `canonical_builtin_owner` folds `Sub | Method | Block | Routine | Code` into `Code`, so the
  recognition table cannot say that `Routine` and not `Block` declares a method.

ADR-12523 asked whether `Code`'s rows need a `DispatchShape`. It concluded they do not, because
none of them is wanted by the pure entries, and ADR-12523 §3 counted the call-site memo as 38 of
64 bits. That answered a question about speed. It did not ask whether the owner of a code value
should follow from its class, and the per-kind owner lists above give wrong answers
(mutsu 08349b95 against Rakudo 2026.07, #14477):

- A block, a pointy block and a `WhateverCode` answer `Routine`'s `multi`, `dispatcher`,
  `is_dispatcher`, `package`, `candidates` and `yada`. Rakudo dies, and mutsu's own `.^can`
  says False.
- A `Regex` dies on the methods it inherits from `Routine` and `Code` (`multi`, `dispatcher`,
  `package`, `candidates`, `yada`, `assuming`, `cando`, `is-implementation-detail`), although
  mutsu's own `.^can` says True.
- `Code.cando` dies on every `Sub` value. Its row is reachable only from the `Instance`
  method-object list.

The bit budget ADR-12523 counted has also moved on: the table keeps one bit per shape in a `u128`, and
about 98 shapes exist.

## 2. Decision

1. **A code value's owners are its class's MRO.** The receiver decodes to the built-in code class
   it is (`Sub`, `Method`, `Submethod`, `Block`, `Regex`, `WhateverCode`), with the same decode
   `type_name` uses, so the answer cannot drift from `.^name`. One closed shape per class names
   its owner chain, nearest first, the way opaque shapes do (`value/dispatch_shape/opaque.rs`):
   `Sub → [Sub, Routine, Block, Code]`, `Regex → [Regex, Method, Routine, Block, Code]`,
   `Block → [Block, Code]`, `WhateverCode → [WhateverCode, Code]`, and so on. Six shapes fit
   the `u128` with room to spare.
2. **Rows are registered on the class Rakudo declares them on.** `canonical_builtin_owner` stops
   folding the five code class names into `Code`, and the recognition table states each
   declaration on its real owner: `multi` on `Routine`, `of` on `Code` and on `Block`, `raku` on
   `Block`, `Routine` and `Regex`, and so on. A row that exists today under the wrong owner moves.
   It is not duplicated.
3. **One lookup replaces the per-site lists.** `routine_owners_of`, the method-name gates of
   `code::answer` and `pure_answer`, `code_accepts_owners` and `METHOD_OBJECT_ROWS` are deleted,
   together with their two call sites. Every dispatch entry reaches a code value's rows through its
   shape, like any other shaped receiver.
4. **A receiver with another class name has no shape.** For a mixin (`Sub+{R}`) or a user
   subclass of a code class, its own methods and its roles' methods are found first, through the
   ordinary user-method path. After that, it reaches the rows of the built-in class it derives from
   through the receiver step (#14080), the same rule a user subclass of any shaped built-in follows.
5. **A method object is the same class.** An `Instance` of class `Method` (`.^lookup`,
   `.^find_method`) decodes to the `Method` shape through `DispatchShape::from_instance_class`.
   Each handler reads its data from whichever representation it is given.

## 3. Consequences

- The wrong answers in §1 are fixed by construction: `Block.multi` finds no row and dies, and
  `Regex.multi` finds `Routine`'s. `.^can` and the dispatch read the same chain.
- `Routine.onlystar` and `Routine.soft`, which mutsu lacks entirely, become ordinary rows on
  `Routine` with no special wiring.
- Adding a method to a code class is one row on one owner. No list at a call site has to agree with
  it.
- The rows stay `Handler::Interp` where they read the registry or a closure's scope. A shape does not
  make a row pure; it only makes the row reachable from every entry.
- `methods_sub.rs`'s by-name arms that implement declared methods are absorbed by rows as the slices
  land. Arms for names Rakudo does not declare on a code class are left alone.

## 4. Alternatives considered

- **Keep `routine_owners_of`, and fix it per kind** (return `["Code"]` for a block, add a regex
  branch). This fixes today's cases but keeps the structure that produced them: one more value kind
  or one more row means editing a list at each call site. Rejected.
- **One `DispatchShape::Code` whose handler decodes the kind** (ADR-12523 §2's original proposal).
  Every row would then have to re-decide which class the receiver is to know whether it may answer.
  That repeats the MRO inside each handler. Rejected in favour of one shape per class.

## 5. Open questions

1. Which `Any`/`Mu` rows a code shape should reach (ADR-11276 §9.17's audit), or whether it starts
   closed like the opaque shapes and leaves them to the default object protocol.
2. How a handler reads an `Instance` method object versus a `Sub` value. One accessor per field
   would keep the handlers single, but how many such accessors are needed is not yet measured.
3. Whether `WeakSub` decodes to `Sub` unconditionally, as `type_name` does today.
