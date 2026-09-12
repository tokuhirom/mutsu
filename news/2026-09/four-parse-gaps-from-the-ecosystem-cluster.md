# Four parse gaps out of the ecosystem's largest cluster

[#7988](https://github.com/tokuhirom/mutsu/issues/7988) is the parity ledger's biggest single
block: 99 distributions that fail to parse something, described only by the set of things mutsu's
parser would have accepted. Two earlier runs fixed the *diagnosis* (the error now names the line
the parser actually stopped on) and surveyed the remainder into roughly forty distinct constructs.
This one works four of them, and every one turned out to be the same shape of mistake — a
capability mutsu already had, restated in a second place with less of it.

## The dotted subscripts are the undotted ones

`.[...]`, `.{...}`, `.<...>`, `.<<...>>` and `.«...»` are the dotted spelling of the very same
postcircumfix subscripts. The postfix loop restated a weaker copy of each after a `.` instead of
using the branches that already parse them, so everything the copies lacked was silently missing
after a dot:

- semicolon dimensions: `$c.[0; 1]` answered `([1 2] [3 4])` where rakudo answers `2`, because the
  dotted arm called `parse_bracket_indices` — which collapses a `MultiDim` to an `ArrayLiteral` —
  rather than `parse_bracket_indices_inner`;
- the zen slice `.<>`: the dotted angle arm demanded a non-empty key set, so `%h.<>`, `@a.<>` and
  `$x.<>` did not parse at all;
- nested-angle keys (`%h.<a<b>>`) and the interpolating `.<<$k>>` / `.«$k»` spellings.

Two of those — the semicolon dimensions and `.<>` — were fixed on `main` while this work was in
flight ([#8155](https://github.com/tokuhirom/mutsu/issues/8155)), by *growing* the copies: a shared
`dotted_subscript_expr` for the two bracket arms, and a third arm for the literal `<>`. That
version says so itself ("adverbs on the dotted form (`%h.<>:k`) are still unhandled, as they are
for every other dotted subscript"), which is the argument for the other direction. Deleting the
copies and rewinding to the opener fixes all five spellings at once, and needs no helper to keep
two arms from drifting apart, because there are no longer two arms. One thing the
dot really does change had to come along: `Type{...}` is the object-constructor shorthand while
`Type.{...}` is a postcircumfix call on the type object (roast's `Mu.{'a'}`), and the shorthand
branch sits earlier in the loop than the postcircumfix one, so a one-iteration flag rides the
rewind to tell them apart. Nothing else needed carrying: the `.` just consumed is part of the span
the postfix loop measures, so the term no longer ends on whitespace and `.{...}` still reads as a
subscript rather than a block.

Game::Entities 0.1.6 (`.[COMPONENTS; $i].<>`) goes `blocked_load` → loading.

## An anonymous destructuring parameter may carry a type

`-> Pair (:key($k), :value($v))` is a typed anonymous parameter that unpacks its argument. Only the
*named* spelling `-> Pair $p (:$key)` parsed: both block-parameter parsers keep a type constraint
only when a **sigil** follows it, so with a bracket after the type they backtracked to the untyped
path, found a bareword where a parameter belongs, and the whole pointy block — or `for` header —
failed with no branch having matched. `parse_pointy_param` now delegates the shape to the
sub-parameter parser, exactly as its `:(...)` branch already does, and the `for` header consumes
the type ahead of the bracket and hands it to the unpack parameter, so each iteration value is
type-checked. Config::BINDish and Red both failed to load on it.

While there, a provably unreachable second `if r.starts_with('(')` branch in `parse_for_params`
came out: the first one returns on every path and propagates its errors with `?`.

## A topic method call may combine a modifier with a quoted name

`.?"$name"()` is the topic spelling of `$x.?"$name"()`, which has always parsed. The topic parser
tried its quoted-method-name branch *before* consuming the `?` / `^` modifier, so with both present
no branch matched and the enclosing block failed to parse. Consuming an optional modifier ahead of
the quote — and passing it through to the `MethodCall` / `DynamicMethodCall` — makes `.?"..."()`
short-circuit to `Nil` for an absent method exactly as `.?method` does. Red 0.2.5's inflator
(`-> $_ { .?"{ $attr.type.^name }"() // .self }`) blocked Red, RedFactory and RedX::HashedPassword
on this one line.

## The atomic store is an operator, in every position

`⚛=` was recognised only by the statement-level and parenthesized assignment parsers — as a
statement shape rather than as an infix. So `return unless $!mouse-capture-stale ⚛== 1`, an
ordinary statement modifier condition, did not parse at all. The expression-level assignment site
now recognises it and lowers to the same `__mutsu_atomic_store_var` call the other sites build,
with an item-assignment (comma-tight) right-hand side.

The `⚛==` spelling in that same line was recognised nowhere. It is not a typo and not a comparison:
it is `infix:<⚛=>` under the assignment metaoperator, which rakudo will tell you itself —
`&infix:«⚛==».name` answers `infix:<⚛=> + {assigning}`. Since `⚛=` answers the value it just
stored, assigning that result back is the same store, so both spellings lower to the same thing. A
shared `strip_atomic_store_assign` consumes both, and the four sites that hand-sliced
`"⚛=".len()` go through it now. Selkie, Selkie::UI and Grammar::Editor were blocked on it.

## What this says about the cluster

The survey's conclusion — a queue of small tickets, not a handful of large ones — holds, but with a
sharper edge than "small": three of these four were not missing capabilities at all. mutsu could
already parse a semicolon subscript, a typed destructuring parameter and a modifier before a quoted
method name; it just could not do so in the second place the same syntax is spelled. The fix in
each case was to delete the copy rather than to grow it, which is why each one is a few lines and
why each reaches more than the distribution it came from.

Pinned by `t/collections/subscript/dotted-postcircumfix-subscripts.t`,
`t/routines/signature/destructure-anonymous-type-constraint.t`,
`t/oo/method/topic-quoted-method-name-modifier.t` and
`t/concurrency/thread-lock/atomic-store-expression-and-metaop.t`, all four verified against rakudo.
#7988 stays open: the long tail is still the work.
