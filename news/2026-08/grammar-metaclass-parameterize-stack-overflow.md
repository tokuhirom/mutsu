# `grammar Bot::Grammar[...]` no longer overflows the stack, and `method ^parameterize` works

The doc-diff harness found that `Language/mop.rakudoc`'s own worked example for
the "parametric" archetype — a grammar that declares `method ^parameterize` and
is then parameterized with a parametric role — aborted mutsu with
`thread 'mutsu-main' has overflowed its stack` (exit 134). It now prints raku's
answer, `sup GreetBot`.

Three separate defects were behind it. The stated guess in the original finding
(that `.^mixin` or the parameterization recursed into `^parameterize`) was
wrong on both counts: `^parameterize` was never reached at all, and the crash
had nothing to do with the metaclass method.

## 1. The crash: a self-resolving parent chain in the class-registry walk

`Interpreter::is_container_subclass` walks a class's registry parent chain, and
resolved each parent name with an exact registry lookup *falling back to a
short-name match* (`Bar` finds `Foo::Bar`). `grammar Bot::Grammar` lowers to a
class whose parent is the built-in `Grammar`, which has no registry entry — so
the fallback matched the short name of `Bot::Grammar` itself. The walk was
therefore `Bot::Grammar -> Grammar -> Bot::Grammar -> ...` forever.

`rust-gdb -batch` breakpoints on `runtime_class_query.rs` proved it in two
steps: the first hit showed `is_non_parametric_type("Bot::Grammar")` entering
`is_container_subclass("Bot::Grammar")`, and a later backtrace showed frames
`Bot::Grammar -> Grammar -> Grammar -> Grammar` stacked on top of it.

The reduced repro needs neither `^parameterize` nor a parametric role — only
the name collision:

```raku
grammar Bot::Grammar { token TOP { .+ } }
role Greetings { token x { 'y' } }
Bot::Grammar[Greetings];   # exit 134
```

Three sibling walkers shared the same shape and the same latent cycle
(`class_is_grammar`, `class_inherits_from_exception`,
`class_inherits_from_immutable_setty`) — `class My::Cool is Cool` would have
hung the last two just as reliably. All four now share one
`resolved_class_parents` helper that returns the resolved *registry key*, and
each walker dedupes on that key, so the chain cannot revisit a class. This is a
visited set for a graph walk, not a depth cap: the answers are the correct ones
(`Bot::Grammar` is not a container subclass; it *is* a grammar).

## 2. `Type[...]` ignored a user-declared `method ^parameterize`

With the crash gone, `Bot::Grammar[Greetings[...]]` merely threw
`X::NotParametric`. Rakudo's `type_object[args]` *is* a call to the metaclass's
`parameterize`, and declaring `method ^parameterize` is the documented way to
make an otherwise non-parametric class or grammar parametric — but mutsu's
positional-index path (`exec_index_op_with_positional`) never looked for one.
It does now, ahead of the `X::NotParametric` arm, passing the type object plus
the type arguments in the metamethod calling convention. Roles keep the
built-in currying protocol.

A user-declared metamethod also has to beat the native ClassHOW method of the
same name — `parameterize`, `compose`, `mixin` and friends were all pre-filtered
out of the user-metamethod dispatch branch by name, so a declaration could never
win. The gate is now the `has_user_method` lookup that immediately follows,
which keeps it narrow: every type that has not declared one still reaches the
native handler.

## 3. A metamethod's `self` was the type object, not the HOW

`method ^foo` is added to the type's *metaclass*, so Rakudo binds `self` to the
HOW and passes the type object separately as the first positional argument.
mutsu bound the type object to both, so the doc example's first statement —
`my Str:D $name = self.name: $this` — died with
`No such method 'name' for invocant of type 'Foo'`. The metamethod dispatch
site now binds the receiver's `.HOW` as the invocant while still resolving the
method body against the declaring class.

## Pin

`t/metaclass-parameterize.t` (8 tests, output verified identical under `raku`)
covers the parameterized rename, the mixed-in role method, the untouched base
type, `self.name($type)` inside a metamethod, the full `Bot::Grammar` grammar
case end-to-end, a `class Zoo::Cool is Cool` name collision, and that a class
*without* `^parameterize` still throws `X::NotParametric`.
