# A type object's own `.Str` is what a regex match and a coercion parameter see

Two independent sites treated a *type object* as a name rather than as a value
to coerce, and both are fixed. They were found together while working the
`MUTSU_REAL_TEST=1` residue of `todo/deep/vendor-real-test-module.md`, because
the real `Test.rakumod` reaches both of them where the native provider reaches
neither.

## A regex smartmatch matched the type NAME

`regex_match_text` (`src/runtime/seq_helpers/smart_match.rs`) is the single
chokepoint every regex-matching arm of `smart_match` stringifies its subject
through. It dispatched a user `.Str` for an `Instance` and otherwise fell back
to `to_string_value()` — which, for a type object, is the type's own name. So
mutsu answered:

| | mutsu (before) | rakudo |
| --- | --- | --- |
| `Int ~~ /Int/` | True | **False** |
| `Any ~~ /Any/` | True | **False** |
| `class C { method Str { 'foo' } }` then `C ~~ /foo/` | False | **True** |
| `C ~~ /C/` | True | **False** |

Rakudo has no "match the type name" rule: a regex match is ordinary string
context, so a type object dispatches its class's `.Str` and otherwise coerces to
`""` with the familiar "Use of uninitialized value of type X in string context"
warning. mutsu already had `warn_type_object_string_context` for exactly that
coercion; `regex_match_text` simply never consulted it. It does now, and the
warnings, their wording and their ordering are byte-identical to rakudo's.

Two details are deliberate and were measured rather than assumed.

**`.Str` only, not `.Stringy`.** Prefix `~` is `.Stringy`, and the two differ —
with `class B { method Stringy { 'bar' }; method Str { 'baz' } }`, rakudo gives
`~B` eq `'bar'` but `B ~~ /baz/` True and `B ~~ /bar/` False. Reusing
`render_str_value` (which is `.Stringy`-first) would have got the headline case
right and this one wrong.

**A bare `/regex/` coerces its subject QUIETLY.** `Any ~~ /a/` and
`Any.match(/a/)` warn, but `/a/`, `if /a/` and `so /a/` against an undefined
topic do not — `roast/S05-metasyntax/regex.t` pins exactly that (`/a/; print
"pass"` must leave STDERR empty), and the first draft of this fix regressed it.
The distinction is a *compile-time* fact — the compiler synthesizes `$_` as the
LHS of a bare regex in `compile_match_regex` — so it is carried on the opcode
rather than guessed at run time: `SmartMatchLhs::Var` gained an
`implicit_topic` flag, which `exec_smart_match_expr_op` reads to route the
subject through `quiet_topic_for_regex_match` before the match. `SmartMatchLhs`
is boxed, so the extra field costs nothing in `size_of::<OpCode>()` (the
`opcode_size_guard` test still passes), and the existing `Var { name, .. }`
patterns needed no change.

Pin: `t/regex-smartmatch-type-object.t`, 23 assertions, green under real `raku`
unchanged — including both halves of the implicit/explicit warning split.

## A coercion-type parameter skipped a type object's coercion method

`try_coerce_value_with_method` (`src/runtime/types/coercion.rs`) dispatches the
target-named method when the argument is an `Instance` whose class defines it,
and had no matching branch for a type object. So `sub f(Str() $g)` given a
`class C { method Str { 'foo' } }` bound `""` where rakudo binds `"foo"`.

A coercion type calls the named method on its argument, and a method call on a
type object dispatches like any other, so the fix is the same branch for
`ValueView::Package`. It is guarded on the class actually defining the method,
which is what keeps `Str(Int)` and `Str(Any)` at `""` — measured against rakudo
across the whole matrix, the type-object-with-a-user-method case was the only
divergence. The branch is not `Str`-specific: `Int()` / `Num()` on a type object
defining `.Int` / `.Num` now dispatch too.

Pin: `t/coercion-param-type-object-user-method.t`, 14 assertions, green under
real `raku` unchanged.

## What it freed

`roast/S24-testing/14-like-unlike.t` passes under both providers. It is the real
module's own spec for `like`/`unlike` accepting non-`Str` objects, and it failed
under `MUTSU_REAL_TEST=1` because rakudo's `like` declares `Str() $got` and the
test hands it `class { method Str { 'foo' } }` — so the whole assertion turned
on the coercion-parameter half. The regex half was found on the way in (the
first reading of the failure blamed the smartmatch, and the diagnostic
`got: ""` is what corrected it) and is a real divergence in its own right, so it
is fixed here as well rather than left behind.

A third divergence surfaced by the same probe is *not* fixed here and is
recorded in `todo/tickets/print-and-put-stringify-through-stringy-not-str.md`:
`print`/`put` go through `render_str_value`, which tries `.Stringy` before
`.Str`, so `print WithStringy` renders `bar` where rakudo renders `""`. That one
changes output for every `print`/`put` of a type object and deserves its own
measurement.
