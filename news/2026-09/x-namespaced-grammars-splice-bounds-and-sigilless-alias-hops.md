# A grammar named `X::…` is not an exception, `splice` checks its bounds everywhere, and a `\c` alias survives a method hop

Four general interpreter bugs, found by re-measuring `Crane` v0.1.2 (the
dependency `Config::TOML` is built on, [#7539](https://github.com/tokuhirom/mutsu/issues/7539))
against the `raku` oracle. `Crane` goes from 7/15 to 9/15 files, and the three
files still red lose most of their remaining failing assertions.

## 1. A `Match` of a grammar declared under `X::` was treated as an exception

mutsu has no complete exception hierarchy to consult, so about twenty sites
decide "is this an exception?" from the class name alone — `Exception`, or the
reserved `X::` / `CX::` namespaces. That test has one systematic false
positive. A grammar's `Match` is typed by the *grammar*, not by `Match`:
rakudo answers `X::Foo::G` for `X::Foo::G.parse($s).^name`. So every one of
those sites was handed a "class name in the exception namespace" that was a
match object:

```raku
grammar X::Deep::G { token TOP { \d+ } }
my $m = X::Deep::G.parse('42');
say $m.Str;        # rakudo: 42      mutsu: X::Deep::G with no message
say +$m;           # rakudo: 42      mutsu: Failure (numifying that text)
say $m.chars;      # rakudo: 2       mutsu: 12
say $m ~~ Exception;  # rakudo: False   mutsu: True
```

Not a contrived namespace. `Crane` declares a grammar inside
`class X::Crane::PathOutOfRange`, to parse the `Range` back out of an
`X::OutOfRange`; its action method is `method integer($/) { make(+$/) }`, so
every out-of-range path error died while rendering its own message —
`Type check failed for an element of @integer; expected Int:D but got Any`.

The namespace test is now one predicate, `Value::instance_is_exception_by_name`,
which excludes a `Match`, and the sites that carry extra terms (an MRO walk, a
`*Exception` suffix) go through it too. The surviving bare-name checks are the
ones that never see a `Match` — a declared parent, a parsed bareword.

## 2. `splice` validated its offset only on the lvalue path

Rakudo checks `splice`'s offset and size *before* it writes: an offset outside
`0..elems` and a negative size are `X::OutOfRange`, not a splice clamped to the
end. mutsu did that for a named-variable invocant and not at all for a
by-value one — a literal, a function result, an element read, anything reached
through `return-rw`:

```raku
[1,2,3].splice(5, 0, 'x')   # rakudo: X::OutOfRange   mutsu: appended silently
```

`Crane.add`'s whole out-of-range family is `Crane::At.at($root, @path).splice(...)`
inside a `CATCH { when X::OutOfRange }`, so five `add`/`copy`/`move` subtests
reported success where rakudo raises `X::Crane::AddPathOutOfRange`. The range
check is now a shared helper both dispatch paths call, so the two cannot drift.

## 3. A sigilless parameter forwarded to a *method* lost the caller's container

A `\c` parameter IS the caller's container, so forwarding it must keep that
identity however many hops it takes. A sub callee always worked: sub frames
chain env, so the exit writeback's "resolve to the root of the alias chain"
reached the original variable. A method callee does not chain env — its
writeback merges only into the frame that called it, where the chain's root is
not a key at all — so the write landed on a phantom entry, and the intermediate
frame then reached its own exit still holding the pre-call value and clobbered
the root with it:

```raku
class Leaf { method go(\c) { c = 1 } }
class Mid  { method go(\c) { Leaf.go(c) } }
my $v; Mid.go($v);   # rakudo: 1   mutsu: (Any)
```

Each frame's writeback now also updates the caller's own alias binding, so the
hops compose into one another the way the chained-env sub path already reached
the root. That is `Crane.add/set/remove($container, :path(), …, :in-place)`,
which is a chain of class methods forwarding one `\container`.

## 4. A raw alias of an `@`/`%` variable stored the bare `List`

Raku gives `@a`/`%h` no `Scalar` of their own, so `\c := @a` makes `c` BE the
array and `c = LIST` is `@a.STORE(LIST)`. The writeback that carries a
sigilless parameter's final value back passed it through verbatim, so the
caller kept a bare `List` — and, one relay hop further, the itemized
`$("x", "y")` the hop's holder had put on it:

```raku
sub f(\c) { c = ('x','y') }
my @a; f(@a);   # rakudo: ["x", "y"]   mutsu: ("x", "y")
my %h; f(%h);   # rakudo: {:x("y")}    mutsu: ("x", "y")
```

The three writeback sites now shape the value for the target's own sigil, in
one shared helper.

## Measurement

| Suite | raku | before | after |
| --- | --- | --- | --- |
| `Crane` v0.1.2 | 15/15 files | 7/15 | **9/15** |
| `Config::TOML` v0.1.3 | 19/19 files | 14/19 | 14/19 |

`Crane`'s `copy` and `move` are the two newly green files; `add` now passes
every assertion of its exception subtest and every in-place assertion but the
nested one.

Pins: `t/grammar/match-of-x-namespaced-grammar-is-not-an-exception.t`,
`t/collections/array/splice-offset-out-of-range-by-value-invocant.t`,
`t/vm/sigilless-alias-forwarded-through-a-class-hop.t` — 55 assertions, each
verified to fail without its fix and to pass under rakudo v2026.07.
