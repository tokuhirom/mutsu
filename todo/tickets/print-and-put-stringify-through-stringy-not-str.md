# `print`/`put` stringify a type object through `.Stringy`, but rakudo uses `.Str`

`render_str_value` (`src/runtime/io_env.rs`, the coercion `print`/`put` use)
tries a user `.Stringy` before a user `.Str` when its argument is a type object.
Rakudo's `print`/`put` are `.Str`, and the two are genuinely different methods —
prefix `~` is the `.Stringy` one.

## Repro (measured 2026-08-28 against `raku` as the oracle)

```raku
class WithStringy { method Stringy { 'bar' } }
print WithStringy;         # raku: ""     mutsu: "bar"
say ~WithStringy;          # raku: "bar"  mutsu: "bar"   (agree)
say WithStringy.Str;       # raku: ""     mutsu: ""      (agree)
```

So only the `print`/`put` path diverges, and only for a class that defines
`.Stringy` *without* a `.Str`. A class defining both is worth checking too — the
same `Stringy`-first ordering would prefer the wrong one there as well.

`raku` emits its "Use of uninitialized value of type WithStringy in string
context" warning in the `print` case, which mutsu currently does not, because it
answered through `.Stringy` instead of reaching the warning.

## How it was found

While fixing `regex_match_text` to coerce a type object properly
(`news/2026-08/type-object-string-coercion-dispatches-its-own-str.md`). The
first draft of that fix delegated to `render_str_value` and got the
`.Stringy`-only case wrong, which is what exposed this. The regex path now has
its own `.Str`-only coercion and does not go through `render_str_value`, so the
two are independent.

## Why it is not in that PR

Changing `render_str_value` changes the rendered output of every `print`/`put`
of a type object, including inside diagnostics and inside `Test`'s own output
paths, and it adds a warning where there was none. That is a wider blast radius
than the fix it was found under, and it deserves its own before/after
measurement over the whitelist rather than being folded in.

## Where to start

`src/runtime/io_env.rs`, `render_str_value` — the `ValueView::Package` branch
tries `Stringy` then `Str` then warns. Check what the non-`Package` tail
(`call_method_with_values(value, "Str", ...)`) does for an *instance* defining
only `.Stringy` before changing the ordering; the two halves should agree.
