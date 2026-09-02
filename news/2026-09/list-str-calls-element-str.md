# Stringifying a list calls each element's own `Str`

```raku
class C { has $.t; method Str { $!t } }
my @a = (C.new(t => "hi"),);
say ~@a;            # raku: hi     mutsu: C()   (before)
say @a.join("");    # raku: hi     mutsu: hi
```

`Value::to_string_value` is a *pure* renderer: it knows the list-shape rules
(space-separated, nested lists flattened) but cannot dispatch a user-defined
method, so an `Instance` element came out as the `ClassName()` fallback.
`.join` had always resolved its elements through the interpreter; every other
string-context entry point had not, so the two disagreed about the same array.

Rather than duplicate the shape rules in an interpreter-aware renderer, the
elements are resolved **in place** — each `Instance` replaced by the string its
class dispatches, recursing into nested lists — and the resulting list handed to
the same pure renderer (`src/runtime/list_element_stringify.rs`). The shape
rules stay in one place; only the per-element stringification moves.

Five entry points needed it, which is why the bug survived: the `.Str`/
`.Stringy` method (the native fast path now declines a list holding an
`Instance` and `dispatch_list_str_method` handles it), prefix `~`
(`exec_str_coerce_op`), string interpolation (`exec_string_concat_op`), infix
string operands (`coerce_stringy_operand`, which is how `eq` sees it), and the
native TAP `is` (`stringify_test_value` plus its `value_for_diag`, so the
diagnostic shows what the comparison actually compared).

Found while re-measuring the `XML` battery candidate
(`todo/tickets/bundle-xml-battery.md`): `t/namespaces.rakutest` asserts
`is @items[3].contents, 'A nested item, oh boy.'`, and `.contents` is a list of
`XML::Text` nodes whose `Str` returns the text. mutsu compared `XML::Text()`.
With this and the Capture-slip fix
(`news/2026-09/slip-array-element-capture-not-respread.md`) the upstream suite
reaches **15/15**, matching `raku`.

A neighbouring difference this surfaced but did not change: mutsu's `.Str`
dispatch falls back to `Stringy`, so a class defining only `Stringy` answers
that where raku answers the `Mu.Str` default. `.join` has always had it too, so
the new path deliberately reuses `.join`'s exact call and the two agree; filed
as `todo/tickets/str-method-falls-back-to-stringy.md`.

Pin: `t/list-str-calls-element-str.t` (12 subtests, passes under `raku` too),
covering all five entry points, nested lists, mixed elements, `.gist`/`.raku`
staying object-shaped, and a plain list keeping the untouched fast path.
