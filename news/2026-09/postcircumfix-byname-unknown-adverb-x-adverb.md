# `postcircumfix:<[ ]>(...)` with an unknown adverb raises `X::Adverb` for slices and hash keys

```raku
my %h = a => 1;
postcircumfix:<{ }>(%h, "a", :foo);          # X::Adverb   (was X::Multi::NoMatch)
postcircumfix:<[ ]>([1, 2], (0, 1), :foo);   # X::Adverb   (was X::Multi::NoMatch)
postcircumfix:<[ ]>([1, 2], 0, :foo);        # X::Multi::NoMatch, as before
```

The syntax form (`%h<a>:foo`, `@a[0,1]:foo`) was classified in #10292 by
`Interpreter::builtin_subscript_named_adverbs`: a slice candidate or an
associative candidate slurps `*%_` in Rakudo and raises `X::Adverb`, while a
single positional element and a multi-dimensional subscript have no candidate
(`X::Multi::NoMatch`). The routine form, `builtin_postcircumfix_subscript`
(reached by a call by name, or as the CORE fallback when no user `postcircumfix`
candidate matches), still answered every unrecognized adverb with
`X::Multi::NoMatch`, so the two spellings of one subscript disagreed.

`postcircumfix_subscript_adverb` now hands any call carrying an adverb that is
not a built-in subscript adverb (`:k :v :kv :p :exists :delete`, now one shared
`is_builtin_subscript_adverb`) to that same classifier, so both forms report the
same `what` (`slice`, `whatever slice`, `zen slice`, `{} slice`, `element
access`), `unexpected` and `nogo`. The report's `source` is the variable the
container was declared as (`%h`, `@a`), read from the container's descriptor
name; an anonymous container is the bare sigil, the convention `array_slot_ref`
already uses for an element's owner (ADR-0064). A call with more than one index
still has no candidate, and the single built-in-adverb calls are unchanged.

Rakudo names an anonymous array literal's source `element` where this reports
`@`; the test does not pin that corner.

Pinned by `t/collections/subscript/postcircumfix-byname-unknown-adverb.t`.
