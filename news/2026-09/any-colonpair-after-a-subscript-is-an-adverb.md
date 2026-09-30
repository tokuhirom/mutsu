# Any colonpair after a subscript is an adverb

A subscript followed by a colonpair whose name is not one of the built-in
subscript adverbs (`:exists`, `:delete`, `:k`, `:v`, `:kv`, `:p`) used to be a
parse error: `%h{"a";"b"}:$no` and `@a[1;0]:foo` said "Confused", and the
single-dimension `@a[0]:foo` was only accepted in its bare `:name` spelling
(not `:$no`, `:foo(3)` or `:foo<x>`) and always raised an `X::Adverb`.

Raku passes every adverb there as a named argument to `postcircumfix:<[ ]>` /
`<{ }>` (or `<[; ]>` / `<{; }>`), and it is the CORE candidates that decide what
an unknown one means. mutsu now reads the whole adverb chain as colonpairs as
soon as one of them is unknown (`parser/expr/postfix/named_adverb.rs`), and
lowers it to a call of a user `postcircumfix` candidate when one is in scope --
now with the adverbs' real values, not just `True` -- or otherwise to a model
of rakudo's candidates: `X::Multi::NoMatch` for a multi-dimensional subscript
and for a single positional element carrying only unknown adverbs,
`X::Adverb` (unexpected + nogo, `!k` for a false built-in adverb) for element
access with a built-in adverb too, and for every slice and associative
subscript.

`X::Adverb`'s message now follows rakudo's as well: "Unexpected adverb
'foo'", "2 unexpected adverbs (...)", the sorted `.nogo` / `.unexpected`
lists, and the same 72-column word wrap. `:$k` / `:$v` / `:$kv` / `:$p` are
accepted as the built-in adverb with a runtime flag.
