# A hyper postfix no longer distributes a metamethod

`».` hypers ordinary method calls only. A **metamethod** after it (`».^name`) is
not distributed — rakudo applies it to the container. mutsu distributed it, so
the two disagreed on every `>>.^...` expression:

```raku
my @a = Int, Str;
say (@a>>.^name).raku;
# raku:            "Array"
# mutsu (before):  ["Int", "Str"]
```

The gap was specific to the `.^` form. An ordinary method after `».` distributes
in both (`@a>>.Str` is `["1", "2"]` either way), and mutsu already agreed with
rakudo on the other introspection dotties — `».WHAT`, `».HOW`, `».VAR` and
`».DEFINITE` all answer about the container in both implementations. Only `.^`
was hypered.

## The fix

A parse-level one, as the ticket expected. In the hyper-postfix branch of the
postfix loop, a `.^` immediately after `»`/`>>` (followed by a method-name start)
now hands the `.^meth` back to the plain dotted-call branch with the target
expression untouched, so it compiles exactly as the hyper-less spelling would.
Everything else after `».` — plain methods, `.?`, `.+`, `.*`, `.[...]`,
`.{...}`, `.(...)`, `.++` — keeps hypering as before.

## Why it matters beyond the literal expression

The spelling people actually reach for, `.^mro>>.^name`, was affected: it means
different things in the two implementations, which is how this was found —
writing `t/pseudostash-type.t` for
[#7588](https://github.com/tokuhirom/mutsu/issues/7588), the natural
`PseudoStash.^mro>>.^name.join(' ')` passed under mutsu and failed under rakudo,
and the difference turned out to be this rather than the MRO under test. Such a
pin can now be written the natural way.

## Pins

`t/hyper-postfix-metamethod.t` covers the `Array`, `List` and instance-array
forms, the `»` spelling, a chained `».^name.^name`, the `.^mro>>.^name` spelling
above, and three controls where an ordinary method after `».` must still
distribute. All nine assertions were checked against rakudo.

Closes [#7643](https://github.com/tokuhirom/mutsu/issues/7643).
