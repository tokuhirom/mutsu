# List::MoreUtils `occurrences` count mismatch was the Regex.Bool topic bug

Issue #9263 reported that `List::MoreUtils` 0.0.10's `t/occurrences.rakutest`
failed one assertion: the occurrence sum

```raku
@o = occurrences @lorem.grep: { /\w+/ };
is @o.pairs.grep( *.value.defined ).map( { .key * .value } ).sum, 124, ...;
```

came out `142` under mutsu against Rakudo's `124`, and suspected a `Bag` /
`Pair` / collection mismatch.

Reduced, the collection side was never wrong. The difference of 18 is exactly
the number of `,` and `.` tokens in the lorem text (11 + 7): the
`.grep: { /\w+/ }` filter kept every token. A block whose tail is a bare
regex returns the `Regex` itself, and Rakudo's `Regex.Bool` matches it against
the `$_` of the scope the literal was written in. mutsu treated the returned
`Regex` as plainly true, so `<a , b . c>.grep({ /\w+/ })` returned all five
elements instead of three. `Bag.pairs`, the `@o[.value].push(.key)`
bucketing and the `.key * .value` sum all agree with Rakudo once the grep
input is right.

That root cause was already fixed by `bf7b416c5` ("Regex.Bool matches against
the regex's lexical `$_`", #9258), which landed after the sweep that recorded
this failure. Building the parent commit reproduces `142` and the fix commit
yields `124`; on current `main` the whole file passes 8/8, and so does
`t/natatime.rakutest`, which the same stale record also listed as red.

`t/regex/regex-bool-lexical-topic.t` now also pins the exact shape the
distribution uses: a `.comb` of words and punctuation filtered by
`.grep({ /\w+/ })` and fed through the Bag-based occurrence sum.
