# `Uni.list`, `.List` and `.values` return a `Seq`

The result of `.NFC`/`.NFD`/`.NFKC`/`.NFKD` is not a `List` in rakudo, so its `.list`, `.List` and
`.values` are `Seq`s. mutsu returned an `Array`/`List`; they now match `raku` (#17371).
