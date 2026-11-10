# `Uni.list`, `.List` and `.values` return a `Seq`

The result of `.NFC`/`.NFD`/`.NFKC`/`.NFKD` is not a `List` in rakudo, so its `.list`, `.List` and
`.values` are `Seq`s (and `.Array` holds the codepoints). mutsu returned a `List` and wrapped the whole `Uni` for `.Array`; they now match `raku` (#17371).
