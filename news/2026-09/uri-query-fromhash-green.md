# URI::Query::FromHash passes: `<&param>` subrules, slipped junction operands, `Hash()` odd-element errors

`URI::Query::FromHash` 0.0.2's `t/basic.t` went from 13/20 to 20/20 under mutsu
(checked by hand; the record is re-measured by the next sweep). The distribution
exposed three general interpreter gaps:

- **`<&name>` where `&name` holds a named regex.** A subrule reference to a
  lexical only accepted an anonymous Regex value, so `my &c = &re; /<&c>/` and a
  `&class = &should-escape` parameter matched nothing. The `&re` reference to a
  `my regex`/`token` carries the declaration's captured Regex value, and the
  lexical lookup now calls it.
- **A Slip operand to `|`, `&`, `^`.** The infix junction operators take
  `+values`, so `|(1, 2) | 3` is `any(1, 2, 3)`; mutsu kept the Slip as one
  eigenstate, and a set written `| 0x2D | 0x2E | …` never matched `0x2D`.
- **`Hash()` coercion types on a scalar or an odd-length list.** `my Hash() $h =
  ''` padded the key with `Any` instead of dying with
  `X::Hash::Store::OddNumber` as `.Hash` does, so a `CATCH` guarding that case
  never fired.
