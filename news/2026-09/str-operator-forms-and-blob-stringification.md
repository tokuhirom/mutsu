# `leg`, the `&infix:<...>` string routines and Blob stringification agree with the operators

Three string-operator paths had their own copies of the same rules (ADR-0118 §2.6):

- **`leg`** stringified its operands with the pure renderer. It skipped the coercion `eq`/`lt` use,
  so an object with a user `Str` compared by its `.gist` (`S.new leg "a"` was `Less` where rakudo
  says `More`), and a junction operand did not autothread. `leg` now runs the comparators'
  operand coercion and junction threading.
- **The routine forms** (`&infix:<eq>`, `&infix:<leg>`, `&infix:<~>`, ...) fell back to the pure
  reduction table, which only knows `.gist` for an object. `.sort(&infix:<leg>)`, `cmp-ok` and a
  direct `infix:<eq>($obj, "b")` therefore disagreed with the operator. They now coerce their
  operands as the opcode does.
- **Blob stringification** had five hand-written `X::Buf::AsStr` throw sites with three different
  messages. None of them set the `object` attribute, and `.message` came back empty. `Buf ~ Str`,
  `~$buf` and `"$buf"` did not die at all: they spliced in lossy-decoded bytes or the gist. There
  is now one constructor, `buf_as_str_error`, and one `.message` in rakudo's wording, wrapped at
  72 columns. One helper, `concat_operand_stringy`, holds the Blob-to-string rule (only `utf8`
  decodes); `~`, prefix `~` and interpolation all use it.

`t/types/string/str-operator-forms-parity.t` pins 29 rows measured with rakudo.
