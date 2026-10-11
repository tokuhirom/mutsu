# Set/Bag/Mix render object elements through their own Str/gist

`Set.Str`, prefix `~`, string interpolation, `.gist` and `say` on a quant hash
used mutsu's default form (`F<16>`) for an element whose class declares `Str`
or `gist`. The object elements are now resolved through the interpreter first
(`runtime/quanthash_element_stringify.rs`, the same approach as
`list_element_stringify.rs`) and the result goes to the same pure renderer:
`Setty.Str` calls each element's `Str`, `Baggy`/`Mixy.Str` and `.gist` its
`gist`. Fixes #12702 (the `set(Int).Str` warning case is not part of this).
