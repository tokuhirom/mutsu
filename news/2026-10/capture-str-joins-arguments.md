# `Capture.Str` joins the arguments instead of answering the `.gist` form

`\(1, "x y", :a(3)).Str`, `"$c"` and `~$c` answered the call-shape text
`\(1, "x y", :a(3))`, the same as `.gist`. Rakudo's string form is each positional argument's
`.Str` followed by each named pair's `.Str` (`key<TAB>value`), joined with a space, so the
answer is now `1 x y a<TAB>3`. The two forms live in `src/value/capture_text.rs`:
`Value::to_string_value` (stringification) answers the `Str` form, and `gist_value` and
`Capture.gist` keep the call shape. Named arguments are rendered in name order, so the text does
not depend on the map's internal order. Pinned in `t/oo/method/capture-method-rows.t`
([#12054](https://github.com/tokuhirom/mutsu/issues/12054)).
