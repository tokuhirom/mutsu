# `<<` before an interpolated variable in a regex

`/<<$name>>/` and `s:g/<<$name>>/…/` never interpolated `$name`: the scalar-interpolation pass
read `<<` as the start of a `<…>` assertion and copied the text through to the next `>`. `<<` is
now passed through as the left word boundary, so the variable that follows is substituted.
Found by working the `Inline::BASIC` distribution, whose interpreter stored every variable as 0.
