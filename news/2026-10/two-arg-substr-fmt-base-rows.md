# Two-argument `substr`, `fmt` and `base` always answer from their rows

`"abcdef".substr(1, *)`, `$list.fmt($format, $sep)` and `Rat.base($radix, *)` used
to bypass the method table whenever an argument was a `Whatever`, a `Range` or a
`Format`, and fall through to a name-matching cascade arm that called the same
function. The rows now take any plain argument, and the four arms of the
two-argument cascade are gone (ADR-11276 §9.113). `Duration.base($radix, $digits)`
no longer depends on that cascade either.
