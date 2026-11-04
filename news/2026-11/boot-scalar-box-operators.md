# A BOOT scalar box is its high-level value at an operator

`nqp::box_i($i, nqp::bootint())` and its `BOOTNum` / `BOOTStr` siblings were hllized when
bound to a parameter (slice 1) but still reached an operator raw: `$box + 1` died with
`Cannot resolve caller Numeric(BOOTInt:D: )`, `$box ~ "y"` printed `BOOTStr<20>y` and
`$box cmp "a"` answered `Less`. The arithmetic, comparison, `~`, `x`, `cmp` and prefix
`+`/`-`/`~`/`!` operators now hllize their operands (`Value::hllize_operand`, an O(1) tag
check), as rakudo does at its binder (#12881).
