# A Num prints in plain decimal up to 1e16

`say 1e15` printed `1e+15`; rakudo 2026.09 prints `1000000000000000` and only
switches to scientific notation at 1e16 (`1e+16`). `Num.Str`/`.gist` and `.raku`
(`1000000000000000e0`) now use the `[1e-4, 1e16)` window, so `1.5e15`,
`2**53 * 1e0` and `1e15 + 1` match.
