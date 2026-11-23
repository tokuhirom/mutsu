# `dd` of a RakuAST node prints through `$*ERR` too

#18985 made `dd` print through the dynamic `$*ERR` instead of the process
stderr, so a program that rebinds `$*ERR` captures its output, as on rakudo
(#19048). The branch added for RakuAST nodes (#19055), which prints a node's
`DEPARSE`, still wrote to the process stderr directly. It now goes through
`$*ERR` like every other `dd` line. A new test pins both paths, and checks
that `note` and `dd` interleave on the one handle.
