# `* += *` is a two-argument WhateverCode

A compound assignment whose target and right-hand side are both bare `*` now
counts the target star as the first parameter, so `(* += *)($x, 5)` adds 5 to
`$x` instead of dying with "Too many positionals". `(* += 1)($x)` writeback on
a direct call is still open (#13311).
