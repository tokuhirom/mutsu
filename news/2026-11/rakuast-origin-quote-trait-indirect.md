# RakuAST: origins of shell quotes, `does` traits and package-indirect names

A batch of `.origin` gaps closed (#15780):

- a variable followed by text starting with a dot in a string
  (`"$n.new answers"`, which is no method call without parentheses) now
  places the variable and the literal after it;
- `qqx/.../` interpolates like `qq`, and `qx/.../`'s lone `exec` processor no
  longer passes for a word quote, so their text is placed;
- a `does` trait on a variable (`my @a does R`) is read with the `is` / `of`
  traits;
- an indirect name under a package (`MY::(q[$x])`) spans the term, as
  `::(...)` already did; that placement moves to `indirect_name_origins.rs`.
