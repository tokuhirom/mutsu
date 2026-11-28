# Heredoc body name checks from Rakudo 2026.11

A heredoc body is written below the line its heredoc starts on, but its code
runs where the heredoc starts. Rakudo 2026.11 therefore refuses a body whose
names would mean something else where they are written. mutsu now reports
the same two compile-time errors (#19284):

- `X::Syntax::Heredoc::HiddenName`: the name is declared only in a block that
  closes on the heredoc's line, before the body
  (`{ my $x = 42; $out = qq:to/END/; }` with `$x` in the body);
- `X::Syntax::Heredoc::AmbiguousName`: the name means one declaration where
  the heredoc starts and another where the body is written. That covers a
  closed block's name with an outer namesake, a `my` later on the heredoc's
  line, and a scope opened on that line that declares the name after the
  body.

The check lives in the parser's declaration-scope walk (`outer_redecl`), so
it is a `===SORRY!===` before anything runs. Each error is reported on the
line rakudo reports. The parser now records how many enclosing blocks close
on the heredoc's line; before, it recorded only whether any did.

This replaces the compiler's 2026.09-era check. That one died at run time
with an `X::Undeclared` message, only for a sub body or an `if` branch, and
skipped a name that had an outer namesake.
