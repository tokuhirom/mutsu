# REPL-sandbox evaluation sees the caller's dynamics and yields sigilless assignments

Two gaps behind Text::CodeProcessing's code-chunk evaluation:

- Code compiled through the compiler object (`REPL.repl-eval`, `Perl6::Compiler.eval`)
  now sees the caller's dynamic variables. The unit used to start from an empty
  environment, so a sandbox that redirected `$*OUT` to capture a chunk's output
  printed to the real stdout and every chunk's `#OUT:` was `Nil`.
- A compilation unit whose last statement is an assignment to a sigilless term
  (`EVAL 'p = 8'`, a REPL chunk ending in `_ = _1`) now yields the assigned
  value; the store used to consume it and the unit answered `Nil`.
