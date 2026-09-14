# Pipe-prefixed declarations keep their declaration semantics

Rakudo accepts a leading `|` before declarations, including the `|# ...`
spelling used by Grok 0.0.3 before a method. mutsu parsed that form as a pipe
expression followed by a detached block, so Grok's `Grok::Moppet` failed to
load with `Variable $!thing used where no 'self' is available`.

The statement parser now recognizes a pipe-prefixed declaration and hands the
following keyword to the normal declaration parser. Plain pipe expressions
keep their existing path.

Grok moves from blocked-load to green: 2/2 Rakudo-baseline files pass under
both interpreters. The two remaining test files are `no_baseline` because
Grok's undeclared `Test::Output` dependency is unavailable to Rakudo as well.

Pinned by `t/lang/parsing/parser-pipe-prefixed-declaration.t`.
