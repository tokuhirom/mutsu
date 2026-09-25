# `nqp::getcomp("Raku")` and REPL sessions that keep their declarations

`nqp::getcomp` was an unsupported op, and it stopped four distributions dead at load time:
CodeUnit, Text::CodeProcessing, Jupyter::Kernel and Jupyter::Chatbook
([#9349](https://github.com/tokuhirom/mutsu/issues/9349)). All four use it the same way — to
build a persistent REPL on the protocol rakudo's own `REPL` class uses: `.eval($code,
:outer_ctx($ctx))`, with a `$*CTXSAVE` object whose `ctxsave` method captures the unit's context
through `nqp::ctxcaller(nqp::ctx)` so the next line can be compiled inside it.

mutsu now has that protocol ([ADR-0122](../../docs/adr/0122-repl-compiler-object-and-eval-contexts.md)):

- `nqp::getcomp("Raku")` answers a `Perl6::Compiler` object, and rakudo's core `REPL` class
  exists (`new`, `repl-eval`, `ctxsave`, `input-incomplete`, ...). Both are plain Raku classes,
  registered on first use; the one native primitive behind them is the unit evaluation.
- `nqp::ctx`, `nqp::ctxcaller` and `nqp::ctxlexpad` work, with contexts named by `BOOTContext`
  handles.
- A unit run through `.eval` has what it declared captured just before its scope unwinds — its
  env, and the lexical subs and operators it added — and the next unit with that `:outer_ctx`
  sees all of it. Nothing leaks into the host program.

```raku
is ev('my $a = 42'), 42;
is ev('sub double($x) { $x * 2 }; double($a)'), 84;
is ev('sub infix:<foo>($, $) { "foo" }; 1'), 1;
is ev('42 foo 666'), 'foo';     # rakudo 2026.07 still fails this one
```

CodeUnit's own test goes from dying to 5 of 6. The sixth needs the numeric-context
"Use of uninitialized value" warning ([#9359](https://github.com/tokuhirom/mutsu/issues/9359)).
Text::CodeProcessing and the Jupyter sandboxes now get past the compiler and are blocked by
`my \_` sharing its storage with the topic `$_`
([#9358](https://github.com/tokuhirom/mutsu/issues/9358)). A rebind bug found on the way
(`my $z := $y; $z := 5` changes `$y`) is [#9357](https://github.com/tokuhirom/mutsu/issues/9357).

Pinned by `t/vm/nqp-getcomp-repl-context.t`.
