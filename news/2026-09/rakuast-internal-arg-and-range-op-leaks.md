# RakuAST stops leaking two mutsu internals

Two `.AST` leaks, both silent wrongness rather than coverage gaps, both found by
the gist-comparison sweep against rakudo 2026.07.

## An injected named argument rendered as a real one

mutsu's parser attaches a `__mutsu_test_callsite_line => N` named argument to
every listop call, so a failing `Test` assertion can report the caller's line. It
is instrumentation, not something the source wrote, and the converter rendered it
as an ordinary argument — on calls as plain as `f()`:

```
$ mutsu -e 'say Q{sub f { }; f()}.AST'
    expression => RakuAST::Call::Name.new(
      name => RakuAST::Name.from-identifier("f"),
      args => RakuAST::ArgList.new(
        RakuAST::ApplyInfix.new(
          left  => RakuAST::QuotedString.new(
            segments   => (
              RakuAST::StrLiteral.new("__mutsu_test_callsite_line"),
```

`arg_list` now skips a named argument whose key is one of mutsu's internal
markers, using the same `is_desugar_marker` predicate that already refuses
internal *routine* and *variable* names — this was simply the third place such a
name could reach the output.

Fixing it exposed a third leak: raku omits the `args` field entirely for an
argument-less call, so filtering left an empty `ArgList` where there should be no
field at all. `call_name` now omits it, the way `control_call` already did for a
bare `return`/`last`/`next`.

## A Rust variant name rendered as an operator

`token_kind_to_op_name` ends in a `{:?}` fallback, so any `TokenKind` without an
explicit row renders its **Rust variant name**. The exclusive range operators had
no rows:

```
$ mutsu -e 'say Q{my @a = 1..^3}.AST'
          infix => RakuAST::Infix.new("DotDotCaret")     # rakudo: "..^"
```

`..^`, `^..`, `^..^` and `...^` now have rows, and the fallback carries a comment
saying what it does, so the next missing operator is a known cost rather than a
surprise.

## Coverage

`t/rakuast-internal-arg-and-range-ops.t` (10 assertions) pins that no `__mutsu`
name appears in a rendered call, that an argument-less call omits `args` while a
call with arguments keeps it, that a listop keeps its *real* argument, each
exclusive range operator rendering as itself, and that no `TokenKind` variant
name leaks into a range gist. It is a dual-oracle test: it passes verbatim under
both mutsu and rakudo 2026.07.

After this, a 28-program sweep across the common syntax renders byte-identically
to rakudo.
