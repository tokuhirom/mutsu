# `DOC INIT` blocks do not see `$=pod`, and cannot until declarator WHEREFORE is a real routine

`--doc` mode runs `DOC INIT` blocks through `run_doc_init_blocks`
(`src/doc_mode.rs`), which builds a fresh `Interpreter` and evaluates the block
without ever collecting the program's Pod. So `$=pod` is `Nil` inside the one
construct whose entire purpose is to render the document:

```raku
=begin pod
=head1 A Heading!
=end pod

DOC INIT {
    use Pod::To::Text;
    say "ELEMS=" ~ $=pod.elems;   # rakudo: ELEMS=1   mutsu: ELEMS= (Nil)
    pod2text($=pod);
}
```

`Interpreter::establish_pod_variables()` (`src/runtime/run.rs`) already does
exactly the required work — `collect_doc_comments` + `collect_pod_blocks` +
`add_declarator_pod_entries` — and `run()` calls it before the mainline. Calling
it in `run_doc_init_blocks` is a one-line fix.

## Why the one-line fix is not enough

With `$=pod` populated, the real `Pod::To::Text` (now bundled verbatim as
`modules/Rakudo-Core/lib/Pod/To/Text.rakumod`) walks the declarator entries and
dies:

```
Runtime error: X::ControlFlow
```

The throw comes from upstream's first line of `declarator2text`:

```raku
sub declarator2text($pod) {
    next unless $pod.WHEREFORE.WHY;
```

`.WHY` returns a false value, so `next` runs outside any loop. `.WHY` fails
because `add_declarator_pod_entries` (`src/runtime/io_doc.rs`, which carries a
TODO saying so) sets a declarator's `WHEREFORE` to a **type-name placeholder**
rather than the declared routine:

```rust
DocDeclKind::Sub => Value::package(Symbol::intern(base_type)),  // "Method", "Sub", ...
```

`dispatch_why` (`src/runtime/methods_introspect.rs`) resolves `.WHY` by looking
the package's *name* up in the doc-comment table, so a class declarator works
(`WHEREFORE` is `Sheep`, and `Sheep` is a real key) while every sub/method
declarator does not (`Method` is a type name, not a doc key). Measured on the
`t/doc-mode-pod-render.t` document:

| `$=pod` entry             | WHEREFORE | `.WHY` |
| ------------------------- | --------- | ------ |
| `Pod::Block::Named`       | Nil       | False  |
| `Pod::Block::Declarator`  | `Sheep`   | True   |
| `Pod::Block::Declarator`  | `Method`  | False  |

## What the real fix is

Give a declarator's `WHEREFORE` the actual `Sub`/`Method`/`Attribute` object, as
rakudo does — there `$pod.WHEREFORE.WHY === $pod` and `.WHEREFORE.signature`
works, which upstream's `declarator2text` goes on to use for
`signature2text($_.signature.params, $_.returns)`. That needs the routine object
to be reachable at `add_declarator_pod_entries` time and `.WHY` to resolve off
the object rather than off a name, so it is more than a placeholder swap.

Until then `src/doc_mode.rs` deliberately skips `establish_pod_variables` and
carries a TODO pointing here.

## Repro

```
timeout 30 target/debug/mutsu --doc <the file above>
```

Compare with `raku --doc <same file>`.
