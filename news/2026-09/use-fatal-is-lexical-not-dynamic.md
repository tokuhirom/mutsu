# `use fatal` no longer leaks into a callee declared outside its scope

`use fatal` is a lexical pragma in real Raku: only code lexically inside its
scope is affected. mutsu tracked it purely as a live, interpreter-wide
`fatal_mode` flag, so it stayed set for the whole *dynamic* extent of a call —
a routine declared outside a `use fatal` block exploded a `Failure` produced
by its own nested calls merely because its caller happened to be inside one:

```raku
sub g($x) { "g-ran" }
sub f($s) { g($s.Int) }
{
    use fatal;
    say (try { f("abc") }) // "died";
}
```

Rakudo prints `g-ran`: `f`'s own body is not lexically under `use fatal`, so
`g($s.Int)`'s coercion failure stays soft. mutsu printed `died`.

The fix tracks `use fatal` at compile time (`Compiler::fatal_pragma_active`,
mirrored from `use fatal;`'s own statement and saved/restored around
import-scoped blocks) and bakes the result into each routine's
`CompiledFunction::captured_fatal_mode`. A second interpreter flag,
`lexical_fatal_mode`, is initialized from that captured value at every
call-dispatch entry point — the four untyped call paths (fast/light/
light_typed/named) and all three TRIR call doors — and is what the
`explode_if_fatal_failure_in_*` checks now gate on, instead of the fully
dynamic `fatal_mode`.

That dynamic flag had to stay untouched at call boundaries: ADR-0058's
`try`-implies-fatal mechanism deliberately inherits a caller's dynamic `try`
state into a callee's own deferred `.map`/`.grep` `Seq` construction
(`SeqSource::MapGrep::fatal`), which a raku-verified regression test pins.
Conflating the two channels into a single reset broke that mechanism; the
fix keeps them separate.

Closes #9521.
