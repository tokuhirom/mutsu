# Method calls on a variable bound to Nil skip the Nil special-case handling

A variable *bound* (not assigned) to `Nil` — `my $v := Nil` — dispatches
methods through the named-variable opcode path (`vm/vm_call_method_mut_ops.rs`),
which has none of the Nil special-casing that the plain `MethodCall` opcode
handler carries (`vm/vm_call_method_ops.rs`, the `target.is_nil()` block):
warn-and-resume numeric/string coercions, the Nil-absorbing catch-all,
mutator errors, etc. The literal form `Nil.Int` behaves correctly; the bound
form silently diverges.

## Repro

```
raku:  my $v := Nil; say $v.Numeric   # warns "Use of Nil in numeric context", 0
mutsu: my $v := Nil; say $v.Numeric   # prints "Nil", no warning

raku:  my $v := Nil; say $v.Str       # warns "Use of Nil in string context", ""
mutsu: my $v := Nil; say $v.Str       # prints "", no warning

raku:  my $v := Nil; say $v.Int       # warns, 0
mutsu: my $v := Nil; say $v.Int       # prints "Nil", no warning
```

(Assignment is unaffected: `my $v = Nil` stores `Any`, and the Any type-object
coercion path handles `.Int`/`.Numeric`/`.Real` correctly as of the
any-nil-int-num-coercion fix.)

## Root cause / why deferred

The Nil handling is ~130 inline lines in the `MethodCall` opcode handler
(`vm_call_method_ops.rs`, the `if target.is_nil()` block) and is not shared
with the named-variable method-call opcode in `vm_call_method_mut_ops.rs`,
which is structured around mutation/writeback concerns. Fixing this properly
means extracting the Nil dispatch into a helper both opcodes call (and
auditing which of its arms are safe when the target is a named variable —
e.g. the autovivifying `push`/`append` arms already have their own separate
implementation on the mut path). That refactor is bigger than a drive-by fix.

Binding a variable to `Nil` is rare in practice (assignment converts to
`Any`), so no known module or roast test is currently blocked by this.
