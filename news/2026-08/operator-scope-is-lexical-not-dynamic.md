# A user-declared operator is scoped to its compilation unit, not to the call stack

`roast/S06-operator-overloading/sub.t` and `roast/S03-metaops/hyper.t` both
regressed under `MUTSU_REAL_TEST=1`, and both had the same cause: mutsu decided
whether a user-declared `sub infix:<op>` was in scope from a **dynamic** signal
— how many module frames deep the VM currently was — where Raku's rule is
**lexical**: an operator belongs to the compilation unit that declared it.

## Why only the real `Test` saw it

The gate exists for a real reason. A test file that declares `sub infix:<+>`
must not have it intercept `Test.rakumod`'s own `$num_of_tests_run + 1`; the
counter went `Nil` when it did. mutsu implemented that as
`Interpreter::module_call_depth`, incremented whenever a routine whose
`source_file` differs from the main script was entered, and
`user_infix_override` returned false whenever the depth was non-zero.

That answers "is a module routine anywhere below me on the stack", which is a
different question, and it gets the callback case exactly backwards. mutsu's
native `Test` provider is Rust, so nothing ever called back into script code
through a module frame. The real `Test.rakumod` is Raku, and every assertion
does: `lives-ok { … }` runs the caller's block as `try { $code(); 1 }` from
inside the module. The block was *written* in the test file, so the test file's
operators must apply inside it — but the depth counter was still non-zero, so
they vanished:

```raku
class Bar { has $.bar is rw; method Stringy() { ~self } }
multi sub infix:<+> (Bar $a, Bar $b) { "$a $b" }
my $val;
lives-ok { $val = $foo + $foo };   # $val was 0, not 'software software'
```

`hyper.t` is the same rule reached through a different door:
`eval-lives-ok 'sub infix:<+++>($a, $b) {…}; 10 >>+++<< 14'` — the EVAL'd unit
declares the operator *and* uses it, but the `EVAL` runs inside the module, so
the metaop could not find `+++` at all (`Unsupported reduction operator: +++`).

Neither reproduces from a one-liner. Both reproduce with any module at all:

```raku
# Runner.rakumod:  sub run-block(&code) is export { code() }
multi sub infix:<+> (Bar $a, Bar $b) { … }
run-block({ $foo + $foo })      # mutsu: 0        rakudo: 'software software'
```

## The fix

Operator visibility is now a question about the **compilation unit currently
executing**, and it is tracked directly:

- `Interpreter::current_unit` names that unit. It is saved and restored around
  every compiled-routine call (`enter_compilation_unit`, replacing the four
  `module_call_depth += 1` / `-= 1` sites), around every compiled *closure*
  call — a block carries the unit it was written in, which is what makes the
  callback case work — and around every `EVAL`, since an EVAL unit exists only
  at runtime. `source_file = None` (AOT-compiled) and `source_file =
  Some(program_path)` (compiled on the fly from the running script) normalise to
  the same main-script key.
- `user_declared_infix_ops` grew from a set of names to a map from name to the
  units that declared it. A *declaration* records the declaring unit; a module
  *export* records none, because an exported operator is lexically visible in
  whatever unit imported it (an empty set means "visible everywhere", which is
  exactly the previous behaviour for that case).
- `EVAL` compiles in its caller's lexical scope, so an operator declared in the
  enclosing unit is in scope inside the EVAL. `note_eval_unit_parent` records
  each EVAL unit's parent and the lookup walks that chain.

`$?FILE` looks like it should answer the same question and does not: the env
entry tracks the unit being *loaded*, so inside a module routine invoked at
runtime it still names the main script. A first attempt that read it passed both
roast files while silently removing the original protection — the module's own
arithmetic started resolving to the caller's candidate again. The regression pin
covers both directions for that reason.

Pin: `t/operator-scope-is-lexical-not-dynamic.t` (13 assertions, green under
real `raku` as well as mutsu), with its helper module `t/lib/OperatorScopeRunner.rakumod`.
