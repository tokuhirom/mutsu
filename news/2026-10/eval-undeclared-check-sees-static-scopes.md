# EVAL's undeclared-variable check sees the caller's static scopes

`EVAL` rejects a variable the snippet uses without a declaration in scope
(`X::Undeclared`). The check consulted only the run-time environment, which
does not yet hold a lexical declared *later* in the caller's source, although
rakudo's static pads already do:

```raku
{
    sub e($s) { EVAL $s }
    say e('!$y.defined');   # True in raku; mutsu said "Variable '$y' is not declared"
    my $y = 4;
}
```

To avoid false errors from that blind spot, the check judged variables in only
a few positions, so `EVAL 'say 1 + $nope'`, `EVAL 'if $nope { }'` and
`EVAL 'my $c = -> $a { $a + $nope }'` all ran when rakudo rejects them.

Now each scope notes every variable its statement list declares when it is
entered. Each chunk that calls `EVAL` records the names of its enclosing static
scopes in `CompiledCode::eval_scope_names`, and the scan accepts those names.
With that in place, every position is judged.

The scan also learned these cases on the way (#10511):
- a unit's file-scope lexicals, imported variables among them, resolve through
  the by-name read chokepoint;
- a statement-form `BEGIN my $c` declares into its scope;
- a placeholder's bare name is left to the placeholder checks.
