# `Parameter.type` is not nominalized for user-declared subsets

rakudo nominalizes a subset-typed parameter for introspection: `subset Odd of
Int where * % 2; sub f(Odd $x) {}` has `&f.signature.params[0].type` `=:= Int`
and `.constraints` `all(Odd)`. mutsu now does this for the **builtin** subset
`UInt` (see `builtin_subset_base` in `src/value/signature.rs`, added for the
Cro route-compiler bind-check path), but user-declared subsets still report the
subset itself as `.type` and leave `.constraints` empty.

The blocker is plumbing, not semantics: `build_parameter_attrs` is a static
function with no access to the runtime's subset registry
(`registry().subsets`, keyed by name with a `SubsetDef { base, predicate, .. }`),
so it cannot resolve an arbitrary type name to "subset of what". Fixing this
properly means either threading `&Interpreter` into the Parameter-instance
construction path (`sig_param_to_parameter_instance` and friends — several
call sites), or resolving lazily at `.type`/`.constraints` method-call time
where the interpreter is available.

Repro:

```
$ raku -e 'subset Odd of Int where * % 2; sub f(Odd $x) {}; my $p = &f.signature.params[0]; say $p.type =:= Int; say $p.constraints.raku'
True
all(Odd)
$ target/debug/mutsu -e '...same...'
False
all()
```

Impact: any code that introspects `Parameter.type` expecting a nominal type
(the Cro route compiler's `$type =:= Int` dispatch is the known consumer;
user-subset-typed route parameters would hit its `die "Parameter type ... not
allowed"` branch or silently skip the bind check).
