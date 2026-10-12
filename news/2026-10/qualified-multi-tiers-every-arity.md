# A package-qualified multi call ranks every arity in one set of tiers

A package-qualified multi call (`M::a(1)`) used to gather its candidates in
stages. It took the ones registered under the call's arity, merged in the
optional or slurpy ones only when an exact-arity candidate itself had such a
parameter, then tried the sub-signature captures, and the flexible-arity ones
last. Rakudo ranks a multi's whole candidate set at once, and
the staging gave a different answer:

```raku
module M {
    our proto a(|) {*}
    multi a(Cool $x)     { 'exact-Cool' }
    multi a(Int $x, $y?) { 'opt-Int' }
}
say M::a(1);   # rakudo: opt-Int   mutsu before: exact-Cool
```

Both qualified paths, `resolve_function_with_types` and the proto path's
`resolve_proto_candidate_with_types`, now gather every candidate of the name
in every arity with one helper, `qualified_multi_candidates`. They rank the
set in one set of topological tiers, the way bare-name calls have done since
the previous #12795 slice.
