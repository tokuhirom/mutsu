# A closure keeps calling its block's `multi` after the block exits

A `multi` family declared in a block is now that block's lexical `&name`
binding for the closures nested in it (ADR-12529 phase 2, slice 6), the way a
plain block `sub` has been since slice 4:

```raku
my &c;
{
    multi q(Int) { 'int' }
    multi q(Str) { 'str' }
    &c = -> { q(5) ~ q('x') };
}
say c();   # intstr (was: Unknown function: q)
```

The binding is the family's dispatcher value, holding every candidate visible
at the block, an outer scope's included. Calling a dispatcher value whose
candidates are out of scope now runs the winner inside a real dispatch, so
`callsame`/`nextsame` defer to the next captured candidate and `samewith`
re-dispatches through the family instead of dying ("callsame is not in the
dynamic scope of a dispatcher") or re-entering the one candidate.
