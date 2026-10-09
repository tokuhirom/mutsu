# `nextcallee` inside a `.wrap` of a `proto method`

A wrapper installed on a `proto method` that called `nextcallee` got `Nil` back
(`Unknown function: original` on the first call), because only the MRO and multi
legs of `nextcallee` knew what to hand back. At the end of a dispatcher wrap chain it
now returns a callable that re-dispatches the multi on the invocant and arguments
it is called with, the same terminal leg `callsame` uses. Method::Protected's
`is protected` on a proto method relies on this.
