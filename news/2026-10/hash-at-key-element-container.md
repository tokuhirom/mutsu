# `Hash.AT-KEY` answers the element container

`%h.AT-KEY($k)` now answers the element's own container, as rakudo's `is raw`
`Hash.AT-KEY` does: `my $c := %h.AT-KEY('a'); $c = 5` writes the hash, a
missing key vivifies on the first write, and a `callsame` from a user
`AT-KEY` on a `Hash` subclass hands back an assignable container
(PDF::COS::Tie::Hash coerces its entries in place through it). Assigning to an
`is Hash`/`is Array`/`is BagHash` subclass no longer calls the user's
`AT-KEY`/`AT-POS`: the inherited native `ASSIGN-*` stores directly.

Three container-passing gaps that the PDF chain then ran into are fixed too:

- `my $v := <container>` inside a method mirrors the binding into the frame's
  env, so an `is rw` callee binds that cell instead of a detached copy.
- A method with an `is raw` parameter is no longer cached for the fast
  dispatch path, which bound a copy and dropped the callee's writes from the
  second call on.
- A sigilless `\obj` parameter's readonly marker no longer leaks into a
  callee's same-named `$obj is rw` parameter.

PDF 0.6.17's `t/cos-type-info.t` goes from 3 to 9 of 10 passing tests, and the
`dict sub-types` subtest of `t/cos-coerce.t` passes.
