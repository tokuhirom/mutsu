# An `is rw` auto-accessor hands its Scalar through `is rw` routines and wrappers

`class A { has $.foo is rw }; sub f() is rw { $i.foo }; f() = 4` now writes
`$i.foo`, and so does `A.foo = 7` through
`A.^method_table<foo>.wrap(method ($s: |c) is rw { callwith($inst, |c) })` —
the shape Staticish's singleton wrapper uses (#9706). Both used to die with
`Cannot modify an immutable ...` because the accessor produced its attribute's
*value* on every path except a `:=` bind.

The fix reuses the one container producer the `:=` bind already had
(`MarkAccessorRefContext`):

- an `is rw` routine whose tail (or `return-rw` operand) is an argument-less
  method call asks that call for a container, so a public `is rw` accessor
  answers with its promoted attribute cell;
- a wrapped accessor carries the request to its wrap-chain terminal
  (`DeferralEntry::Accessor { want_container }`), which answers with the same
  cell. The want-ref consumer is now one function (`rw_accessor_container`)
  shared by the VM fast path and the terminal;
- `$obj.acc = v` / `Class.acc = v` on a wrapped accessor runs the wrapper chain
  and assigns through what it returns, instead of storing the attribute
  directly and skipping the wrappers;
- a bare `callsame` term no longer strips the container its candidate
  returned.

A read-only accessor, a plain (non-rw) wrapper and a plain read still yield a
value, and a typed attribute's constraint travels with the container.
