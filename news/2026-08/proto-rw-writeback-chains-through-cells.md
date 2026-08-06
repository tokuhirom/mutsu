# `is rw` writeback chains through a non-trivial proto body

`{*}` redispatch inside a proto whose candidate trips the OTF gate (a class
declaration in the body) used to lose the candidate's rw write on the way
back to the caller:

```raku
proto sub bump($x is rw) { {*} }
multi sub bump($x is rw) { class B2 { }; $x = $x + 1; $x }
my $v = 10;
say bump($v);   # 11 (both)
say $v;         # raku: 11 — mutsu printed 10
```

The proto-dispatch frame carried the proto's entry-time argument *values*,
so the candidate's writeback landed nowhere the proto's own exit writeback
read. With shared-cell rw binding
(`news/2026-08/rw-params-bind-shared-cells.md`) the chain aliases one
container end to end: the proto's rw param binds the caller's cell, the
`{*}` redispatch (`proto_rw_redispatch_args`) forwards the proto param's
current value — now the cell itself — and the candidate's rw param adopts
that same cell, so its write is immediately visible to the proto frame and
the caller. No proto-dispatch changes were needed; the existing redispatch
machinery relays the cell as a value.

The previously commented-out assertion in
`t/proto-dispatch-interpreter-path.t` ("rw writeback chains through the
proto") is enabled and passing.
