# `is rw` writeback through a non-trivial proto body is lost

`{*}` redispatch inside a proto whose body is more than the bare `{*}` does not
chain a candidate's `is rw` parameter write back to the caller's container:

```raku
proto sub bump($x is rw) { {*} }
multi sub bump($x is rw) { $x = $x + 1; $x }
my $v = 10;
say bump($v);   # 11 (mutsu agrees)
say $v;         # raku: 11 — mutsu: 10
```

Raku semantics: the proto's rw parameter aliases the caller's container, the
candidate's rw parameter aliases the proto's, so the candidate's write chains
through to `$v`. In mutsu the proto-dispatch frame carries the proto's
*entry-time argument values*, not the caller's containers, so the candidate's
writeback lands nowhere the proto's own exit writeback reads.

This is a known pre-existing gap — `vm_call_proto_dispatch`'s doc comment
(`src/vm/vm_call_func_ops.rs`) records it, and it fails identically whether the
candidate runs through the VM path or the interpreter fallback
(`call_proto_dispatch`), before and after the ADR-0019 C6d-3 rewire (verified
2026-08-05 by A/B on that rewire: 11/10 both sides). The partial machinery that
exists (`proto_rw_redispatch_args`) rebuilds args from the proto's current
parameter values and names the proto params as arg sources, but only for simple
all-positional signatures, and the final hop (proto param -> caller container at
proto exit) still only works when the *proto body itself* is trivial enough to
take the VM's trivial-proto fork.

`t/proto-dispatch-interpreter-path.t` pins the surrounding behavior and carries
a commented-out assertion for the `$v == 11` half; enable it when fixing this.

Fix direction: the proto-dispatch frame needs to carry the caller's arg sources
(or containers) so the candidate's rw writeback targets them, or the proto's
exit writeback needs to read the candidate's post-`{*}` parameter values.
