# A trivial proto's dispatch stops copying the proto

Calling a multi through a `{*}`-only proto (`my proto sub insert(|) {*}`)
looked the proto up with `resolve_proto_function`, which returns an owned
copy of its `FunctionDef`, only to check that its body is trivial and that the
arguments match its signature, and then dropped the copy. It also ran the
argument matcher against a bare `(|)` capture, which accepts every argument
list.

The resolver now reads the registry's shared definition
(`resolve_proto_function_shared`) and treats a bare capture signature as
matching. The resolver itself went from ~20k to ~3.4k instructions per call; a
whole three-argument call through such a proto went from ~91.6k to ~82.5k.
Array::Sorted::Util makes five such calls per Concurrent::PriorityQueue push
(#15786).
