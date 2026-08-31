# Native scalar reads retain their type for smartmatch

Mutsu now distinguishes a direct read from a native-typed scalar from its boxed
value when smartmatching against a native type object. For example, `5 ~~ int`
is false while `my int $x = 5; $x ~~ int` is true, matching Rakudo. The same
rule applies to sized integers, native floating-point types, and `str`.

The implementation does not enlarge the eight-byte NaN-boxed `Value`. The
existing smartmatch opcode already identifies a direct variable LHS, so the VM
consults that scalar container's declared type only for a native type-object
matcher. Values produced by literals, declaration expressions, arithmetic, or
topicalization have no such read provenance and remain boxed. Exact native type
identity and native type-smiley behavior are covered by a TAP regression test.
