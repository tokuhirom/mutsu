# Metamodel and grammar built-in rule deferral bridges become a frame entry

`callsame`/`nextsame` out of a user HOW method or a grammar's own `method ws` now reaches the native candidate through a
`Native` entry the frame builder pushes, decided by the receiver, instead of an exhaustion probe list. `NATIVE_BASE_MULTI`
and two `NativeBase` variants are gone (ADR-11276 §9.44, part of #12423).
