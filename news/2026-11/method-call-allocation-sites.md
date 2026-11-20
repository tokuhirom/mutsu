# Fewer allocations per method call on the container and dispatch-frame paths

Three per-call allocations on hot method-call paths are gone:

- The method fast binder's container-parameter classification reads each argument's
  source name through a handle on the call's shared source table instead of copying the
  `String` out.
- In a program that names a deferral builtin (`callsame` and kin), every method call
  records a lazy dispatch frame. That record now shares its winner's list of raw
  positional parameters, memoized per method, instead of rebuilding a `Vec` and a
  `String` per parameter on every call. Almost every such frame is popped unread.
- `Attribute.get_value` / `set_value` read the meta-object's attribute name, owner and
  sigil as borrowed strings.

On AttrX::Mooish's `t/090-concurrent.custom` probe, allocations per round went from
2,495 to 2,402 and instructions from 5.076M to 5.050M (part of #18267).
