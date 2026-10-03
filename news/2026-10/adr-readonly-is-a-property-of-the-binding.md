# ADR-11142: readonly-ness becomes a property of the binding

mutsu decides whether `$x = 1` is allowed from one process-wide registry keyed by bare name,
which follows the dynamic call stack, while Raku decides it lexically. Five bugs in a row
(#10389, #10400, #11054, #11070, #11142) were the same defect: a writer reached the right binding
but asked the registry, which answered for whatever same-named binding a caller had marked. The
first four were patched by recording readonly state on code objects, methods and routines and
reconciling it on entry; #11142 (a caller's parameter shadowing an outer `my $x := 42`) cannot be
fixed that way, because the shadowed kind is gone from the registry.

[ADR-11142](../../docs/adr/11142-readonly-is-a-property-of-the-binding.md) (Accepted) moves the
kind onto the binding, following ADR-0097's descriptor halves: compile-time kinds (non-rw
parameters, `constant`, sigilless terms) on `BindingDesc`, runtime kinds (`:=`, `for`/topic
aliases) in a slot-parallel `Locals` array, and the kind of a binding reachable from another frame
on its binding cell. The registry, its undo journal, the per-call parameter marking and the
`captured_readonly` reconcile machinery are deleted at the end of a four-slice migration.
Implementation has not started.
