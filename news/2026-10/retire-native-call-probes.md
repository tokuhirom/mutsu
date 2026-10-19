# The native entries share one receiver guard; the E2 and E4b probes are gone

The VM's native method entry (`try_native_method_raw`) and the interpreter's
(`call_method_with_values`) each checked, in their own words, whether the
receiver needs the interpreter before a built-in row answers:

- a method the instance's class implements in the interpreter;
- a computed exception message;
- a `Cool` name the receiver's ancestry does not declare.

Those checks are now one function, `native_receiver_guard`, that both entries
call. The checks only one entry needs stay with it: the VM's numeric bridge,
and the interpreter's `Supply`/`Stash` receivers.

Slice 5 of the method table campaign (ADR-11276) also retires two
observation-only probes that had compared the arity cascades with the
recognition table: the `native_call_unmodeled` counter (ADR-0019 E2a) and the
E4b probe of `resolve_sequence`'s `Native` candidate. Both were
`MUTSU_VM_STATS` output, with 21 call sites at the method entries. The
cascades they measured have been replaced by the method table.
