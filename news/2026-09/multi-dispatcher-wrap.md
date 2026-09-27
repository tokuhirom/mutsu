# `.wrap` works on a multi method's dispatcher

`A.^method_table<m>.wrap(...)` and `A.^lookup('m').wrap(...)` on a `multi
method` family used to die with `No such method 'wrap' for invocant of type
'Method'` (issue #9705). The object those calls return is the multi's
dispatcher (the proto), and method wrap chains were keyed only by candidate
index, which a dispatcher does not have.

A dispatcher wrap now has its own chain, stored in
`Registry::method_wrap_chains` under a sentinel slot (`DISPATCHER_WRAP_IDX`)
of the class that owns the multi family. Both method-wrap entry sites (the
slow path in `class_dispatch.rs` and the compiled path's
`check_method_wrap_chain`) share the new `enter_method_wrap_chain`. It checks
the dispatcher chain first. The frame it pushes (ADR-0019 E9b-2's single
`MethodDispatchFrame`) ends in a new `DeferralEntry::Redispatch` instead of a
resolved candidate. The innermost `callsame`/`callwith` therefore dispatches
the multi again on the frame's *current* invocant and arguments, so a wrapper
that does `callwith($instance, |c)` selects against the new invocant. The
wrapper also runs once per call, not once per `nextsame` step.

That re-dispatch skips only the dispatcher chain it came from.
`Interpreter::dispatcher_wrap_bypass` records the call-frame and routine-stack
depth where the re-dispatch starts. A recursive call from inside the chosen
candidate is deeper than that, so it is wrapped again, as in Rakudo.
`.unwrap`/`.restore` on the handle work the same way as for candidate wraps.

Staticish's `t/020-test.t` now gets past its `class Multi is Static`
declaration and passes all four multi assertions. Its `MetamodelX::StaticHOW`
wraps every `.^method_table` entry during `compose`. The two remaining
failures in that file come from assigning through a wrapped `is rw` accessor
called on a type object, which is a separate bug.
