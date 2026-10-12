# A call's recorded capture keeps the caller's containers

`nqp::usecapture` and `nqp::p6argvmarray` now hand out a sub's or a
closure's arguments the way MoarVM keeps them (#12839). A variable argument
is its `Scalar`, an array or hash element is the element's `Scalar`, and a
literal is the value. Assigning through a captured container writes the
caller's variable or element.

The call entry requests the record before binding, and the binder fills it,
because only the binder knows each argument's caller variable. It goes
through the routine the `|c` capture parameter already used, so the two
cannot drift. The `IndexArgRef` gate now counts a capture-reading callee as
one that binds every argument's container. A method's capture still holds
values: Rakudo's method dispatch hands a `$` parameter's argument over
decontainerized even with `--optimize=0`.

The work also fixed a `|c` bug. A caller variable that shared its name with
one of the callee's parameters (`sub f($y, |c)`, called as `f(1, $y)`)
replaced that parameter, because the capture's cell was installed in the
callee env under the caller's name. The cell is now written back at return
instead, and the method return path learned that writeback too.
