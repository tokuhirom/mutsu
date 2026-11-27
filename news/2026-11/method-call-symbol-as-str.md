# Planned method calls stop turning symbols back into strings

The planned method-call path (`run_method_call_plan` into the slot frame)
called `Symbol::as_str` about nine times per call. It resolved the receiver
class, owner class and method name into strings that the call then never read:

- the deferral frame's names, before the `dispatcher_possible` exit;
- the owner class for a non-role attribute lookup;
- the class name `finish_compiled_method_mut_call` reads only for a `Proxy`
  result;
- the method name for the raw-invocant arrival gate and the implicit-self take.

These now carry the `Symbol` and resolve it only on the branch that uses the
text. Every instance receiver also checked for a BOOT object class, and probed
for an `Iterator` shape, by string match. Both are now symbol compares
(`wk::boot_io`, `wk::iterator_class`). The slot frame reads the receiver's
attribute cell directly, which also drops one `Gc` clone per call.

A no-argument method call in a loop (`$o.n0()` x 20000) goes from 183.2M to
177.3M Ir, about 300 Ir less per call. `Symbol::as_str` drops from 5.6M to
0.64M Ir. This is one slice of #19232. The larger costs on that path are the
`Gc<InstanceAttrs>` refcount traffic and the layered mut-site dispatch.
