# A warm method call reuses its resolution and binding decision

ADR-15737 D1 asks for a per-call-site plan: a method call should be resolved
and bound once, then replayed. Until now, every call on the plain-method lane
(#8880) still ran the full dispatch tail. That tail re-asked the
class-scoped-accessor and accessor-wins probes, resolved the method, looked
for a wrap chain, and re-ran the fast-path gate over the whole signature.
After that, `call_compiled_method_fast_in_unit` seeded each local slot of the
method's frame by matching the local's *name*. That meant a linear search of
the bound parameters, a re-interned attribute name, an owner-qualified key,
and a freshly formatted `@`/`%` key. All of this was redone on every call.
None of it depended on anything but the receiver's class, the method and the
argument types.

Three pieces change that:

- **The frame plan** (`vm_method_frame_plan`). Each local slot's source is
  decided once per method and stored as a slot-indexed instruction, with every
  attribute key already interned. It is memoized on the `MethodDef` and checked
  against the owner and the compiled code it was built for.
- **The call plan** (`vm_method_call_plan`). A plain-method-lane hit now finds
  the resolved `(owner, MethodDef)` and the gate's verdict for its key, and goes
  straight to the fast binder. It keeps only the tail's per-call effects: the
  raw-invocant channel, the dispatch frame, the attribute commit and the Proxy
  fetch on return. The plan table is keyed on the registry *write* generation,
  so no plan outlives a declaration, an `augment`, a wrap or `.^add_method`. A
  plan is installed only where replaying the decision is provably the same
  decision:
  - a non-`multi` method;
  - an ordinary instance receiver;
  - arguments whose gate reading is fixed by their type.
- **One gate.** The fast-path decision moved into `method_fast_gate`, which
  both `call_compiled_method_in_unit` and the plan's install call. A receiver's
  sigilless-alias check no longer scans its attributes unless some instance has
  ever carried alias metadata.

Callgrind slope (profiling build, 2,000 to 4,000 iterations, second run):

| call | before | after |
| --- | ---: | ---: |
| `$k.get` (no arguments, body `$!v`) | 14,600 | 12,170 |
| `$k.set('x')` (one argument) | 18,600 | 15,820 |

That is -17% and -15% of a method call. It is a step toward the
ADR's call floor, not the end of it: the frame set-up and tear-down around the
body is still about 5,800 instructions per call. On MCP::Server::Tool::Web's
1 MB `html-to-text` fixture (profiling build, local), `html-to-text` went from
30.4 s to 28.1 s and `extract-links` from 22.0 s to 20.8 s.
