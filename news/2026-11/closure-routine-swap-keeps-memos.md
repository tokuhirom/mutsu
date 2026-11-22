# A worker calling the main thread's closures keeps its memos

A closure created on one thread and called on another runs with its
creator's routine tables installed for the call (#18135). Each of those swaps
moved the registry write generation to a fresh value. A worker calling such a
closure once per iteration therefore retired every generation-keyed memo
twice per iteration: method and accessor probes, bareword sites and type
memos. AttrX::Mooish's build plan runs a closure created at class composition
on every `Foo.new`, so its `t/090-concurrent` workers paid about 30% more
instructions per round than the same loop on the main thread.

Two changes fix this:

- **A repeated swap reuses its generation.** Installing the same creator
  tables over the same starting generation installs the same content, so it
  gets the same generation again. Leaving a call that wrote nothing restores
  the starting generation.
- **The per-(class, method) probe memo is keyed by the registry's own
  generation.** That generation moves with every registry write, but not with
  a routine-table write or swap.

The AttrX::Mooish round on a worker went from 5.34M to 4.56M instructions;
the same loop on the main thread costs 3.87M.
