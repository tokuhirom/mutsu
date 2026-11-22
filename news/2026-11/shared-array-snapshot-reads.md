# Threads reading a shared array no longer slow each other down

Four `start` workers reading one `:=`-bound array used to run slower than one:
every `@shared[$i]` cloned the array out of its cell and read it under a lock,
and each of those steps wrote to cache lines that every reading core shared
(ADR-8069). Now a cell publishes what its readers need lock-free
(ADR-8069, ADR-18637):

- a bound scalar holding a small `Int`, a `Num`, a `Bool` or a type object is
  published as one atomic word that a reader loads without the cell's mutex;
- a captured array that is read far more often than it is written publishes an
  immutable snapshot, held through `crossbeam-epoch` and validated against a
  version that every in-place write bumps. A captured `@var[$i]` with a simple
  subscript compiles to the new `IndexUpvalue` op, which reads the element out
  of that snapshot without cloning the array or taking a lock.

On a 4-core container the shared array-plus-scalar read loop went from about
770 ms to about 96 ms at four workers, and `bench-threads` from about 650 ms to
about 350 ms. A write-heavy shared array behaves exactly as before: its
snapshot is rebuilt only after `len / 16 + 1` slow reads.
