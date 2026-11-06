# A method of a nested class keeps its writes to the enclosing class body's `my`

A class nested in a `unit class` (or in any class body) now gets the enclosing
class bodies' `my` variables injected on every method call, outermost first, as
its own class body's are. Before, its methods reached them only through a
read-only snapshot, so `@a.push`, `++%h<k>` and element stores to the unit
class's file-scope `my` were lost on return (#16843; RedFactory's per-factory
counter).
