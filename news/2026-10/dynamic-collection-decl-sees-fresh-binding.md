# `my @*x = f()` exposes the fresh empty binding to the initializer

A dynamic collection declaration (`my @*x = ...`, `my %*x = ...`) now seeds its fresh
empty container before the initializer runs, as Rakudo does, so a callee of the
initializer sees `[]` instead of the outer (or no) binding, and the declaration no
longer leaks out of its block. Found by the Test::META distribution
(`t/020-internals.t`, `get-meta() uses @*META-CANDIDATES`).
