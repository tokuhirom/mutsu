# Data::StaticTable now runs under mutsu

Data::StaticTable 0.1.1's five baseline tests now pass under mutsu, including
its row-selection, query, nested hash lookup, and performance coverage.

The interpreter now keeps compound subset declarations qualified inside their
package, resolves those names in typed parameters and private method calls,
handles empty hyper subscripts used to flatten indexed rows, stores through raw
aggregate parameters, runs `LAST` phasers in signature-bearing map blocks, and
decontainerizes nested hash values for `:exists` checks. It also recognizes
exact parameterized type objects during typed attribute checks.
