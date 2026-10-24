# The Rakudo method-table snapshot is parsed per owner, on demand

The introspection snapshot (`rakudo_method_tables.txt`, 105 KB, ADR-14032) was
parsed whole on the first method call of every program: the Cool-only method
gate asked it before looking at the receiver, and the first ask built the
lists of all ~120 owners (about 3M instructions and 1.7k allocations, 9-12% of
`bench-grammar-parse`). The gate now tests the receiver shape before the name
set, and the snapshot is only indexed by owner up front; an owner's lists are
parsed when that owner is first asked about.
