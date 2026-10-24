# The RakuAST hierarchy table is only built when a RakuAST name is asked about

`hierarchy::entry` forced the whole measured RakuAST hierarchy table (514 TSV
rows parsed into a `HashMap`) for any type name, so every program paid about
2.9M instructions and 1.3k allocations the first time a type object's MRO was
read. All measured names live under `RakuAST`, so other names now answer `None`
before touching the table. `bench-grammar-parse` returns to its pre-#14847
instruction count.
