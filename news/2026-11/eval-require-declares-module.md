# `require Module` declares the module's name in an EVAL'd unit

`EVAL 'require M; M.new'` (and `EVAL(Q{...}.AST)`) reported `Undeclared name: M`,
because the EVAL undeclared-name check collected the names of `use`/`need`/`import`
but not of a `require` call, which the frontend lowers to `Call require(Package M)`.
Rakudo declares the name at the `require`. Three more `t/` files now run as
`EVAL(slurp($f).AST)`.
