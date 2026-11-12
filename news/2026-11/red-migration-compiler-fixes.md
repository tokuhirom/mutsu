# Red's migration compiler: core-name shadowing, mixin multis, Block.signature

Three gaps kept Red's synchronous migration compiler from running:

- A class declared in a package now shadows a core class of the same name
  there, even for the core types the runtime registers as classes
  (`Compiler`, `VM`, `Kernel`, `Lock`, ...). Red's `class Compiler` inside
  `unit module Red::Migration::Data` resolved to the core `Compiler`.
- A `multi` method a mixed-in role supplies joins the value's own multi
  candidates: `$driver but LiteralSQL` adding one `translate` candidate no
  longer hides the driver's other `translate` candidates.
- `RakuAST::Block.signature` answers `Nil` for a block without a declared
  signature, as `RakuAST::Code.signature` does in rakudo.
