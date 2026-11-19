# A `use` no longer slows down every operator and keyword in its unit

Inside a unit or block with a `use`, the compiler puts an
`OpCode::LexImportBuiltinOp` prefix in front of every built-in operator,
because the `use` might import user candidates for `eq`, `+` or `<=`
(ADR-8751 stage 2). If the module exports through a `sub EXPORT` hook,
`True`, `False` and `Nil` also compile to `OpCode::GetShadowableTerm`, because
the hook might shadow them (#9047). Almost no program binds either, but both
had a cost (#18211):

- The JIT did not support `GetShadowableTerm`. Any routine in such a unit that
  named `False` ran entirely in the interpreter. In MCP::Server::Tool::Web's
  tokenizer, that was every character-class helper.
- Each operator prefix cost a dispatch and a world-table read in the
  interpreter, and a helper call in JIT-compiled code.

Two changes fix this:

- `GetShadowableTerm` is now a JIT step op.
- A process-wide flag, `OPERATOR_IMPORT_POSSIBLE`, is raised before the first
  import set that binds an operator is stored, and is never lowered. While it
  is clear, the interpreter skips the prefix at once, and the JIT tests it
  inline and branches past the helper call. Because the flag is never lowered,
  it stays correct across threads and interpreters. Code that went hot before
  an operator import exists still sees the candidates once one does.
