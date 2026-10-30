# EVAL lowers an assignment to an anonymous `@` / `%` (`=%=`, `=@=`)

`my constant \COLORS is export(:colors) =%= %( ... )` reads in RakuAST as an assignment to `VarDeclaration::Anonymous(sigil => "%")`, but the lowering accepted only the anonymous scalar (`$ = 5`) as an assignment target, so the RakuAST round-trip refused every `Color::Names::*` module. The anonymous array and hash targets are now named as the parser names bare `@` / `%` terms. This unblocks Color::Names::CSS3 and so CSS::Writer, CSS::Module, CSS::Properties, CSS::Stylesheet and CSS::Font::Resources (#15137).
