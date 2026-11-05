# RakuAST constructors: the last repeated named argument wins

A RakuAST constructor read a repeated named argument by taking the first
(`RakuAST::Doc::Block.new(type => 'para', type => 'code', ...)` was `para`),
where a Raku call binds the last, so the idiom `.new(|%defaults, |%overrides)`
silently ignored the overrides. `named_arg`, the one lookup behind the named
constructors, now scans from the end (#16762).
