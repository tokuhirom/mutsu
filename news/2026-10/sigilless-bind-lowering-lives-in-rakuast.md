# The sigilless bind lowering lives in rakuast

ADR-15334: `ParsedStmt::SigillessDecl` was already typed, but the lowering that turns it into the
bind the compiler consumes (`build_sigilless_bind_stmt`, including the `my \foo = my $x = 3`
split) was parser code. It moved to `rakuast::sigilless_lower`; `synthetic-block-sites` 29 -> 27.
